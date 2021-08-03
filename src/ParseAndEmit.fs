module ParseAndEmit

open Domain

/// More or less a span over a section of string
type Token = {
    Value: string
    StartIndex: int
    EndIndex: int
}

/// A standardized parameter name, and all of it's usages.
type Parameter = {
    Name: string
    Usages: Token list
}

type ParsedTask = {
    YAML: string
    Parameters: Parameter list
}

type TaskGroupCompilation = {
    TaskGroup: TaskGroup
    FileName: string
}

// ------------------------------------------------------------------

type CleanVariable = {
    Name: string
    Value: string
}

type CleanParameter = {
    Name: string
    Description: string option
}

type CleanArgument = {
    ParameterName: string
    Value: string
}

type CleanTask = {
    YAML: string
}
and CleanTaskGroup = {
    Parameters: CleanParameter list
    Steps: CleanStep list
}
and CleanTaskGroupReference = {
    FileName: string
    Arguments: CleanArgument list
    TaskGroup: CleanTaskGroup
}
and CleanStep =
    | CleanTask of CleanTask
    | CleanTaskGroupReference of CleanTaskGroupReference

type CleanPipeline = {
    FileName: string
    AgentPool: string
    VariableGroups: string list
    Variables: CleanVariable list
}

// ------------------------------------------------------------------

module Parse =
    open System
    open BCLExtensions.StringExt
    open System.Collections.Generic

    /// Parse parameter references (ex: $(Some.Variable)) from a string
    let parameterReferences (str:string) =
        if str.Length < 2 then
            []

        else
            let mutable inToken = false
            let mutable start = -1

            [for i = 1 to str.Length - 1 do
                let c = str.[i]
                if not inToken && c = '(' && str.[i-1] = '$' then
                    inToken <- true
                    start <- i-1
                elif inToken && c = ')' then
                    // printf "%A to %A of %A" start i (yaml.Length)
                    yield { Value = str.Substring(start, i-start+1)
                            StartIndex = start
                            EndIndex = i }
                    inToken <- false
                    start <- -1]


module Lex =
    open BCLExtensions.StringExt

    let parameterReference (token:Token) =
        let parameterName = token.Value
                            |> replace "^\\$\\(\\s*" ""
                            |> replace "\\)$" ""
                            |> replace "\\." ""

        { Name = parameterName
          Usages = [ token ] }


module Template =
    open System.Text

    let populate (template:string) (parameters: Parameter list) =
        let sb = StringBuilder(template)

        parameters
        |> Seq.collect (fun p -> p.Usages |> Seq.map (fun pr -> (pr, p)))
        |> Seq.sortByDescending (fun (pr,_) -> pr.StartIndex)
        |> Seq.iter (fun (pr,p) ->
            printf "%A" (pr,p,sb.ToString())
            sb.Remove(pr.StartIndex, pr.EndIndex-pr.StartIndex+1)
              .Insert(pr.StartIndex, sprintf "${{ parameters.%s }}" p.Name)
            |> ignore)

        sb.ToString()


module Clean =
    open System
    open System.Linq
    open BCLExtensions.StringExt

    let fileName (str:string) =
        str |> replace "\\s" "-"
            |> replace "[^a-zA-Z0-9-]" ""
            |> (fun str -> str + ".yaml")

    let parameterOrVariableName (str:string) =
        str.Replace(".", "")

    let variable (pv:Domain.PipelineVariable) =
        { Name = pv.Name
          Value = pv.Value |> function Value str -> str | Secret -> "SECRET: DO SOMETHING WITH THIS" |> parameterOrVariableName}

    let parameter (tgp:Domain.TaskGroupParameter) =
        { Name = tgp.Name |> parameterOrVariableName
          Description = if String.IsNullOrWhiteSpace(tgp.Description) then None else Some (tgp.Description.Trim()) }

    let argument (tgp:Domain.Argument) =
        { ParameterName = tgp.ParameterName
                          |> parameterOrVariableName
          Value = Parse.parameterReferences tgp.ArgumentValue
                  |> List.map Lex.parameterReference
                  |> Template.populate tgp.ArgumentValue }

    let task (task:Domain.Task) =
        let undefinedVariableNames = task.YAML
                                         .Split("\r\n")
                                         .TakeWhile(fun str -> str.StartsWith("#"))
                                         .Select(fun str -> str.Split('‘').Last().Split('’').First())
                                         |> Set.ofSeq

        let yaml =
            let i = task.YAML.IndexOf("steps:")
            task.YAML.Substring(i + 6).Trim()

        let yaml =
            Parse.parameterReferences yaml
            |> List.map Lex.parameterReference
            |> List.filter (fun p -> undefinedVariableNames.Contains p.Name)
            |> Template.populate yaml

        { YAML = yaml }

    let rec taskGroup (taskGroup:Domain.TaskGroup) =
        { Parameters = taskGroup.Parameters |> List.map parameter
          Steps = [for t in taskGroup.Tasks do
                    match t with
                    | Domain.TaskListTask.Task t -> CleanTask (task t)
                    | Domain.TaskListTask.TaskGroupReference tgr -> CleanTaskGroupReference (taskGroupReference tgr) ]}

    and taskGroupReference (taskGroupReference:Domain.TaskGroupReference) =
        { FileName = fileName taskGroupReference.TaskGroup.Name
          Arguments = taskGroupReference.Arguments |> List.map argument
          TaskGroup = taskGroup taskGroupReference.TaskGroup }

    let pipeline (pipeline:Domain.Pipeline) =
        { FileName = fileName pipeline.Name
          AgentPool = pipeline.AgentPool
          VariableGroups = pipeline.VariableGroupVariables |> List.map (fun x -> x.Name)
          Variables = pipeline.PipelineVariables |> List.map variable }


module Sprint =
    open BCLExtensions.StringExt

    let parameter (p:CleanParameter) =
        match p.Description with
        | Some description -> sprintf "- name: %s # %s" p.Name description
        | None             -> sprintf "- name: %s" p.Name

    let task (t:CleanTask) =
        t.YAML

    let rec taskGroupReference (tgr:CleanTaskGroupReference) =
        seq {
            yield sprintf "- template: %s" tgr.FileName
            if tgr.Arguments.Length > 0 then
                yield "  parameters:"
                for a in tgr.Arguments do
                    yield sprintf "    %s: %s" a.ParameterName a.Value
        }
        |> join "\r\n"

    let taskGroup (tg:CleanTaskGroup) =
        seq {
            if tg.Parameters.Length > 0 then
                yield "parameters:"
                for p in tg.Parameters do
                    yield parameter p
                yield ""

            yield "steps:"

            let mutable first = true
            for t in tg.Steps do
                if first then
                    first <- false
                else
                    yield ""
                yield (match t with
                       | CleanTask t -> task t
                       | CleanTaskGroupReference tgr -> taskGroupReference tgr)
        }
        |> join "\r\n"

    let pipeline (p:CleanPipeline) =
        seq {
            yield "trigger: none"
            yield "name: TODO"
            yield "pr: TODO"
            yield ""
            yield "pool:"
            yield sprintf "  name: %s" p.AgentPool
            yield ""
            if p.VariableGroups.Length > 0 then
                yield "variables:"
                for vg in p.VariableGroups do
                    yield sprintf "- group: %s" vg
            if p.VariableGroups.Length > 0 && p.Variables.Length > 0 then
                yield ""
            if p.Variables.Length > 0 then
                for p in p.Variables do
                    yield sprintf "- name: %s" p.Name
                    yield sprintf "  value: \"%s\"" p.Value
        }
        |> join "\r\n"


module Process =
    open BCLExtensions.FileExt

    let rec taskGroup (tgr:CleanTaskGroupReference) =
        let str = Sprint.taskGroup tgr.TaskGroup
        writeFile tgr.FileName str

    let pipeline (p:Pipeline) =
        let p = Clean.pipeline p
        let str = Sprint.pipeline p
        writeFile p.FileName str
