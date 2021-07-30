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

    let parameterName (str:string) =
        str.Replace(".", "")

    let parameter (tgp:Domain.TaskGroupParameter) =
        { Name = tgp.Name |> parameterName
          Description = if String.IsNullOrWhiteSpace(tgp.Description) then None else Some (tgp.Description.Trim()) }

    let argument (tgp:Domain.Argument) =
        { ParameterName = tgp.ParameterName
                          |> parameterName
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


module Process =
    open BCLExtensions.FileExt

    let process (agentJob:Domain.AgentJob) =

    let rec taskGroup (tgr:CleanTaskGroupReference) =
        let str = Sprint.taskGroup tgr.TaskGroup
        writeFile tgr.FileName str



open BCLExtensions.FileExt
open Newtonsoft.Json

(readFile @"C:\HI\blah.json"
|> JsonConvert.DeserializeObject<Pipeline>)
    .AgentJobs
    .[0]
    .Tasks
    |> List.pick (function | Domain.TaskListTask.TaskGroupReference tgr -> Some tgr.TaskGroup | _ -> None)
    |> Clean.taskGroup
    |> Sprint.taskGroup
    |> writeFile @"C:\HI\Blahahahah.yaml"


// // pipeline.AgentJobs.[0] |> compileAgentJob
// //                        |> writeFile @"C:\HI\Doodles2.yaml"

// let taskGroup = pipeline.AgentJobs.[0].Tasks
//                 |> List.pick (function | TaskGroupReference tgr -> Some tgr.TaskGroup | _ -> None)




// module Parse =
//     open System
//     open BCLExtensions.StringExt
//     open System.Collections.Generic

//     /// Parse parameter references (ex: $(Some.Variable)) from the main body of the ADO generated YAML
//     let parseParameters (yaml:string) =
//         if yaml.Length < 2 then
//             []

//         else
//             let mutable inToken = false
//             let mutable start = -1

//             [for i = 1 to yaml.Length - 1 do
//                 let c = yaml.[i]
//                 if not inToken && c = '(' && yaml.[i-1] = '$' then
//                     inToken <- true
//                     start <- i-1
//                 elif inToken && c = ')' then
//                     // printf "%A to %A of %A" start i (yaml.Length)
//                     yield { Value = yaml.Substring(start, i-start+1)
//                             StartIndex = start
//                             EndIndex = i }
//                     inToken <- false
//                     start <- -1]

//     let standardizeParameters parameterReferences =
//         let name paramRef =
//             paramRef.Value
//             |> replace "^\\$\\(\\s*" ""
//             |> replace "\\)$" ""
//             |> replace "\\." ""

//         let dict = Dictionary<string, Parameter>()
//         for paramRef in parameterReferences do
//             let name = paramRef |> name
            
//             dict.[name] <-
//                 if dict.ContainsKey(name) then
//                     let uv = dict.[name]
//                     { uv with Usages = uv.Usages @ [paramRef] }
//                 else
//                     { Name = name; Usages = [paramRef] }

//         dict.Values
//         |> Seq.sortBy (fun x -> x.Name)
//         |> List.ofSeq

//     let parseTask (yaml:string) =
//         let cleanYAML =
//             let i = yaml.IndexOf("steps:")
//             yaml.Substring(i + 6).Trim()

//         printf "%A" cleanYAML

//         { YAML = cleanYAML
//           Parameters = cleanYAML |> parseParameters |> standardizeParameters }


// module Emit =
//     open System.Text
//     open BCLExtensions.StringExt

//     let emitParameters parameters =
//         seq {
//             yield "parameters:"
//             for p in parameters do
//                 yield sprintf "  - name: %s" p.ParameterName
//         }
//         |> join "\r\n"

//     let emitTaskBody parsedTask =
//         let sb = StringBuilder(parsedTask.YAML)

//         parsedTask.Parameters
//         |> Seq.collect (fun p -> p.Usages |> Seq.map (fun pr -> (pr, p)))
//         |> Seq.sortByDescending (fun (pr,_) -> pr.StartIndex)
//         |> Seq.iter (fun (pr,p) ->
//             printf "%A" (pr,p,sb.ToString())
//             sb.Remove(pr.StartIndex, pr.EndIndex-pr.StartIndex+1)
//               .Insert(pr.StartIndex, sprintf "${{ parameters.%s }}" p.Name)
//             |> ignore)

//         sb.ToString()


// module Compile =
//     open BCLExtensions.StringExt
//     open Domain
//     open Parse
//     open Emit

//     let compileAgentJob (agentJob:AgentJob) =
//         let tasks = agentJob.Tasks
//                     |> List.choose (function | Task task -> Some task | _ -> None) // TODO: Support TaskGroups

//         let parameters = tasks |> List.collect (fun t -> t.YAML |> parseParameterReferences)
//                                |> standardizeParameters
//                                |> List.distinctBy (fun x -> x.Name)
//                                |> List.sortBy (fun x -> x.Name)

//         let tasks = tasks |> List.map (fun t -> t.YAML |> parseTask)

//         seq {
//             yield emitParameters parameters
//             yield ""
//             yield "steps:"

//             let mutable first = true
//             for t in tasks do
//                 if first then
//                     first <- false
//                 else
//                     yield ""
//                 yield emitTaskBody t
//         }
//         |> join "\r\n"






// open System
// open BCLExtensions.FileExt
// open BCLExtensions.StringExt
// open Newtonsoft.Json
// open Domain
// open Compile

// let pipeline = readFile @"C:\HI\blah.json"
//                |> JsonConvert.DeserializeObject<Pipeline>

// // pipeline.AgentJobs.[0] |> compileAgentJob
// //                        |> writeFile @"C:\HI\Doodles2.yaml"

// let taskGroup = pipeline.AgentJobs.[0].Tasks
//                 |> List.pick (function | TaskGroupReference tgr -> Some tgr.TaskGroup | _ -> None)


// let parameter (tgp:Domain.TaskGroupParameter) =
//     seq {
//         if tgp.Description = "" then
//             sprintf "- name: %s" tgp.Name
//         else
//             sprintf "- name: %s # %s" tgp.Name tgp.Description
//     }
//     |> join "\r\n"

// let blahTaskGroup (tg:Domain.TaskGroup) =
//     seq {
//         if tg.Description <> "" then
//             for line in tg.Description.Split("\r\n") do
//                 yield sprintf "# %s" line
//             yield ""
        
//         if tg.Parameters |> List.isEmpty |> not then
//             yield "parameters:"
//             for p in tg.Parameters do
//                 yield parameter p

//     }
//     |> join "\r\n"