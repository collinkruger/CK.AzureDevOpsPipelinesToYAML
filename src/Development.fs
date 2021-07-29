module Development

open System
open System.IO
open Newtonsoft.Json
open Domain
open System.Linq
open System.Collections.Generic
open System.Text.RegularExpressions

module File =
    open System.IO

    let readFile fp =
        File.ReadAllText(fp)

    let writeFile fp str =
        File.WriteAllText(fp, str)

module StringExt =
    open System

    let  join (seperator:string) (lines:string seq) =
        String.Join(seperator, lines)

open File
let json_path = @"C:\HI\blah.json"
let pipeline = File.ReadAllText(json_path) |> JsonConvert.DeserializeObject<Pipeline>
let yaml_path = @"C:\Hi\Doodles.yaml"
let task (Task task) = task
(pipeline.AgentJobs.[0].Tasks.[0] |> task).YAML |> writeFile yaml_path
let yaml = readFile yaml_path

type VariableReference = {
    Value: string
    StartIndex: int
    EndIndex: int
}

type UndefinedVariable = {
    Name: string
    Usages: VariableReference list
}

type Parameter = {
    Name: string
    UndefinedVariable: UndefinedVariable
}

module Blah =
    open StringExt

    /// Read parameters from the first few comment lines of ADO generated YAML
    let readParameters (yaml:string) =
        yaml.Split("\r\n")
            .TakeWhile(fun x -> not (String.IsNullOrWhiteSpace(x)))
            .Select(fun x -> x.Split('‘').Last().Split('’').First())

    /// Read parameter references (ex: $(Some.Variable)) from the main body of the ADO generated YAML
    let readParameterReferences (yaml:string) =
        if yaml.Length < 2 then
            []

        else
            let mutable inToken = false
            let mutable start = -1

            [for i = 1 to yaml.Length - 1 do
                let c = yaml.[i]
                if not inToken && c = '(' && yaml.[i-1] = '$' then
                    inToken <- true
                    start <- i-1
                elif inToken && c = ')' then
                    printf "%A to %A of %A" start i (yaml.Length)
                    yield { Value = yaml.Substring(start, i-start+1)
                            StartIndex = start
                            EndIndex = i }
                    inToken <- false
                    start <- -1]

    /// Remove parameter comment lines (the leading comment lines of the ADO generated YAML), and trim.
    let removeLeadingComments (yaml:string) =

        let lines = yaml.Trim().Split("\r\n")

        let mutable removing = true
        seq { for line in lines do
                if removing then
                    if String.IsNullOrWhiteSpace(line) then
                        removing <- false
                    elif not (line.StartsWith("#")) then
                        removing <- false
                        yield line
                else
                    yield line }
        |> join "\r\n"

    let toUndefinedVariables (variableReferences:VariableReference list) =
        let replace (pattern:string) (replacement:string) (str:string) =
            Regex.Replace(str, pattern, replacement)

        let name token =
            token.Value
            |> replace "^\\$\\(\\s*" ""
            |> replace "\\)$" ""

        let dict = Dictionary<string, UndefinedVariable>()
        for token in variableReferences do
            let name = token |> name
            
            dict.[name] <-
                if dict.ContainsKey(name) then
                    let uv = dict.[name]
                    { uv with Usages = uv.Usages @ [token] }
                else
                    { Name = name; Usages = [token] }

        dict.Values
        |> Seq.sortBy (fun x -> x.Name)
        |> List.ofSeq

    let toParameter (undefinedVariable:UndefinedVariable) =
        { Name = undefinedVariable.Name.Replace(".", "")
          UndefinedVariable = undefinedVariable }

module Template =
    open System.Text
    open StringExt

    let parameters parameters =
        seq {
            yield "parameters:"
            for p in parameters do
                yield sprintf "  - name: %s" p.Name
        }
        |> join "\r\n"

    let body body parameters =
        let sb = StringBuilder(body:string)

        parameters
        |> Seq.collect (fun p -> p.UndefinedVariable.Usages |> Seq.map (fun v -> (v, p)))
        |> Seq.sortByDescending (fun (v,_) -> v.StartIndex)
        |> Seq.iter (fun (v,p) -> 
            sb.Remove(v.StartIndex, v.EndIndex-v.StartIndex+1)
              .Insert(v.StartIndex, sprintf "${{ %s }}" p.Name)
            |> ignore)

        sb.ToString()



    // open Blah

    // yaml_path |> readFile
    //           |> readParameterReferences
    //           |> toUndefinedVariables
    //           |> List.map toParameter
    //           |> parameters

    // yaml_path |> readFile
    //           |> readParameterReferences
    //           |> toUndefinedVariables
    //           |> List.map toParameter
    //           |> body (yaml)
