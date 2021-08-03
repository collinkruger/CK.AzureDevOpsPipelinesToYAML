module Development

open Newtonsoft.Json
open BCLExtensions.FileExt
open Domain
open ParseAndEmit

do
    (readFile @"C:\HI\blah.json"
    |> JsonConvert.DeserializeObject<Pipeline>)
        // .AgentJobs
        // .[0]
        // .Tasks
        // |> List.pick (function | TaskGroupReference tgr -> Some tgr.TaskGroup | _ -> None)
        // |> Clean.taskGroup
        // |> Sprint.taskGroup
        // |> writeFile @"C:\HI\Blahahahah.yaml"
        |> Process.pipeline
