open System
open System.Linq
open System.IO
open canopy.classic
open canopy.configuration
open Scrape

let getChromeDriverDir () =
    let userDir = Environment.GetFolderPath(Environment.SpecialFolder.UserProfile)
    let nugetDir = userDir + "\\.nuget"

    if not (Directory.Exists(nugetDir)) then
        failwith $"A nuget directory was expected to be found at {nugetDir}, but was not."
       
    let packageDir = Directory.GetDirectories(nugetDir, "*", SearchOption.AllDirectories)
                              .SingleOrDefault(fun x -> x.Contains("chromedriver")
                                                     && x.Contains("91.0.4472.10100")
                                                     && x.EndsWith("win32"))
    
    if isNull packageDir then
        failwith $"A chrome driver package directory was expected to be found somewhere under {nugetDir}, but was not"

    packageDir

[<EntryPoint>]
let main argv =
    chromeDir <- getChromeDriverDir()

    start chrome

    printfn "Navigate to your Azure DevOps pipeline, then press any key."
    Console.ReadKey() |> ignore

    getAllPipelineData_WIP_WIP_WIP() |> printfn "%A"

    // TODO: Flatten variable references
    // TODO: Save data in a meaningful way

    quit()

    0