module Scrape

open System
open canopy
open canopy.classic
open OpenQA.Selenium


// Types ----------------------------------------------------------------------------

type RightPane = {
    DOMElement: IWebElement
}
let RightPane el = { DOMElement = el }

type AgentJobHeader = {
    DOMElement: IWebElement
}
let AgentJobHeader el = { DOMElement = el }

type TaskHeader = {
    DOMElement: IWebElement
}
let TaskHeader el = { DOMElement = el }

type PipelineMetaData = {
    Name: string
    AgentPool: string
}

type AgentJobMetaData = {
    DisplayName: string
    AgentPool: string
}

type VariableValue =
    | Value of string
    | Secret

type PipelineVariable = {
    Name: string
    Value: VariableValue
    SettableAtQueueTime: bool
}

type VariableGroupVariable = {
    Name: string
    Value: VariableValue
}

type VariableGroup = {
    Name: string
    Variables: VariableGroupVariable list
}

type TaskGroupArgument = {
    ParameterName: string
    ArgumentValue: string
}


// Functions ------------------------------------------------------------------------

let getRightPane () =
    element ".right-section"
    |> RightPane

let getAgentJobHeaders () =
    elements ".phase-item"
    |> List.map AgentJobHeader

let getTaskHeaders () =
    elements ".task-item-overview"
    |> List.map TaskHeader

let getPipelineMetaData (rightPane:RightPane) =
    { Name      = rightPane.DOMElement |> descendent "input.ms-TextField-field" |> read
      AgentPool = rightPane.DOMElement |> descendent "input.ms-ComboBox-Input" |> read }

let getAgentJobMetaData (rightPane:RightPane) =
    { DisplayName = rightPane.DOMElement |> descendent "input.ms-TextField-field" |> read
      AgentPool   = rightPane.DOMElement |> descendent "input.ms-ComboBox-Input"  |> read }

let isTaskGroup (taskHeader:TaskHeader) =
    taskHeader.DOMElement
    |> descendent "img"
    |> attr "src"
    |> (fun str -> str.EndsWith("icon-meta-task.png"))

let clickTasksTab () =
    findByText "Tasks"
    |> click
    waitForElement ".left-section .phase-list-controller-view"

let clickPipelineHeader () =
    findByText "Pipeline"
    |> click
    waitFor (fun () -> findByTextOpt "Name" |> Option.isSome)

let clicksVariablesTab () =
    findByText "Variables"
    |> click
    waitFor (fun () -> findByTextOpt "Pipeline variables" |> Option.isSome)

let clickPipelineVariables () =
    findByText "Pipeline variables"
    |> click

let clickVariableGroups () =
    findByText "Variable groups"
    |> click

let getPipelineVariablesVariables (rightPane:RightPane) =
    rightPane.DOMElement
    |> descendent "[aria-label=\"Pipeline variables table\"] > div:not(:first-child)"
    |> descendents "[role=row]"
    |> List.map (fun el ->
        let texts = el |> descendents ".ms-List-page .flat-view-text-preserve" |> List.map read
        let settableAtQueueTime = el |> descendent "button[aria-label=\"Settable at queue time\"]" |> matches ".is-checked"

        { Name = texts.[0]
          Value = if texts.Length = 2 then Value texts.[1] else Secret
          SettableAtQueueTime = settableAtQueueTime }
    )

let expandVariableGroups (rightPane:RightPane) =
    rightPane.DOMElement
    |> descendents "[aria-label=\"Expand variable group\"]"
    |> List.iter click

let getVariableGroupsAndVariables (rightPane:RightPane) =
    rightPane.DOMElement
    |> descendents ".ms-GroupedList-group"
    |> List.map (fun el ->
        let groupName = el |> descendent ".dtc-variable-group-header-info" |> read

        let variables = el
                        |> descendents ".ms-DetailsRow-fields"
                        |> List.map (fun el ->
                            let texts = el |> descendents "[role=gridcell]" |> List.map read
                            { Name = texts.[0]
                              Value = if texts.Length = 2 then Value texts.[1] else Secret }
                        )

        { Name = groupName
          Variables = variables }
    )

let getTaskYaml () =
    findByText "View YAML" |> click
    waitForElement "body > .ui-dialog"
    let str = element "body > .ui-dialog [aria-label=\"Content to copy\"]" |> read
    element "body > .ui-dialog [aria-label=\"Close\"]" |> click
    str


// WIP ------------------------------------------------------------------------------

let getAllPipelineData_WIP_WIP_WIP () =
    // TODO: Handle Multiple Agent Jobs
    // TODO: Handle Task Groups

    // Get Variables ------------------------------------------------------
    clicksVariablesTab()

    clickPipelineVariables()
    let pipelineVariables = getRightPane() |> getPipelineVariablesVariables

    clickVariableGroups()
    let rightPane = getRightPane()
    expandVariableGroups rightPane
    let variableGroupVariables = rightPane |> getVariableGroupsAndVariables


    // Get Tasks ----------------------------------------------------------
    clickTasksTab()
    clickPipelineHeader()

    let rightPane = getRightPane()
    
    let pipelineMetaData = rightPane |> getPipelineMetaData
    
    let agentJobHeader = getAgentJobHeaders() |> List.head // TODO: CHANGE THIS. THIS DOES NOT HOLD.
    
    let agentJobTasksYAML = getTaskHeaders ()
                            |> List.filter (isTaskGroup >> not)
                            |> List.map (fun header ->
                                click header.DOMElement
                                getTaskYaml ())

    {| PipelineVariables = pipelineVariables
       VariableGroupVariables = variableGroupVariables
       PipelineMetaData = pipelineMetaData
       AgentJobHeader = agentJobHeader
       AgentJobTasksYAML = agentJobTasksYAML |}


// Focus A Task Group Reference
(getTaskHeaders () |> List.filter isTaskGroup |> List.head).DOMElement |> click
waitForElement ".rightPane .task-details-body"

// Read Task Group Reference Arguments
getRightPane().DOMElement
|> descendents ".task-details-body .ms-List-cell"
|> List.map (fun el -> { ParameterName = el |> descendent "label" |> read
                         ArgumentValue = el |> descendent "textarea" |> read })


// Open Task Group
// TODO

getRightPane().DOMElement |> descendent ".heading-row > .task-type-info i" |> click

element ".callout-taskgroup-link a" |> click

switchToTab 2

// name
getRightPane().DOMElement |> descendentByText "Name" |> ancestor ".input-field-component" |> descendent "input" |> read

// tasks
getTaskHeaders ()
|> List.filter (isTaskGroup >> not)
|> List.map (fun header ->
    click header.DOMElement
    getTaskYaml ())

// get task group id

(currentUrl () |> System.Uri).LocalPath.Split('/') |> Array.last