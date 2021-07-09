module Scrape

open System
open canopy
open canopy.classic
open OpenQA.Selenium
open Domain


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

type PipelineType =
    | Build
    | Release


// Functions ------------------------------------------------------------------------

let getRightPane () =
    element ".right-section"
    |> RightPane

let getAgentJobHeaders () =
    elements ".phase-item .two-panel-overview"
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

let isTaskGroupDisabled (taskHeader:TaskHeader) =
    taskHeader.DOMElement
    |> matches ".is-disabled"

let isTaskGroupEnabled taskHeader = not <| isTaskGroupDisabled taskHeader

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

let getPipelineVariablesVariables (pipelineType:PipelineType) (rightPane:RightPane) =
    rightPane.DOMElement
    |> descendent "[aria-label=\"Pipeline variables table\"] > div:not(:first-child)"
    |> descendents "[role=row]"
    |> List.map (fun el ->
        let texts = el |> descendents "pre" |> List.map read
        
        let settableAtQueueTime = el |> descendent (match pipelineType with Build -> "button[aria-label=\"Settable at queue time\"]"
                                                                          | Release -> "button[aria-label=\"Settable at release time\"]")
                                     |> matches ".is-checked"

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

let getTaskDisplayName (rightPane:RightPane) =
    rightPane.DOMElement
    |> descendent ".task-name input"
    |> read

let getTaskYaml () =
    findByText "View YAML" |> click
    waitForElement "body > .ui-dialog"
    let str = element "body > .ui-dialog [aria-label=\"Content to copy\"]" |> read
    element "body > .ui-dialog [aria-label=\"Close\"]" |> click
    str

let getTaskGroupReference (rightPane:RightPane) =
    let displayName = getTaskDisplayName rightPane

    let arguments = rightPane.DOMElement
                    |> descendents ".task-details-body .ms-List-cell"
                    |> List.map (fun el -> { ParameterName = el |> descendent "label" |> read
                                             ArgumentValue = el |> descendent "textarea" |> read })
    
    { DisplayName = displayName; Arguments = arguments }


let getTaskGroupMeta (rightPane: RightPane) =
    let name        = rightPane.DOMElement |> descendentByText "Name" |> ancestor ".input-field-component" |> descendent "input" |> read
    let description = rightPane.DOMElement |> descendentByText "Description" |> ancestor ".input-field-component" |> descendent "textarea" |> read
    let category    = rightPane.DOMElement |> descendentByText "Category" |> ancestor ".dtc-task-group-dialog-category" |> descendent "input" |> read

    let parameters = rightPane.DOMElement
                     |> descendents ".dtc-task-group-parameter-row"
                     |> List.map (fun el ->
                          { Name         = el |> descendent ".dtc-task-group-parameter-name" |> read
                            DefaultValue = el |> descendent ".task-group-parameter-value-input input" |> read
                            Description  = el |> descendent ".task-group-parameter-description-input input" |> read }
                     )

    {| Name = name
       Description = description
       Category = category
       Parameters = parameters |}


// WIP ------------------------------------------------------------------------------

let getAllPipelineData_WIP_WIP_WIP (pipelineType:PipelineType) =
    // TODO: Handle Multiple Agent Jobs
    // TODO: Handle Task Groups

    // Get Variables ------------------------------------------------------
    clicksVariablesTab()

    clickPipelineVariables()
    let pipelineVariables = getRightPane() |> getPipelineVariablesVariables pipelineType

    clickVariableGroups()
    let rightPane = getRightPane()
    expandVariableGroups rightPane
    let variableGroupVariables = rightPane |> getVariableGroupsAndVariables


    // Get Tasks ----------------------------------------------------------
    clickTasksTab()
    // clickPipelineHeader()

    let rightPane = getRightPane()
    
    let pipelineMetaData = rightPane |> getPipelineMetaData
    
    let agentJobHeader = getAgentJobHeaders() |> List.head // TODO: CHANGE THIS. THIS DOES NOT HOLD.
    
    let agentJobTasks = getTaskHeaders()
                        |> List.filter (fun x -> (isTaskGroup x |> not) || (isTaskGroupEnabled x))
                        |> List.map (fun header ->
                              click header.DOMElement
                              waitForElement ".rightPane .task-details-body"
                              let rightPane = getRightPane()
                              if header |> isTaskGroup then
                                  // TODO: recurse into task group
                                  TaskGroupReference (getTaskGroupReference rightPane)
                              else
                                  Task { DisplayName = rightPane |> getTaskDisplayName
                                         YAML = getTaskYaml() }
                        )

    {| PipelineVariables = pipelineVariables
       VariableGroupVariables = variableGroupVariables
       PipelineMetaData = pipelineMetaData
       AgentJobHeader = agentJobHeader
       AgentJobTasks = agentJobTasks |}

// getAllPipelineData_WIP_WIP_WIP PipelineType.Build 

// // Open Task Group
// // TODO

// getRightPane().DOMElement |> descendent ".heading-row > .task-type-info i" |> click

// element ".callout-taskgroup-link a" |> click

// switchToTabIndex 1

// let rightPane = getRightPane()
// let taskGroupMeta = getTaskGroupMeta rightPane



// // tasks
// getTaskHeaders ()
// |> List.filter (isTaskGroup >> not)
// |> List.map (fun header ->
//     click header.DOMElement
//     getTaskYaml ())

// // get task group id

// (currentUrl () |> System.Uri).LocalPath.Split('/') |> Array.last