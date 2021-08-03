module Scrape

open System
open canopy
open canopy.classic
open canopy.Selectors
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

type PipelineType =
    | Build
    | Release


// Functions ------------------------------------------------------------------------

let getRightPane () =
    element ".right-section"
    |> RightPane


module Tabs =

    let clickTasksTab () =
        findByText "Tasks"
        |> click
        waitForElement ".left-section .phase-list-controller-view"

    let clickVariablesTab () =
        findByText "Variables"
        |> click
        waitFor (fun () -> findByTextOpt "Pipeline variables" |> Option.isSome)

    let clickPipelineHeader () =
        findByText "Pipeline"
        |> click
        waitFor (fun () -> findByTextOpt "Name" |> Option.isSome)


module Tasks =

    module Pipeline =
        
        let getMetaData (rightPane:RightPane) =
            { Name      = rightPane.DOMElement |> descendent "input.ms-TextField-field" |> read
              AgentPool = rightPane.DOMElement |> descendent "input.ms-ComboBox-Input" |> read }


    module AgentJobs =

        let getHeaders () =
            elements ".phase-item .two-panel-overview"
            |> List.map AgentJobHeader

        let focusHeader (agentJobHeader:AgentJobHeader) =
            click agentJobHeader.DOMElement
            waitForElement ".rightPane .phase-name"

        let getMetaData (rightPane:RightPane) =
            { DisplayName = rightPane.DOMElement |> descendent "input.ms-TextField-field" |> read
              AgentPool   = rightPane.DOMElement |> descendent "input.ms-ComboBox-Input"  |> read }


        module Tasks =

            let private headerclass = ".task-item-overview"

            let getHeaders () =
                elements headerclass
                |> List.map TaskHeader

            let getHeadersForAgentJob (agentJobHeader: AgentJobHeader) =
                agentJobHeader.DOMElement
                |> ancestor ".phase-item-overview"
                |> descendents headerclass
                |> List.map TaskHeader

            let focusHeader header =
                click header.DOMElement
                waitForElement ".rightPane .task-details-header .task-type-info-icon"
                waitForElement ".rightPane .task-details-body"

            let isTaskGroup (taskHeader:TaskHeader) =
                taskHeader.DOMElement
                |> descendent "img"
                |> attr "src"
                |> (fun str -> str.EndsWith("icon-meta-task.png"))

            let isNotTaskGroup = isTaskGroup >> not

            let isTaskGroupDisabled (taskHeader:TaskHeader) =
                taskHeader.DOMElement
                |> matches ".is-disabled"

            let isTaskGroupEnabled taskHeader = not <| isTaskGroupDisabled taskHeader

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


        module TaskGroup =

            let getPartialTaskGroupReference (rightPane:RightPane) =
                let displayName = Tasks.getTaskDisplayName rightPane

                let arguments = rightPane.DOMElement
                                |> descendents ".task-details-body .ms-List-cell"
                                |> List.map (fun el -> { ParameterName = el |> descendent "label" |> read
                                                         ArgumentValue = el |> descendent "textarea" |> read })
                
                {| DisplayName = displayName; Arguments = arguments |}

            let parseCategory str : TaskGroupCategory =
                match str with
                | "Build"   -> TaskGroupCategory.Build
                | "Deploy"  -> Deploy
                | "Package" -> Package
                | "Utility" -> Utility
                | "Test"    -> Test
                | _         -> Unknown str

            let getTaskGroupMeta (rightPane: RightPane) =
                let name        = rightPane.DOMElement |> descendentByText "Name" |> ancestor ".input-field-component" |> descendent "input" |> read
                let description = rightPane.DOMElement |> descendentByText "Description" |> ancestor ".input-field-component" |> descendent "textarea" |> read
                let category    = rightPane.DOMElement |> descendentByText "Category" |> ancestor ".dtc-task-group-dialog-category" |> descendent "input" |> read |> parseCategory

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

            let getTaskGroupUrl (rightPane: RightPane) =
                rightPane.DOMElement |> descendent ".heading-row > .task-type-info i" |> click
                element ".callout-taskgroup-link a" |> attr "href"

            let navigateToTaskGroup (rightPane: RightPane) =
                let currentId = Tabs.currentId()
                let currentIds = Tabs.ids()

                rightPane.DOMElement |> descendent ".heading-row > .task-type-info i" |> click
                waitForElement ".callout-taskgroup-link a"
                element ".callout-taskgroup-link a" |> click
                waitFor (fun () -> currentIds.Length <> Tabs.ids().Length)
                let currentId' = Set(Tabs.ids()) - Set(currentIds) |> Seq.head

                Tabs.switchToId currentId'

                {| From = currentId
                   To = currentId' |}


module Variables =

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
                let groupName = (el |> descendent ".dtc-variable-group-header-info" |> read).Split(' ').[0]

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


// Procedures -----------------------------------------------------------------------

open Variables
open Tasks
open Tasks.AgentJobs

/// Given a root level task group reference (AKA a task group directly under a pipeline)
/// Recursively dig in
let rec recTaskGroup (rightPane:RightPane) : TaskGroupReference =
    // On Page 1, Get Partial Task Group Reference
    let partialRef = TaskGroup.getPartialTaskGroupReference rightPane

    // Navgiate To Task Group, AKA Go To Page 2
    let tabIds = TaskGroup.navigateToTaskGroup rightPane

    // On Page 2, Get Task Group Meta Data
    let taskGroupMeta = getRightPane() |> TaskGroup.getTaskGroupMeta

    // On Page 2, Iterate Over Tasks/Task Groups
    //            And Recurse Into Task Groups
    let tasks = Tasks.getHeaders()
                |> List.filter (fun x -> (Tasks.isNotTaskGroup x) || (Tasks.isTaskGroupEnabled x))
                |> List.map (fun header ->
                    Tasks.focusHeader header
                    let rightPane = getRightPane()
                    if header |> Tasks.isTaskGroup then
                        recTaskGroup rightPane
                        |> TaskGroupReference
                    else
                        Task { DisplayName = rightPane |> Tasks.getTaskDisplayName
                               YAML = Tasks.getTaskYaml() }
                )

    Tabs.closeCurrent ()
    Tabs.switchToId tabIds.From

    { DisplayName = partialRef.DisplayName
      Arguments   = partialRef.Arguments
      TaskGroup   = { Name        = taskGroupMeta.Name
                      Description = taskGroupMeta.Description
                      Category    = taskGroupMeta.Category
                      Parameters  = taskGroupMeta.Parameters
                      Tasks       = tasks } }


let getPipeline (pipelineType:PipelineType) : Pipeline =
    // TODO: Handle Multiple Agent Jobs
    // TODO: Handle Task Groups

    let getPipelineMeta () =
        Tabs.clickPipelineHeader()
        getRightPane()
        |> Pipeline.getMetaData

    let getTaskListTasks (agentJobHeader:AgentJobHeader) = // TODO: Change this to look for tasks under an agent job
        Tasks.getHeadersForAgentJob agentJobHeader
        |> List.filter (fun x -> (Tasks.isNotTaskGroup x) || (Tasks.isTaskGroupEnabled x))
        |> List.map (fun header ->
            Tasks.focusHeader header
            let rightPane = getRightPane()
            if header |> Tasks.isTaskGroup then
                recTaskGroup rightPane
                |> TaskGroupReference
            else
                Task { DisplayName = rightPane |> Tasks.getTaskDisplayName
                       YAML = Tasks.getTaskYaml() }
        )

    let getAgentJobs () = // TODO: Support more than one Agent Job
        let header = AgentJobs.getHeaders()
                     |> List.head
        AgentJobs.focusHeader header
        let meta = AgentJobs.getMetaData (getRightPane())
        let tasks = getTaskListTasks header

        [{ DisplayName = meta.DisplayName
           AgentPool = meta.AgentPool
           Tasks = tasks }]


    let getPipelineVariables () =
        Tabs.clickVariablesTab()
        Variables.clickPipelineVariables()
        getRightPane()
        |> getPipelineVariablesVariables pipelineType

    let getVariableGroupVariables () =
        Tabs.clickVariablesTab()
        Variables.clickVariableGroups()
        let rightPane = getRightPane()
        expandVariableGroups rightPane
        rightPane |> Variables.getVariableGroupsAndVariables


    let pipelineMeta = getPipelineMeta()
    let jobs = getAgentJobs ()
    let pipelineVariables = getPipelineVariables()
    let variableGroupVariables = getVariableGroupVariables()

    { Name = pipelineMeta.Name
      AgentPool = pipelineMeta.AgentPool
      PipelineVariables = pipelineVariables
      VariableGroupVariables = variableGroupVariables
      AgentJobs = jobs }

// PipelineType.Build |> getPipeline |> Newtonsoft.Json.JsonConvert.SerializeObject |> (fun str -> System.IO.File.WriteAllText("C:\\HI\\blah.json", str))