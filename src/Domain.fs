namespace Domain

type VariableValue =
    | Value of string
    | Secret

type PipelineVariable = {
    Name: string
    Value: VariableValue
    SettableAtQueueTime: bool
}

type Variable = {
    Name: string
    Value: VariableValue
}

type VariableGroup = {
    Name: string
    Variables: Variable list
}

type Argument = {
    ParameterName: string
    ArgumentValue: string
}

type Task = {
    DisplayName: string
    YAML: string
}

type TaskGroupCategory =
    | Build
    | Deploy
    | Package
    | Utility
    | Test
    | Unknown of string

type TaskGroupParameter = {
    Name: string
    DefaultValue: string
    Description: string

}

type TaskGroup = {
    Name: string
    Description: string
    Category: TaskGroupCategory
    Parameters: TaskGroupParameter list
    Tasks: TaskListTask list
}
and TaskGroupReference = {
    DisplayName: string
    Arguments: Argument list
    TaskGroup: TaskGroup
}
and TaskListTask =
    | Task of Task
    | TaskGroupReference of TaskGroupReference

type AgentJob = {
    DisplayName: string
    AgentPool: string
    Tasks: TaskListTask list
}

type Pipeline = {
    Name: string
    AgentPool: string
    // Repository: string // TODO
    // Branch: string     // TODO
    PipelineVariables: PipelineVariable list
    VariableGroupVariables: VariableGroup list
    AgentJobs: AgentJob list
}