namespace Domain

type Variable = {
    Name: string
    Value: string
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
}

type TaskGroupReference = {
    DisplayName: string
    Arguments: Argument list
}

type AgentJobTask =
    | Task of Task
    | TaskGroupReference of TaskGroupReference

type AgentJob = {
    Name: string
    AgentPool: string option
    Tasks: AgentJobTask list
}

type Pipeline = {
    Name: string
    AgentPool: string
    Repository: string
    Branch: string
    AgentJobs: AgentJob list
}