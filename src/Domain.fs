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
   YAML: string
}

type TaskGroupCategory =
    | Build
    | Deploy
    | Package
    | Utility
    | Test


type TaskGroup = {
    Name: string
    Category: TaskGroupCategory
    Variables: Variable list
}

type TaskGroupReference = {
    Name: string
    Category: TaskGroupCategory
    Arguments: Argument list
}

type AgentJobTask =
    | Task
    | TaskGroup

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