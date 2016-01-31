module Aggregator

type MatchResult =
    | Win
    | Draw
    | Lose

type Location =
    | Home
    | Away

type FilterType = 
    | ByDate
    | HomeOnly
    | AwayOnly


type Team(teamName: string, allNames: string []) = 
    member this.TeamName = teamName
    member internal this.GetAllNames = fun _ -> allNames

type ResultForTeam(teamName: string, goalsScored: int, goalsConceded: int, date: System.DateTime, location: Location) =
    member this.TeamName = teamName
    member this.MatchResult = if(goalsScored > goalsConceded) then MatchResult.Win
                                else if(goalsScored = goalsConceded) then MatchResult.Draw
                                else MatchResult.Lose
    member this.GoalsScored = goalsScored
    member this.GoalsConceded = goalsConceded
    member this.GoalDifference = goalsScored - goalsConceded
    member this.Date = date
    member this.Location = location

type AggregateResult(pointsForWin, team: Team, resultsIn: ResultForTeam list) =
    member val Position = 0 with get, set 
    member this.Team = team
    member this.Games = resultsIn.Length
    member this.Points = resultsIn 
                        |> List.sumBy(
                            fun(result : ResultForTeam) -> 
                                match result.MatchResult with
                                | MatchResult.Win -> pointsForWin
                                | MatchResult.Draw -> 1
                                | MatchResult.Lose -> 0
                         )
    member this.GoalsScored = resultsIn |> List.sumBy(fun(result : ResultForTeam) ->  result.GoalsScored)
    member this.GoalsConceded = resultsIn |> List.sumBy(fun(result : ResultForTeam) ->  result.GoalsConceded)
    member this.GoalDifference = this.GoalsScored - this.GoalsConceded

type AggregationParameters(sortFunction: AggregateResult -> AggregateResult -> int, filters: seq<ResultForTeam list -> ResultForTeam list>) =
    member this.SortFunction = sortFunction
    member this.Filters = filters
   
let private filterByLocation  (location:Location)  (results:ResultForTeam list) =
    results
    |> List.where(fun result -> result.Location = location)

let private filterByDates startDate endDate (results:ResultForTeam list) =
    results
    |> List.where(fun result -> result.Date >= startDate && result.Date <= endDate)

let private filterOnlyByDates startDate endDate = [filterByDates startDate endDate;]
let private filterByDatesTeamAndHomeSide startDate endDate = [filterByLocation Location.Home; filterByDates startDate endDate]
let private filterByDatesTeamAndAwaySide startDate endDate = [filterByLocation Location.Away; filterByDates startDate endDate]

let createLeagueTable (aggregationParamters: AggregationParameters) (teams: Team list) (results: ResultForTeam list) =
    let findTeamFromTeamName (teams:Team list) (teamName:string) = 
        let teamExistsInList = teams |> List.exists(fun team -> team.GetAllNames() |> Array.contains teamName)
        match teamExistsInList with
        | true -> teams |> List.find(fun team -> team.GetAllNames() |> Array.contains teamName)
        | false -> new Team(teamName, [teamName] |> Array.ofList)

    let filterByTeam (team:Team) (results:ResultForTeam list) =
        results 
        |> List.where(fun(result : ResultForTeam) -> 
            team.GetAllNames() |> Array.contains result.TeamName)

    let applyMultipleFilters (results:ResultForTeam list) filters =
        let mutable resultsWithFilters = results 
        filters |> Seq.iter(fun filter -> resultsWithFilters <- filter resultsWithFilters)
        resultsWithFilters  
     
    let filterResults (team:Team) (results:ResultForTeam list) filters = 
        applyMultipleFilters results filters
        |> filterByTeam team

    let createAggregateResultsForTeam pointsForWin (team: Team) (results: ResultForTeam list) filters= 
        new AggregateResult(pointsForWin, team, filterResults team results filters)

    let createAggregrateResultsForAllTeams pointsForWin filters (results: ResultForTeam list)  = 
        let awayTeams = results |> List.map(fun result -> result.TeamName)
        let homeTeams = results |> List.map(fun result -> result.TeamName)
        let teams = awayTeams |> List.append homeTeams |> List.distinct |>  List.map(fun team -> findTeamFromTeamName teams team ) |> List.distinct
        teams |> List.map(fun(team) -> createAggregateResultsForTeam pointsForWin team results filters)

    let createAggregrateResultsForAllTeamsWithThreePointsForWin = 
        createAggregrateResultsForAllTeams 3

    let mutable order = 1
    let returnVal = 
        results 
        |> createAggregrateResultsForAllTeamsWithThreePointsForWin aggregationParamters.Filters
        |> List.where(fun aggregateResult -> aggregateResult.Games > 0)
        |> List.sortWith aggregationParamters.SortFunction

    returnVal |> List.iter(fun agg -> 
        agg.Position <- order
        order <- order + 1
    )
    returnVal

let sortByPointsThenGoalDifference = fun(x: AggregateResult) (y: AggregateResult) -> 
            if(x.Points > y.Points) then -1 else
            if(x.Points < y.Points) then 1 else
            if(x.GoalDifference > y.GoalDifference) then -1 else 1

let sortByName (x: AggregateResult) (y: AggregateResult) =
            if(x.Team.TeamName > y.Team.TeamName) then -1 else 1

let createLeagueTableWithDefaultSortingAndFilteringHome teams results startDate endDate =
    let aggregationParamters = new AggregationParameters(sortByPointsThenGoalDifference, (filterByDatesTeamAndHomeSide startDate endDate))
    createLeagueTable aggregationParamters teams results 

let createLeagueTableWithDefaultSorting teams results startDate endDate (filterTypes: seq<FilterType>) = 
    let mutable filters = [filterByDates startDate endDate]
    if(filterTypes |> Seq.contains FilterType.HomeOnly) then
        filters <- filters |> List.append [filterByLocation Location.Home]
        else if(filterTypes |> Seq.contains FilterType.AwayOnly) then
            filters <- filters |> List.append [filterByLocation Location.Away]

    let aggregationParamters = new AggregationParameters(sortByPointsThenGoalDifference, filters)
    createLeagueTable aggregationParamters teams results

let createLeagueTableWithNameSorting teams results startDate endDate = 
    let aggregationParamters = new AggregationParameters(sortByName, (filterOnlyByDates startDate endDate))
    createLeagueTable aggregationParamters teams results