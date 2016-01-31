module Aggregator

type MatchResult =
    | Win
    | Draw
    | Lose

type Location =
    | Home
    | Away

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
   

let createLeagueTable sortFunction (teams: Team list) (results: ResultForTeam list) startDate endDate =
    let findTeamFromTeamName (teams:Team list) (teamName:string) = 
        let teamExistsInList = teams |> List.exists(fun team -> team.GetAllNames() |> Array.contains teamName)
        match teamExistsInList with
        | true -> teams |> List.find(fun team -> team.GetAllNames() |> Array.contains teamName)
        | false -> new Team(teamName, [teamName] |> Array.ofList)

    let filterByLocation  (location:Location)  (results:ResultForTeam list) =
        results
        |> List.where(fun result -> result.Location = location)

    let filterByDates startDate endDate (results:ResultForTeam list) =
        results
        |> List.where(fun result -> result.Date >= startDate && result.Date <= endDate)

    let filterByTeam (team:Team) (results:ResultForTeam list) =
        results 
        |> List.where(fun(result : ResultForTeam) -> 
            team.GetAllNames() |> Array.contains result.TeamName)

    let filterHomeTeamResults (results:ResultForTeam list) = 
        results 
        |> filterByLocation Location.Home

    let filterAwayTeamResults (results:ResultForTeam list) = 
        results 
        |> filterByLocation Location.Away

    let applyMultipleFilters (results:ResultForTeam list) filters =
        let mutable resultsWithFilters = results 
        filters |> Seq.iter(fun filter -> resultsWithFilters <- filter resultsWithFilters)
        resultsWithFilters  
     
    let filterResults (team:Team) (results:ResultForTeam list) = 
        let filters = [filterByTeam team; filterByDates startDate endDate]
        applyMultipleFilters results filters

    let createAggregateResultsForTeam pointsForWin (team: Team) (results: ResultForTeam list) = 
        new AggregateResult(pointsForWin, team, filterResults team results)

    let createAggregrateResultsForAllTeams pointsForWin (results: ResultForTeam list) startDate = 
        let awayTeams = results |> List.map(fun result -> result.TeamName)
        let homeTeams = results |> List.map(fun result -> result.TeamName)
        let teams = awayTeams |> List.append homeTeams |> List.distinct |>  List.map(fun team -> findTeamFromTeamName teams team ) |> List.distinct
        teams |> List.map(fun(team) -> createAggregateResultsForTeam pointsForWin team results)

    let createAggregrateResultsForAllTeamsWithThreePointsForWin = 
        createAggregrateResultsForAllTeams 3

    let mutable order = 1
    let returnVal = 
        results 
        |> createAggregrateResultsForAllTeamsWithThreePointsForWin results
        |> List.where(fun aggregateResult -> aggregateResult.Games > 0)
        |> List.sortWith sortFunction

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

let createLeagueTableWithDefaultSorting teams results startDate endDate = createLeagueTable sortByPointsThenGoalDifference teams results startDate endDate

let createLeagueTableWithNameSorting teams results startDate endDate = createLeagueTable sortByName teams results startDate endDate