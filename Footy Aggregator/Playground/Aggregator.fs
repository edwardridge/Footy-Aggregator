module Aggregator

type MatchResult =
    | Win
    | Draw
    | Lose

type Team(teamName: string, allNames: string []) = 
    member this.TeamName = teamName
    //member private this.AllNames = allNames
    member internal this.GetAllNames = fun _ -> allNames

type ResultForTeam(teamName: string, goalsScored: int, goalsConceded: int) =
    member this.TeamName = teamName
    member this.MatchResult = if(goalsScored > goalsConceded) then MatchResult.Win
                                else if(goalsScored = goalsConceded) then MatchResult.Draw
                                else MatchResult.Lose
    member this.GoalsScored = goalsScored
    member this.GoalsConceded = goalsConceded
    member this.GoalDifference = goalsScored - goalsConceded
//    member this.Date = date
    
type Result(homeTeamResult: ResultForTeam, awayTeamResult: ResultForTeam, date: System.DateTime) = 
    member this.HomeTeamResult = homeTeamResult
    member this.AwayTeamResult = awayTeamResult
    member this.Date = date

type AggregateResult(pointsForWin, team: Team, resultsIn: ResultForTeam list) = 
    member this.Team = team
    //member this.Results = resultsIn
    member this.Games = resultsIn.Length
    member this.Points = resultsIn 
                        |> List.sumBy(
                            fun(i : ResultForTeam) -> 
                                match i.MatchResult with
                                | MatchResult.Win -> pointsForWin
                                | MatchResult.Draw -> 1
                                | MatchResult.Lose -> 0
                         )
    member this.Wins = resultsIn |> List.sumBy(fun(i : ResultForTeam) ->  if i.MatchResult = MatchResult.Win then 1 else 0)
    member this.Loses = resultsIn |> List.sumBy(fun(i : ResultForTeam) ->  if i.MatchResult = MatchResult.Lose then 1 else 0)
    member this.Draws = resultsIn |> List.sumBy(fun(i : ResultForTeam) ->  if i.MatchResult = MatchResult.Draw then 1 else 0)
    member this.GoalsScored = resultsIn |> List.sumBy(fun(i : ResultForTeam) ->  i.GoalsScored)
    member this.GoalsConceded = resultsIn |> List.sumBy(fun(i : ResultForTeam) ->  i.GoalsConceded)
    member this.GoalDifference = this.GoalsScored - this.GoalsConceded

let timeAndRunFunction functionToRun textToPrint =
    printfn "Started %s" textToPrint
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    let returnRes = functionToRun()
    stopWatch.Stop()
    printfn "Ended %s - took %d milliseconds" textToPrint stopWatch.ElapsedMilliseconds
    returnRes

let createLeagueTable sortFunction (teams: Team list) (results: Result list) startDate endDate =
    let findTeamFromTeamName (teams:Team list) (teamName:string) = 
        let teamExistsInList = teams |> List.exists(fun y -> y.GetAllNames() |> Array.contains teamName)
        match teamExistsInList with
        | true -> teams |> List.find(fun x -> x.GetAllNames() |> Array.contains teamName)
        | false -> new Team(teamName, [teamName] |> Array.ofList)

    let filterHomeTeamResults (results:Result list) (team:Team) = 
        results 
        |> List.filter(fun(result : Result) -> 
            team.GetAllNames() |> Array.contains result.HomeTeamResult.TeamName &&
            result.Date >= startDate && result.Date <= endDate)
        |> List.map(fun(result) -> result.HomeTeamResult)

    let filterAwayTeamResults (results:Result list) (team:Team) = 
        results 
        |> List.filter(fun(result : Result) -> 
            team.GetAllNames() |> Array.contains result.AwayTeamResult.TeamName &&
            result.Date >= startDate && result.Date <= endDate)
        |> List.map(fun(result) -> result.AwayTeamResult)
     
    let filterResults (team:Team) (results:Result list) = filterHomeTeamResults results team |> List.append(filterAwayTeamResults results team)

    let createAggregateResultsForTeam pointsForWin (team: Team) (results: Result list) = 
        new AggregateResult(pointsForWin, team, filterResults team results)

    let createAggregrateResultsForAllTeams pointsForWin (results: Result list) startDate = 
        let awayTeams = results |> List.map(fun x -> x.AwayTeamResult.TeamName)
        let homeTeams = results |> List.map(fun x -> x.HomeTeamResult.TeamName)
        let emptyStringArray = [""] |> Array.ofList
        let teams = awayTeams |> List.append homeTeams |> List.distinct |>  List.map(fun x-> findTeamFromTeamName teams x ) |> List.distinct
        teams |> List.map(fun(team) -> createAggregateResultsForTeam pointsForWin team results)

    let createAggregrateResultsForAllTeamsWithThreePointsForWin = 
        createAggregrateResultsForAllTeams 3

    results 
    |> createAggregrateResultsForAllTeamsWithThreePointsForWin results
    |> List.where(fun xx -> xx.Games > 0)
    |> List.sortWith sortFunction

let sortByPointsThenGoalDifference = fun(x: AggregateResult) (y: AggregateResult) -> 
            if(x.Points > y.Points) then -1 else
            if(x.Points < y.Points) then 1 else
            if(x.GoalDifference > y.GoalDifference) then -1 else 1

let sortByName (x: AggregateResult) (y: AggregateResult) =
            if(x.Team.TeamName > y.Team.TeamName) then -1 else 1

let createLeagueTableWithDefaultSorting teams results startDate endDate = createLeagueTable sortByPointsThenGoalDifference teams results startDate endDate

let createLeagueTableWithNameSorting teams results startDate endDate = createLeagueTable sortByName teams results startDate endDate