open Combinator
open Aggregator
open RSSSFParser
open ResultDbParser
open System
open IParser
open TeamParser

let printFullInfoForAggregateResult (aggregate: AggregateResult) = 
    printfn "Team name is %s" aggregate.Team.TeamName 
    printfn "Goals scored is %d" aggregate.GoalsScored 
    printfn "Points is %d" aggregate.Points 

let padTeamName (teamName:string) =    
    teamName.PadRight(20)

let padNumberColumn number = 
    number.ToString().PadRight(5)

let printLeagueTableRow (aggregateResult:AggregateResult) =
    let paddedTeamName = padTeamName aggregateResult.Team.TeamName
    printfn "%s %s %s %s" paddedTeamName (padNumberColumn aggregateResult.Games) (padNumberColumn aggregateResult.Points) (padNumberColumn aggregateResult.GoalDifference)

let printLeagueTable (aggregateResults:AggregateResult list) =
    printfn "%s %s %s %s" (padTeamName "Team") (padNumberColumn "Games") (padNumberColumn "P") (padNumberColumn "GD")
    printfn "--------------------------------------------------------------------------"
    aggregateResults |> List.iter printLeagueTableRow
    

[<EntryPoint>]
let main argv = 
    let combinator = new Combinator()
    printfn "Enter two dates, seperated by a comma"
    let mutable line = Console.ReadLine()
    while line <> "q" do
        printfn "Line: %s" line
        let splitLine = line.Split(',')
        let startDate = DateTime.Parse(Array.head splitLine)
        let endDate = DateTime.Parse(Array.item 1 splitLine)


        let leagueTable = combinator.createLeagueWithTeamsAndSort startDate endDate []
        printLeagueTable leagueTable
        line <- Console.ReadLine()
    0 // return an integer exit code
