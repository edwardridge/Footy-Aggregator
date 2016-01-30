//This is the parser for http://www.rsssf.com/histdom.html results
module RSSSFParser

open IParser
open Aggregator
open System
open System.Text.RegularExpressions

type SideParsed(teamName:string, goals:int) =
    member this.TeamName = teamName
    member this.Goals = goals

type RSSSFParser() = 
    let parseHomeSide (lineToParse:string) =
         let splitLineToParse = lineToParse.Split('-')
         let homeSide = splitLineToParse |> Array.head
         let matchDigit = Regex.Match(homeSide, @"\d\d?")
         let homeTeamName = homeSide.Substring(0, matchDigit.Index).Trim()
         let homeTeamGoals = Int32.Parse matchDigit.Value
         new SideParsed(homeTeamName, homeTeamGoals)

    let parseAwaySide (lineToParse:string) =
         let splitLineToParse = lineToParse.Split('-')
         let awaySide = splitLineToParse |> Array.tail |> Array.head
         let matchDigit = Regex.Match(awaySide, @"\d\d?")
         let awayTeamName = awaySide.Substring(matchDigit.Index + 1).Trim()
         let awayTeamGoals = Int32.Parse matchDigit.Value
         new SideParsed(awayTeamName, awayTeamGoals)

    let parseLineInternal lineToParse currentDate =
         let homeSide = parseHomeSide lineToParse
         let awaySide = parseAwaySide lineToParse
         let homeResult = if homeSide.Goals > awaySide.Goals then MatchResult.Win else if homeSide.Goals = awaySide.Goals then MatchResult.Draw else MatchResult.Lose
         let awayResult = match homeResult with 
                          | MatchResult.Win -> MatchResult.Lose
                          | MatchResult.Draw -> MatchResult.Draw
                          | MatchResult.Lose -> MatchResult.Win
     
         let homePoints = if homeSide.Goals > awaySide.Goals then 3 else if homeSide.Goals = awaySide.Goals then 1 else 0
         let awayPoints = match homePoints with 
                          | 3 -> 0 
                          | 1 -> 1 
                          | 0 -> 3
                          | _-> 999

         new Result(new ResultForTeam(homeSide.TeamName, homeSide.Goals, awaySide.Goals), new ResultForTeam(awaySide.TeamName, awaySide.Goals, homeSide.Goals), currentDate)

    let parseDateLine (lineToParse: string) startingYear = 
        let startOfDate = lineToParse.IndexOf('[')
        let endOfDate = lineToParse.IndexOf(']')
        let dateString = lineToParse.Substring(startOfDate + 1 , endOfDate - startOfDate - 1)
        let dateBeforeChange = DateTime.Parse(dateString + " " + startingYear.ToString())
        let date = if dateBeforeChange.Month < 6 then dateBeforeChange.AddYears(1) else dateBeforeChange
        date

    let parseYear (lineToParse:string) = 
        let matchYear = Regex.Match(lineToParse, @"\d\d\d\d")
        Int32.Parse(matchYear.Value)

    member this.parseLine lineToParse = 
        parseLineInternal lineToParse (new DateTime())

    interface IParser with
        member this.parse (linesToParse:string) = 
            let mutable currYear = 1800
            let cleanLines = 
                linesToParse.Split('\n') 
                |> Array.where (fun x -> String.IsNullOrEmpty(x.Trim()) = false)

            let mutable currDate = new DateTime()

            let teams = []
            let mutable results = List.empty<Result>
            cleanLines 
            |> Array.iter 
                (fun x -> 
                    if(x.Contains("[")) then
                        currDate <- parseDateLine x currYear
                        else if x.Contains("Year") then
                            currYear <- parseYear x
                            else if x.Contains("Round") = false then
                                let thingToAppend = parseLineInternal x currDate
                                let newResult = List.append results [thingToAppend]
                                results <- newResult
                )
            results
