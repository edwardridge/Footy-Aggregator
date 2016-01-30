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

         [new ResultForTeam(homeSide.TeamName, homeSide.Goals, awaySide.Goals, currentDate, Location.Home); new ResultForTeam(awaySide.TeamName, awaySide.Goals, homeSide.Goals, currentDate, Location.Away)]

    let parseDateLine (lineToParse: string) startingYear = 
        let startOfDate = lineToParse.IndexOf('[')
        let endOfDate = lineToParse.IndexOf(']')
        let dateString = lineToParse.Substring(startOfDate + 1 , endOfDate - startOfDate - 1)
        let dateBeforeChange = DateTime.Parse(dateString + " " + startingYear.ToString())
        if dateBeforeChange.Month < 6 then dateBeforeChange.AddYears(1) else dateBeforeChange

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
            let mutable results = List.empty<ResultForTeam>
            cleanLines 
            |> Array.iter 
                (fun line -> 
                    if(line.Contains("[")) then
                        currDate <- parseDateLine line currYear
                        else if line.Contains("Year") then
                            currYear <- parseYear line
                            else if line.Contains("Round") = false then
                                results <- List.append results (parseLineInternal line currDate)
                )
            results
