//This is the parser for http://www.resultdb.com/
module ResultDbParser

open IParser
open Aggregator
open System

type ResultDbParser() = 
    let getDate (lineToParse:string) =
        let splitLine = lineToParse.Split('\t')
        let dateString = Array.head splitLine
        let dateStringReplaced = dateString.Replace("th", "").Replace("st", "").Replace("nd", "").Replace("rd", "").Replace("Augu", "August")
        //printfn "%s " dateStringReplaced
        DateTime.Parse(dateStringReplaced)

    let getHomeTeamName (lineToParse:string) =
        let splitLine = lineToParse.Split('-')
        let firstHalfOfSplit = Array.head splitLine
        let splitLineAtDate = firstHalfOfSplit.Split('\t')
        let homeTeamNameUntrimmed = Array.item 1 splitLineAtDate
        homeTeamNameUntrimmed.Trim()

    let getAwayTeamName (lineToParse:string) =
        let splitLine = lineToParse.Split('-')
        let secondHalfOfSplit = Array.item 1 splitLine
        let splitLineAtDate = secondHalfOfSplit.Split('\t')
        let awayTeamNameUntrimmed = Array.head splitLineAtDate
        awayTeamNameUntrimmed.Trim()

    let getHomeGoals (lineToParse:string) =
        let splitLine = lineToParse.Split('-')
        let secondHalfOfSplit = Array.item 1 splitLine
        let splitLineAtDate = secondHalfOfSplit.Split('\t')
        let homeGoals = Array.item 1 splitLineAtDate
        Int32.Parse homeGoals

    let getAwayGoals (lineToParse:string) =
        let splitLine = lineToParse.Split('-')
        let goals = Array.item 2 splitLine
        Int32.Parse goals

    let getPoints matchResult = 
        match matchResult with
            | MatchResult.Win -> 3
            | MatchResult.Draw -> 1
            | MatchResult.Lose -> 0


    let parseLine lineToParse = 
        let date = getDate lineToParse
        let homeTeamName = getHomeTeamName lineToParse
        let awayTeamName = getAwayTeamName lineToParse
        let homeGoals = getHomeGoals lineToParse
        let awayGoals = getAwayGoals lineToParse

        let homeResult = if homeGoals > awayGoals then MatchResult.Win else
                            if homeGoals = awayGoals then MatchResult.Draw else
                                MatchResult.Lose

        let awayResult = match homeResult with
                            | MatchResult.Win -> MatchResult.Lose
                            | MatchResult.Draw -> MatchResult.Draw
                            | MatchResult.Lose -> MatchResult.Win

        new Result(new ResultForTeam(homeTeamName,homeGoals,awayGoals), new ResultForTeam(awayTeamName,awayGoals,homeGoals), date)
    
    interface IParser with
        member this.parse lineToParse = 
           let linesToParse = lineToParse.Split('\n') 
           let parsedLines = linesToParse |> Array.map(fun(x) -> parseLine x)
           parsedLines |> List.ofArray

