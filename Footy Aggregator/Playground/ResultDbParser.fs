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

    let parseLine lineToParse = 
        let date = getDate lineToParse
        let homeTeamName = getHomeTeamName lineToParse
        let awayTeamName = getAwayTeamName lineToParse
        let homeGoals = getHomeGoals lineToParse
        let awayGoals = getAwayGoals lineToParse

        [new ResultForTeam(homeTeamName,homeGoals,awayGoals, date, Location.Home); new ResultForTeam(awayTeamName,awayGoals,homeGoals, date, Location.Away)]
    
    interface IParser with
        member this.parse lineToParse = 
           let linesToParse = lineToParse.Split('\n') 
           let parsedLines = linesToParse |> Array.map(fun(line) -> parseLine line)
           let mutable returnVal = List.empty<ResultForTeam>
           parsedLines |> Array.iter(fun resultList -> returnVal <- List.append returnVal resultList)
           returnVal

