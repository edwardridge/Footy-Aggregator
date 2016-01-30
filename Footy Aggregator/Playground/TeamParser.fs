module TeamParser

open Aggregator
open System

type TeamParser() = 
    let parseLine (lineToParse:string) = 
        let splitLine = lineToParse.Split(',') |> Array.map(fun x -> x.Trim())
        new Team(Array.head splitLine, splitLine)

    member this.parseLines (lineToParse:string) = 
        lineToParse.Split('\n') |> Array.map(fun x -> parseLine x)