module Combinator

open Aggregator
open RSSSFParser
open ResultDbParser
open System
open IParser
open TeamParser

type Combinator() = 
    let readFileAndPutBackNewlines file = 
        let lines = System.IO.File.ReadLines file
        String.concat "\n" lines

    let parseLines linesToParse (parser:IParser) = 
        parser.parse linesToParse

    let parseFiles baseDir files parser = 
                    files 
                    |> Seq.map(fun x -> readFileAndPutBackNewlines (baseDir + x)) 
                    |> Seq.map(fun y -> parseLines y parser)
                    |> Seq.concat
                    |> List.ofSeq

    let baseDir = "C:\Users\Ed\Documents\GitHub\Footy-Aggregator\Footy Aggregator\Playground\\"

    let teamParser = new TeamParser()
    let teamFile = "Teams.txt"
    
    let filesForRSSSSF = ["1999.txt"; "2012.txt"; "2013.txt"; "2014.txt"]
    let filesForResultDb = ["2000.txt"; "2001.txt"; "2002.txt"; "2003.txt"; "2004.txt"; "2005.txt"; "2006.txt"; "2007.txt"; "2008.txt"; "2009.txt"; "2010.txt"; "2011.txt"]

    let rsssfParserImp = new RSSSFParser()
    let rsssfParser = rsssfParserImp :> IParser
    let rsssfParsedLines = parseFiles baseDir filesForRSSSSF rsssfParser

    let rdbParserImp = new ResultDbParser()
    let rdbParser = rdbParserImp :> IParser
    let rdbParsedLines = parseFiles baseDir filesForResultDb rdbParser

    let teams = teamParser.parseLines (readFileAndPutBackNewlines (baseDir + teamFile)) |> List.ofArray
    let combinedResults = List.append rsssfParsedLines rdbParsedLines
    member this.createLeagueWithTeamsAndSort startDate endDate =
        createLeagueTableWithDefaultSorting teams combinedResults startDate endDate