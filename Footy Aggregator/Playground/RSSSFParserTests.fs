module RSSSFParserTests

open NUnit.Framework
open IParser
open RSSSFParser
open Aggregator
open System

let getResultsForLocation (results: ResultForTeam list) (location: Location) = 
    let resultsForLocation = results |> List.where(fun x -> x.Location = location)
    resultsForLocation

[<Test>]
let ``With Arsenal as home team line, home team name is Arsenal``() = 
    let lineToParse = "Arsenal 0-0 Everton"
    let parser = new RSSSFParser()
    let parsedLine = parser.parseLine lineToParse 
    let homeTeam = getResultsForLocation parsedLine Location.Home |> Seq.head
    Assert.AreEqual("Arsenal", homeTeam.TeamName)

[<Test>]
let ``With Leceister as home team line, home team name is Leceister``() = 
    let lineToParse = "Leceister 0-0 Everton"
    let parser = new RSSSFParser()
    let parsedLine = parser.parseLine lineToParse
    let homeTeam = getResultsForLocation parsedLine Location.Home |> Seq.head
    Assert.AreEqual("Leceister", homeTeam.TeamName)

[<Test>]
let ``With Arsenal as home team with 1 goal, home team goals scored is 1``() = 
    let lineToParse = "Arsenal 1-0 Everton"
    let parser = new RSSSFParser()
    let parsedLine = parser.parseLine lineToParse
    let team = getResultsForLocation parsedLine Location.Home |> Seq.head
    Assert.AreEqual(1, team.GoalsScored)

[<Test>]
let ``With West Ham as home team, home team name is West Ham``() = 
    let lineToParse = "West Ham 2-0 Everton"
    let parser = new RSSSFParser()
    let parsedLine = parser.parseLine lineToParse
    let team = getResultsForLocation parsedLine Location.Home |> Seq.head
    Assert.AreEqual("West Ham", team.TeamName)

[<Test>]
let ``With West Ham as home team with 2 goals, home team goals scored is 2``() = 
    let lineToParse = "West Ham 2-0 Everton"
    let parser = new RSSSFParser()
    let parsedLine = parser.parseLine lineToParse
    let team = getResultsForLocation parsedLine Location.Home |> Seq.head
    Assert.AreEqual(2, team.GoalsScored)

[<Test>]
let ``With Tottenham as away team, away team name is Tottenham``() = 
    let lineToParse = "Arsenal 0-0 Tottenham"
    let parser = new RSSSFParser()
    let parsedLine = parser.parseLine lineToParse
    let team = getResultsForLocation parsedLine Location.Away |> Seq.head
    Assert.AreEqual("Tottenham", team.TeamName)

[<Test>]
let ``With Tottenham as away team and score 3 goals, 3 goals are scored``() = 
    let lineToParse = "Arsenal 0-3 Tottenham"
    let parser = new RSSSFParser()
    let parsedLine = parser.parseLine lineToParse
    let team = getResultsForLocation parsedLine Location.Away |> Seq.head
    Assert.AreEqual(3, team.GoalsScored)

[<Test>]
let ``With home team scoring 2 goals and away team scoring 1 goal, home team wins``() = 
    let lineToParse = "Arsenal 2-1 Tottenham"
    let parser = new RSSSFParser()
    let parsedLine = parser.parseLine lineToParse
    let team = getResultsForLocation parsedLine Location.Home |> Seq.head
    Assert.AreEqual(MatchResult.Win, team.MatchResult)

[<Test>]
let ``With home team scoring 1 goal and away team scoring 2 goals, away team wins``() = 
    let lineToParse = "Arsenal 1-2 Tottenham"
    let parser = new RSSSFParser()
    let parsedLine = parser.parseLine lineToParse
    let team = getResultsForLocation parsedLine Location.Away |> Seq.head
    Assert.AreEqual(MatchResult.Win, team.MatchResult)

[<Test>]
let ``With home team scoring 4 goals and away team scoring 3 goals, home goals conceded is 3``() = 
    let lineToParse = "Arsenal 4-3 Crystal Palace"
    let parser = new RSSSFParser()
    let parsedLine = parser.parseLine lineToParse
    let team = getResultsForLocation parsedLine Location.Home |> Seq.head
    Assert.AreEqual(3, team.GoalsConceded)

[<Test>]
let ``With home team scoring 5 goals and away team scoring 1 goals, away goals conceded is 5``() = 
    let lineToParse = "Man City 5-1 Man U"
    let parser = new RSSSFParser()
    let parsedLine = parser.parseLine lineToParse
    let team = getResultsForLocation parsedLine Location.Away |> Seq.head
    Assert.AreEqual(5, team.GoalsConceded)

[<Test>]
let ``With home team scoring 8 goals and away team scoring 5 goals, home goal difference is 3``() = 
    let lineToParse = "Man City 8-5 Man U"
    let parser = new RSSSFParser()
    let parsedLine = parser.parseLine lineToParse
    let team = getResultsForLocation parsedLine Location.Home |> Seq.head
    Assert.AreEqual(3, team.GoalDifference)

[<Test>]
let ``With home team scoring 1 goals and away team scoring 3 goals, home goal difference is -2``() = 
    let lineToParse = "Man City 1-3 Man U"
    let parser = new RSSSFParser()
    let parsedLine = parser.parseLine lineToParse
    let team = getResultsForLocation parsedLine Location.Home |> Seq.head
    Assert.AreEqual(-2, team.GoalDifference)

[<Test>]
let ``With home team scoring 2 goals and away team scoring 5 goals, away goal difference is 3``() = 
    let lineToParse = "Spurs 2-5 Arsenal"
    let parser = new RSSSFParser()
    let parsedLine = parser.parseLine lineToParse
    let team = getResultsForLocation parsedLine Location.Away |> Seq.head
    Assert.AreEqual(3, team.GoalDifference)

[<Test>]
let ``Feed test - With home team scoring 1 goals and away team scoring 3 goals, home goal difference is -2``() = 
    let lineToParse = "Burnley       1-3 West Ham "
    let parser = new RSSSFParser()
    let parsedLine = parser.parseLine lineToParse
    let team = getResultsForLocation parsedLine Location.Home |> Seq.head
    Assert.AreEqual(-2, team.GoalDifference)


[<Test>]
let ``Multiple line test``() = 
    let linesToParse = 
        @"Everton       3-0 Aston Villa   
        Crystal P     1-2 Chelsea"
    let parserImp = new RSSSFParser()
    let parser = parserImp :> IParser
    let parsedLines = parser.parse linesToParse
    let firstResultHome = getResultsForLocation parsedLines Location.Home |> Seq.head
    let firstResultAway  =getResultsForLocation parsedLines Location.Away |> Seq.head
    let secondResultHome = getResultsForLocation parsedLines Location.Home |> Seq.item 1
    let secondResultAway  =getResultsForLocation parsedLines Location.Away |> Seq.item 1

    Assert.AreEqual(3, firstResultHome.GoalDifference)
    Assert.AreEqual(MatchResult.Win, firstResultHome.MatchResult)
    Assert.AreEqual(MatchResult.Lose, secondResultHome.MatchResult)
    Assert.AreEqual(1, secondResultAway.GoalsConceded )

[<Test>]
let ``Multiple line test - new line ignored``() = 
    let linesToParse = 
        @"Stoke         1-0 Newcastle     

        Aston Villa   0-2 Manchester C "
    let parserImp = new RSSSFParser()
    let parser = parserImp :> IParser
    let parsedLines = parser.parse linesToParse 
    
    Assert.AreEqual(4, parsedLines.Length )

[<Test>]
let ``Multiple line test - lines that contain Round are ignored``() = 
    let linesToParse = 
        @"Stoke         1-0 Newcastle     
        Round 2
        Aston Villa   0-2 Manchester C "
    let parserImp = new RSSSFParser()
    let parser = parserImp :> IParser
    let parsedLines = parser.parse linesToParse 
    
    Assert.AreEqual(4, parsedLines.Length )

[<Test>]
let ``Multiple line test - lines that contain [ are ignored``() = 
    let linesToParse = 
        @"Stoke         1-0 Newcastle     
        [Aug 18]
        Aston Villa   0-2 Manchester C "
    let parserImp = new RSSSFParser()
    let parser = parserImp :> IParser
    let parsedLines = parser.parse linesToParse 
    
    Assert.AreEqual(4, parsedLines.Length )

[<Test>]
let ``Multiple line test - lines that contain [ set the date``() = 
    let linesToParse = 
        @"[Aug 18]
        Aston Villa   0-2 Manchester C "
    let parserImp = new RSSSFParser()
    let parser = parserImp :> IParser

    let parsedLines = parser.parse linesToParse 
    let firstRow = parsedLines |> List.head
    Assert.AreEqual(firstRow.Date.Month, 8)
    Assert.AreEqual(firstRow.Date.Day, 18)

[<Test>]
let ``Multiple line test - lines that contain [ set the date - with text before [``() = 
    let linesToParse = 
        @"Round 3 [Sep 03]
        Aston Villa   0-2 Manchester C "
    let parserImp = new RSSSFParser()
    let parser = parserImp :> IParser

    let parsedLines = parser.parse linesToParse 
    let firstRow = parsedLines |> List.head
    Assert.AreEqual(firstRow.Date.Month, 9)
    Assert.AreEqual(firstRow.Date.Day, 3)

[<Test>]
let ``Multiple line test - multiple dates``() = 
    let linesToParse = 
        @"Round 3 [Sep 03]
        Aston Villa   0-2 Manchester C
        Round 3 [Sep 10]
        Arsenal   0-2 West Ham "
    let parserImp = new RSSSFParser()
    let parser = parserImp :> IParser

    let parsedLines = parser.parse linesToParse 
    let firstRow =  List.item 0 parsedLines
    let secondRow =  List.item 2 parsedLines
    Assert.AreEqual(firstRow.Date.Month, 9)
    Assert.AreEqual(firstRow.Date.Day, 3)
    Assert.AreEqual(secondRow.Date.Month, 9)
    Assert.AreEqual(secondRow.Date.Day, 10)

[<Test>]
let ``Multiple line test - new year is handled correctly``() = 
    let year = 2014
    let linesToParse = 
        @"Year: " + year.ToString() + "
        Round 3 [Dec 31]
        Aston Villa   0-2 Manchester C
        Round 3 [Jan 01]
        Arsenal   0-2 West Ham "
    let parserImp = new RSSSFParser()
    let parser = parserImp :> IParser

    let parsedLines = parser.parse linesToParse
    let firstRow =  List.item 0 parsedLines
    let secondRow =  List.item 2 parsedLines
    Assert.AreEqual(firstRow.Date, new DateTime(year, 12 ,31))
    Assert.AreEqual(secondRow.Date, new DateTime(year + 1, 1 ,1))

[<Test>]
let ``Multiple line test - year is set``() = 
    let year = 2014
    let linesToParse = 
        @"Year: " + year.ToString() + "
        Round 3 [Dec 31]
        Aston Villa   0-2 Manchester C"
    let parserImp = new RSSSFParser()
    let parser = parserImp :> IParser

    let parsedLines = parser.parse linesToParse
    let firstRow =  List.item 0 parsedLines
    Assert.AreEqual(firstRow.Date, new DateTime(year, 12 ,31))

[<Test>]
let ``Multiple line test - year can be changed``() = 
    let year = 2014
    let linesToParse = 
        @"Year: " + year.ToString() + "
        Round 3 [Dec 01]
        Aston Villa   0-2 Manchester C
        Year: " + (year + 1).ToString() + "
        [Dec 03]
        Arsenal   0-2 Manchester C"
    let parserImp = new RSSSFParser()
    let parser = parserImp :> IParser

    let parsedLines = parser.parse linesToParse
    let firstRow =  List.item 0 parsedLines
    let secondRow =  List.item 2 parsedLines
    Assert.AreEqual(firstRow.Date, new DateTime(year, 12 ,1))
    Assert.AreEqual(secondRow.Date, new DateTime(year + 1, 12 ,3))

//[<Test>]
//let ``First round test!``() = 
//    let linesToParse = 
//        @"Round 1
//        [Aug 16]
//        Arsenal       2-1 Crystal P     
//        Leicester     2-2 Everton       
//        Manchester U  1-2 Swansea       
//        QPR           0-1 Hull          
//        Stoke         0-1 Aston Villa   
//        West Bromwich 2-2 Sunderland    
//        West Ham      0-1 Tottenham     
//        [Aug 17]
//        Liverpool     2-1 Southampton   
//        Newcastle     0-2 Manchester C  
//        [Aug 18]
//        Burnley       1-3 Chelsea"
//    let parserImp = new RSSSFParser()
//    let parser = parserImp :> IParser
//
//    let parsedLines = parser.parse linesToParse 
//    let leagueTable = createLeagueTableWithDefaultSorting parsedLines (new DateTime()) (new DateTime())
//    let topTeam = leagueTable.Head
//    Assert.AreEqual("Chelsea", topTeam.Team.TeamName )
//
//[<Test>]
//let ``With two rounds teams aren't duplicated``() = 
//    let linesToParse = 
//        @"Round 37
//[May 16]
//Southampton   6-1 Aston Villa   
//Burnley       0-0 Stoke         
//QPR           2-1 Newcastle     
//Sunderland    0-0 Leicester     
//Tottenham     2-0 Hull          
//West Ham      1-2 Everton       
//Liverpool     1-3 Crystal P     
//[May 17]
//Swansea       2-4 Manchester C  
//Manchester U  1-1 Arsenal       
//[May 18]
//West Bromwich 3-0 Chelsea       
//
//Round 38 [May 24]
//Arsenal       4-1 West Bromwich 
//Aston Villa   0-1 Burnley       
//Chelsea       3-1 Sunderland    
//Crystal P     1-0 Swansea       
//Everton       0-1 Tottenham     
//Hull          0-0 Manchester U  
//Leicester     5-1 QPR           
//Manchester C  2-0 Southampton   
//Newcastle     2-0 West Ham      
//Stoke         6-1 Liverpool   "
//    let parserImp = new RSSSFParser()
//    let parser = parserImp :> IParser
//
//    let parsedLines = parser.parse linesToParse 
//    let leagueTable = createLeagueTableWithDefaultSorting parsedLines (new DateTime()) (new DateTime())
//    let arsenalCounts = leagueTable |> List.where(fun x -> x.Team.TeamName = "Arsenal")
//    Assert.AreEqual(1, arsenalCounts.Length )
//
//[<Test>]
//let ``With one team getting 3 points and 1 points aggregate points is 4``() = 
//    let linesToParse = 
//        @"Manchester U  1-1 Arsenal       
//        Arsenal       4-1 West Bromwich "
//    let parserImp = new RSSSFParser()
//    let parser = parserImp :> IParser
//
//    let parsedLines = parser.parse linesToParse 
//    let leagueTable = createLeagueTableWithDefaultSorting parsedLines (new DateTime()) (new DateTime())
//    
//    let arsenalAggResults = leagueTable |> List.filter(fun x -> x.Team.TeamName = "Arsenal") |> List.head
//    Assert.AreEqual(arsenalAggResults.Points, 4)
//
//[<Test>]
//let ``League table is correct v1``() = 
//    let linesToParse = 
//        @"Manchester U 0-1 Arsenal       
//        Arsenal       1-0 West Bromwich 
//        Manchester U 0-1 West Bromwich"
//    let parserImp = new RSSSFParser()
//    let parser = parserImp :> IParser
//
//    let parsedLines = parser.parse linesToParse 
//    let leagueTable = createLeagueTableWithDefaultSorting parsedLines (new DateTime()) (new DateTime())
//    
//    let topOfTable = leagueTable |> List.head
//    let middleOfTable = List.item 1 leagueTable
//    let bottomOfTable = List.item 2 leagueTable
//    Assert.AreEqual(topOfTable.Team.TeamName, "Arsenal")    
//    Assert.AreEqual(middleOfTable.Team.TeamName, "West Bromwich")    
//    Assert.AreEqual(bottomOfTable.Team.TeamName, "Manchester U")