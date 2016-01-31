module TeamParserTests

open NUnit.Framework
open TeamParser
open System
open Aggregator


[<Test>]
let ``Parses one name correctly``() = 
    let lineToParse = "Arsenal"
    let teamParser = new TeamParser()
    let parsedLines = teamParser.parseLines lineToParse
    let firstLine = parsedLines |> Seq.head
    Assert.AreEqual("Arsenal",firstLine.TeamName)

[<Test>]
let ``Parses two names correctly``() = 
    let lineToParse = "Arsenal, Gunners"
    let teamParser = new TeamParser()
    let parsedLines = teamParser.parseLines lineToParse
    let firstLine = parsedLines |> Seq.head
    Assert.AreEqual("Arsenal",firstLine.TeamName)
    Assert.AreEqual(["Arsenal"; "Gunners"],firstLine.GetAllNames())

[<Test>]
let ``Parses two lines correctly``() = 
    let lineToParse = @"Arsenal, Gunners
    Manchester City, Man C"
    let teamParser = new TeamParser()
    let parsedLines = teamParser.parseLines lineToParse
    let firstLine = parsedLines |> Seq.head
    let secondLine = parsedLines |> Seq.item 1
    Assert.AreEqual("Arsenal",firstLine.TeamName)
    Assert.AreEqual(["Arsenal"; "Gunners"],firstLine.GetAllNames())
    Assert.AreEqual("Manchester City",secondLine.TeamName)
    Assert.AreEqual(["Manchester City"; "Man C"],secondLine.GetAllNames())

[<Test>]
let ``League table correctly joins teams``() = 
    let teamsToParse = @"Arsenal, Gunners
    Manchester City, Man C"
    let teamParser = new TeamParser()
    let parsedTeams = teamParser.parseLines teamsToParse |> List.ofArray
    let results = [new ResultForTeam("Arsenal",2,1, new DateTime(2015,1,1), Location.Home); new ResultForTeam("Man C",1,2, new DateTime(2015,1,1), Location.Away)]
//    let results = [firstResult]
    let leagueTable = createLeagueTableWithDefaultSorting parsedTeams results (new DateTime(2014,1,1)) (new DateTime(2016,1,1)) []
    let topOfLeague = leagueTable |> List.head
    let bottomOfLeague = leagueTable |> List.item 1
    Assert.AreEqual("Arsenal",topOfLeague.Team.TeamName)
    Assert.AreEqual("Manchester City",bottomOfLeague.Team.TeamName)

[<Test>]
let ``Fills in missing team``() = 
    let teamsToParse = @"Arsenal"
    let teamParser = new TeamParser()
    let parsedTeams = teamParser.parseLines teamsToParse |> List.ofArray
    let results = [new ResultForTeam("Arsenal",2,1, new DateTime(2015,1,1), Location.Home); new ResultForTeam("Man C",1,2, new DateTime(2015,1,1), Location.Away)]
//    let results = [firstResult]
    let leagueTable = createLeagueTableWithDefaultSorting parsedTeams results (new DateTime(2014,1,1)) (new DateTime(2016,1,1)) []
    let topOfLeague = leagueTable |> List.head
    let bottomOfLeague = leagueTable |> List.item 1
    Assert.AreEqual("Arsenal",topOfLeague.Team.TeamName)
    Assert.AreEqual("Man C",bottomOfLeague.Team.TeamName)

[<Test>]
let ``Aggregates results``() = 
    let teamsToParse = @"Arsenal, Gunners
    Manchester City, Man C"
    let teamParser = new TeamParser()
    let parsedTeams = teamParser.parseLines teamsToParse |> List.ofArray
    let firstResults = [new ResultForTeam("Arsenal",2,1, new DateTime(2015,1,1), Location.Home); new ResultForTeam("Man C",1,2, new DateTime(2015,1,1), Location.Away)]
    let secondResults = [new ResultForTeam("Gunners",1,1, new DateTime(2015,1,1), Location.Home); new ResultForTeam("Manchester City",1,1, new DateTime(2015,1,1), Location.Away)]
    let results = firstResults |> List.append secondResults
    let leagueTable = createLeagueTableWithDefaultSorting parsedTeams results (new DateTime(2014,1,1)) (new DateTime(2016,1,1)) []
    let topOfLeague = leagueTable |> List.head
    let bottomOfLeague = leagueTable |> List.item 1
    Assert.AreEqual("Arsenal",topOfLeague.Team.TeamName)
    Assert.AreEqual(4,topOfLeague.Points)
    Assert.AreEqual("Manchester City",bottomOfLeague.Team.TeamName)    
    Assert.AreEqual(1,bottomOfLeague.Points)

[<Test>]
let ``Removes teams with no results``() = 
    let teamsToParse = @"Arsenal, Gunners
    Manchester City, Man C
    Derby"
    let teamParser = new TeamParser()
    let parsedTeams = teamParser.parseLines teamsToParse |> List.ofArray
    let firstResults = [new ResultForTeam("Arsenal",2,1, new DateTime(2015,1,1), Location.Home); new ResultForTeam("Man C",1,2, new DateTime(2015,1,1), Location.Away)]
    let secondResults = [new ResultForTeam("Gunners",1,1, new DateTime(2015,1,1), Location.Home); new ResultForTeam("Manchester City",1,1, new DateTime(2015,1,1), Location.Away)]
    let thirdIgnoredResults = [new ResultForTeam("Derby",1,1, new DateTime(2016,1,1), Location.Home); new ResultForTeam("Manchester City",1,1, new DateTime(2016,1,1), Location.Away)]
    let results = firstResults |> List.append secondResults |> List.append thirdIgnoredResults  
    let leagueTable = createLeagueTableWithDefaultSorting parsedTeams results (new DateTime(2014,1,1)) (new DateTime(2015,1,1)) []
    let topOfLeague = leagueTable |> List.head
    let bottomOfLeague = leagueTable |> List.item 1
    Assert.AreEqual(2, leagueTable.Length)