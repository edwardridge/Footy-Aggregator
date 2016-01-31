module AggregatorTests

open NUnit.Framework
open Aggregator
open System

[<Test>]
let ``Filtes home results correctly``() = 
    let date = new DateTime(2015,1,1)
    let firstResult = new ResultForTeam("Arsenal", 3, 1, date, Location.Home)
    let secondResult = new ResultForTeam("Arsenal", 2, 2, date, Location.Away)
    let thirdResult = new ResultForTeam("Arsenal", 2, 1, date, Location.Home)
    let results = [firstResult; secondResult; thirdResult]
    let league = createLeagueTableWithDefaultSorting [] results date date [FilterType.HomeOnly]
    let firstLeaguePlace = league |> List.head
    Assert.AreEqual(6, firstLeaguePlace.Points)