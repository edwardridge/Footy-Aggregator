﻿module ResultDbParserTests

open IParser
open Aggregator
open NUnit.Framework
open System
open ResultDbParser

[<Test>]
let ``Sets date correctly with th``() = 
    let lineToParse = "May 19th 2001	Leeds - Leicester City	3-1"
    let parserImp = new ResultDbParser()
    let parser = parserImp :> IParser
    let parsedLine = parser.parse lineToParse
    Assert.AreEqual(new DateTime(2001,5, 19), parsedLine.Head.Date)

[<Test>]
let ``Sets date correctly with st``() = 
    let lineToParse = "May 1st 2001	Leeds - Leicester City	3-1"
    let parserImp = new ResultDbParser()
    let parser = parserImp :> IParser
    let parsedLine = parser.parse lineToParse
    Assert.AreEqual(new DateTime(2001,5, 1), parsedLine.Head.Date)
    
[<Test>]
let ``Sets date correctly with August``() = 
    let lineToParse = "August 1st 2001	Leeds - Leicester City	3-1"
    let parserImp = new ResultDbParser()
    let parser = parserImp :> IParser
    let parsedLine = parser.parse lineToParse
    Assert.AreEqual(new DateTime(2001,8, 1), parsedLine.Head.Date)

[<Test>]
let ``Sets date correctly with nd``() = 
    let lineToParse = "May 2nd 2001	Leeds - Leicester City	3-1"
    let parserImp = new ResultDbParser()
    let parser = parserImp :> IParser
    let parsedLine = parser.parse lineToParse
    Assert.AreEqual(new DateTime(2001,5, 2), parsedLine.Head.Date)

[<Test>]
let ``Sets date correctly with rd``() = 
    let lineToParse = "March 3rd 2001	Arsenal - West Ham	3-0"
    let parserImp = new ResultDbParser()
    let parser = parserImp :> IParser
    let parsedLine = parser.parse lineToParse
    Assert.AreEqual(new DateTime(2001,3, 3), parsedLine.Head.Date)

[<Test>]
let ``Sets home team name correctly``() = 
    let lineToParse = "March 3rd 2001	Arsenal - West Ham	3-0"
    let parserImp = new ResultDbParser()
    let parser = parserImp :> IParser
    let parsedLine = parser.parse lineToParse
    Assert.AreEqual("Arsenal", parsedLine.Head.HomeTeamResult.TeamName)

[<Test>]
let ``Sets away team name correctly``() = 
    let lineToParse = "March 3rd 2001	Arsenal - West Ham	3-0"
    let parserImp = new ResultDbParser()
    let parser = parserImp :> IParser
    let parsedLine = parser.parse lineToParse
    Assert.AreEqual("West Ham", parsedLine.Head.AwayTeamResult.TeamName)

[<Test>]
let ``Sets away team name correctly second test``() = 
    let lineToParse = "March 3rd 2001	Man U - Man C	3-2"
    let parserImp = new ResultDbParser()
    let parser = parserImp :> IParser
    let parsedLine = parser.parse lineToParse
    Assert.AreEqual("Man C", parsedLine.Head.AwayTeamResult.TeamName)

[<Test>]
let ``Sets home goals scored correctly``() = 
    let lineToParse = "March 3rd 2001	Man U - Man C	3-0"
    let parserImp = new ResultDbParser()
    let parser = parserImp :> IParser
    let parsedLine = parser.parse lineToParse
    Assert.AreEqual(3, parsedLine.Head.HomeTeamResult.GoalsScored)

[<Test>]
let ``Sets away goals scored correctly``() = 
    let lineToParse = "April 28th 2001	Middlesbrough - Manchester United	0-2"
    let parserImp = new ResultDbParser()
    let parser = parserImp :> IParser
    let parsedLine = parser.parse lineToParse
    Assert.AreEqual(2, parsedLine.Head.AwayTeamResult.GoalsScored)

[<Test>]
let ``Sets home goals conceded correctly``() = 
    let lineToParse = "April 28th 2001	Middlesbrough - Manchester United	0-2"
    let parserImp = new ResultDbParser()
    let parser = parserImp :> IParser
    let parsedLine = parser.parse lineToParse
    Assert.AreEqual(2, parsedLine.Head.HomeTeamResult.GoalsConceded)

[<Test>]
let ``Sets away goals conceded correctly``() = 
    let lineToParse = "April 28th 2001	Middlesbrough - Manchester United	5-2"
    let parserImp = new ResultDbParser()
    let parser = parserImp :> IParser
    let parsedLine = parser.parse lineToParse
    Assert.AreEqual(5, parsedLine.Head.AwayTeamResult.GoalsConceded)

[<Test>]
let ``Sets home win correctly``() = 
    let lineToParse = "April 28th 2001	Middlesbrough - Manchester United	5-2"
    let parserImp = new ResultDbParser()
    let parser = parserImp :> IParser
    let parsedLine = parser.parse lineToParse
    Assert.AreEqual(MatchResult.Win, parsedLine.Head.HomeTeamResult.MatchResult)

[<Test>]
let ``Sets home draw correctly``() = 
    let lineToParse = "April 28th 2001	Middlesbrough - Manchester United	2-2"
    let parserImp = new ResultDbParser()
    let parser = parserImp :> IParser
    let parsedLine = parser.parse lineToParse
    Assert.AreEqual(MatchResult.Draw, parsedLine.Head.HomeTeamResult.MatchResult)

[<Test>]
let ``Sets home lose correctly``() = 
    let lineToParse = "April 28th 2001	Middlesbrough - Manchester United	2-4"
    let parserImp = new ResultDbParser()
    let parser = parserImp :> IParser
    let parsedLine = parser.parse lineToParse
    Assert.AreEqual(MatchResult.Lose, parsedLine.Head.HomeTeamResult.MatchResult)

[<Test>]
let ``Sets away win correctly``() = 
    let lineToParse = "April 28th 2001	Middlesbrough - Manchester United	0-2"
    let parserImp = new ResultDbParser()
    let parser = parserImp :> IParser
    let parsedLine = parser.parse lineToParse
    Assert.AreEqual(MatchResult.Win, parsedLine.Head.AwayTeamResult.MatchResult)

[<Test>]
let ``Sets away draw correctly``() = 
    let lineToParse = "April 28th 2001	Middlesbrough - Manchester United	2-2"
    let parserImp = new ResultDbParser()
    let parser = parserImp :> IParser
    let parsedLine = parser.parse lineToParse
    Assert.AreEqual(MatchResult.Draw, parsedLine.Head.AwayTeamResult.MatchResult)

[<Test>]
let ``Sets away lose correctly``() = 
    let lineToParse = "April 28th 2001	Middlesbrough - Manchester United	2-0"
    let parserImp = new ResultDbParser()
    let parser = parserImp :> IParser
    let parsedLine = parser.parse lineToParse
    Assert.AreEqual(MatchResult.Lose, parsedLine.Head.AwayTeamResult.MatchResult)

[<Test>]
let ``Sets multiple lines correctly``() = 
    let linesToParse = @"April 28th 2001	Middlesbrough - Manchester United	4-1
May 1st 2002	Derby - Spurs	3-2"
    let parserImp = new ResultDbParser()
    let parser = parserImp :> IParser
    let parsedLines = parser.parse linesToParse

    let firstResult = parsedLines.Head
    let secondResult = Seq.item 1 parsedLines
    Assert.AreEqual(MatchResult.Lose, firstResult.AwayTeamResult.MatchResult)
    Assert.AreEqual(MatchResult.Win, secondResult.HomeTeamResult.MatchResult)
    Assert.AreEqual(3, secondResult.HomeTeamResult.GoalsScored)
    Assert.AreEqual(1, firstResult.AwayTeamResult.GoalsScored)

[<Test>]
let ``Full season test``() = 
    let linesToParse = @"May 15th 2004	Bolton - Fulham	0-2
May 15th 2004	Wolverhampton - Tottenham	0-2
May 15th 2004	Arsenal - Leicester City	2-1
May 15th 2004	Aston Villa - Manchester United	0-2
May 15th 2004	Blackburn - Birmingham	1-1
May 15th 2004	Charlton Athletic - Southampton	2-1
May 15th 2004	Chelsea - Leeds	1-0
May 15th 2004	Liverpool - Newcastle	1-1
May 15th 2004	Manchester City - Everton	5-1
May 15th 2004	Portsmouth - Middlesbrough	5-1
May 12th 2004	Southampton - Newcastle	3-3
May 9th 2004	Fulham - Arsenal	0-1
May 9th 2004	Newcastle - Wolverhampton	1-1
May 8th 2004	Tottenham - Blackburn	1-0
May 8th 2004	Birmingham - Liverpool	0-3
May 8th 2004	Everton - Bolton	1-2
May 8th 2004	Leeds - Charlton Athletic	3-3
May 8th 2004	Leicester City - Portsmouth	3-1
May 8th 2004	Manchester United - Chelsea	1-1
May 8th 2004	Middlesbrough - Manchester City	2-1
May 8th 2004	Southampton - Aston Villa	1-1
May 4th 2004	Portsmouth - Arsenal	1-1
May 2nd 2004	Aston Villa - Tottenham	1-0
May 2nd 2004	Liverpool - Middlesbrough	2-0
May 2nd 2004	Bolton - Leeds	4-1
May 1st 2004	Arsenal - Birmingham	0-0
May 1st 2004	Blackburn - Manchester United	1-0
May 1st 2004	Charlton Athletic - Leicester City	2-2
May 1st 2004	Chelsea - Southampton	4-0
May 1st 2004	Manchester City - Newcastle	1-0
May 1st 2004	Portsmouth - Fulham	1-1
May 1st 2004	Wolverhampton - Everton	2-1
April 25th 2004	Birmingham - Wolverhampton	2-2
April 25th 2004	Leeds - Portsmouth	1-2
April 25th 2004	Newcastle - Chelsea	2-1
April 25th 2004	Tottenham - Arsenal	2-2
April 24th 2004	Everton - Blackburn	0-1
April 24th 2004	Fulham - Charlton Athletic	2-0
April 24th 2004	Leicester City - Manchester City	1-1
April 24th 2004	Manchester United - Liverpool	0-1
April 24th 2004	Middlesbrough - Aston Villa	1-2
April 24th 2004	Southampton - Bolton	1-2
April 20th 2004	Manchester United - Charlton Athletic	2-0
April 18th 2004	Aston Villa - Newcastle	0-0
April 17th 2004	Blackburn - Leicester City	1-0
April 17th 2004	Bolton - Tottenham	2-0
April 17th 2004	Charlton Athletic - Birmingham	1-1
April 17th 2004	Chelsea - Everton	0-0
April 17th 2004	Liverpool - Fulham	0-0
April 17th 2004	Manchester City - Southampton	1-3
April 17th 2004	Portsmouth - Manchester United	1-0
April 17th 2004	Wolverhampton - Middlesbrough	2-0
April 16th 2004	Arsenal - Leeds	5-0
April 13th 2004	Leeds - Everton	1-1
April 13th 2004	Manchester United - Leicester City	1-0
April 12th 2004	Aston Villa - Chelsea	3-2
April 12th 2004	Fulham - Blackburn	3-4
April 12th 2004	Liverpool - Charlton Athletic	0-1
April 12th 2004	Middlesbrough - Southampton	3-1
April 12th 2004	Portsmouth - Birmingham	3-1
April 12th 2004	Tottenham - Manchester City	1-1
April 12th 2004	Wolverhampton - Bolton	1-2
April 11th 2004	Newcastle - Arsenal	0-0
April 10th 2004	Birmingham - Manchester United	1-2
April 10th 2004	Blackburn - Leeds	1-2
April 10th 2004	Bolton - Aston Villa	2-2
April 10th 2004	Charlton Athletic - Portsmouth	1-1
April 10th 2004	Chelsea - Middlesbrough	0-0
April 10th 2004	Leicester City - Fulham	0-2
April 10th 2004	Manchester City - Wolverhampton	3-3
April 9th 2004	Everton - Tottenham	3-1
April 9th 2004	Arsenal - Liverpool	4-2
April 5th 2004	Leeds - Leicester City	3-2
April 4th 2004	Liverpool - Blackburn	4-0
April 4th 2004	Aston Villa - Manchester City	1-1
April 3rd 2004	Fulham - Birmingham	0-0
April 3rd 2004	Middlesbrough - Bolton	2-0
April 3rd 2004	Newcastle - Everton	4-2
April 3rd 2004	Tottenham - Chelsea	0-1
April 3rd 2004	Wolverhampton - Southampton	1-4
March 28th 2004	Leicester City - Liverpool	0-0
March 28th 2004	Bolton - Newcastle	1-0
March 28th 2004	Arsenal - Manchester United	1-1
March 27th 2004	Birmingham - Leeds	4-1
March 27th 2004	Blackburn - Portsmouth	1-2
March 27th 2004	Charlton Athletic - Aston Villa	1-2
March 27th 2004	Chelsea - Wolverhampton	5-2
March 27th 2004	Everton - Middlesbrough	1-1
March 27th 2004	Manchester City - Fulham	0-0
March 27th 2004	Southampton - Tottenham	1-0
March 22nd 2004	Leeds - Manchester City	2-1
March 21st 2004	Portsmouth - Southampton	1-0
March 20th 2004	Aston Villa - Blackburn	0-2
March 20th 2004	Chelsea - Fulham	2-1
March 20th 2004	Arsenal - Bolton	2-1
March 20th 2004	Leicester City - Everton	1-1
March 20th 2004	Liverpool - Wolverhampton	1-0
March 20th 2004	Middlesbrough - Birmingham	5-3
March 20th 2004	Newcastle - Charlton Athletic	3-1
March 20th 2004	Manchester United - Tottenham	3-0
March 17th 2004	Liverpool - Portsmouth	3-0
March 14th 2004	Manchester City - Manchester United	4-1
March 14th 2004	Southampton - Liverpool	2-0
March 14th 2004	Tottenham - Newcastle	1-0
March 14th 2004	Wolverhampton - Aston Villa	0-4
March 13th 2004	Birmingham - Leicester City	0-1
March 13th 2004	Blackburn - Arsenal	0-2
March 13th 2004	Bolton - Chelsea	0-2
March 13th 2004	Charlton Athletic - Middlesbrough	1-0
March 13th 2004	Everton - Portsmouth	1-0
March 13th 2004	Fulham - Leeds	2-0
March 9th 2004	Middlesbrough - Tottenham	1-0
March 6th 2004	Birmingham - Bolton	2-0
March 3rd 2004	Birmingham - Middlesbrough	3-1
February 29th 2004	Portsmouth - Newcastle	1-1
February 29th 2004	Leeds - Liverpool	2-2
February 28th 2004	Arsenal - Charlton Athletic	2-1
February 28th 2004	Blackburn - Southampton	1-1
February 28th 2004	Everton - Aston Villa	2-0
February 28th 2004	Fulham - Manchester United	1-1
February 28th 2004	Leicester City - Wolverhampton	0-0
February 28th 2004	Manchester City - Chelsea	0-1
February 22nd 2004	Tottenham - Leicester City	4-4
February 22nd 2004	Aston Villa - Birmingham	2-2
February 21st 2004	Bolton - Manchester City	1-3
February 21st 2004	Charlton Athletic - Blackburn	3-2
February 21st 2004	Chelsea - Arsenal	1-2
February 21st 2004	Manchester United - Leeds	1-1
February 21st 2004	Newcastle - Middlesbrough	2-1
February 21st 2004	Southampton - Everton	3-3
February 21st 2004	Wolverhampton - Fulham	2-1
February 11th 2004	Birmingham - Everton	3-0
February 11th 2004	Blackburn - Newcastle	1-1
February 11th 2004	Charlton Athletic - Tottenham	2-4
February 11th 2004	Fulham - Aston Villa	1-2
February 11th 2004	Liverpool - Manchester City	2-1
February 11th 2004	Manchester United - Middlesbrough	2-3
February 11th 2004	Portsmouth - Chelsea	0-2
February 10th 2004	Leicester City - Bolton	1-1
February 10th 2004	Leeds - Wolverhampton	4-1
February 10th 2004	Arsenal - Southampton	2-0
February 8th 2004	Chelsea - Charlton Athletic	1-0
February 8th 2004	Manchester City - Birmingham	0-0
February 7th 2004	Wolverhampton - Arsenal	1-3
February 7th 2004	Aston Villa - Leeds	2-0
February 7th 2004	Bolton - Liverpool	2-2
February 7th 2004	Everton - Manchester United	3-4
February 7th 2004	Middlesbrough - Blackburn	0-1
February 7th 2004	Newcastle - Leicester City	3-1
February 7th 2004	Southampton - Fulham	0-0
February 7th 2004	Tottenham - Portsmouth	4-3
February 1st 2004	Blackburn - Chelsea	2-3
February 1st 2004	Arsenal - Manchester City	2-1
January 31st 2004	Birmingham - Newcastle	1-1
January 31st 2004	Charlton Athletic - Bolton	1-2
January 31st 2004	Fulham - Tottenham	2-1
January 31st 2004	Leeds - Middlesbrough	0-3
January 31st 2004	Leicester City - Aston Villa	0-5
January 31st 2004	Liverpool - Everton	0-0
January 31st 2004	Manchester United - Southampton	3-2
January 31st 2004	Portsmouth - Wolverhampton	0-0
January 21st 2004	Wolverhampton - Liverpool	1-1
January 19th 2004	Newcastle - Fulham	3-1
January 18th 2004	Chelsea - Birmingham	0-0
January 18th 2004	Aston Villa - Arsenal	0-2
January 17th 2004	Bolton - Portsmouth	1-0
January 17th 2004	Everton - Charlton Athletic	0-1
January 17th 2004	Manchester City - Blackburn	1-1
January 17th 2004	Middlesbrough - Leicester City	3-3
January 17th 2004	Southampton - Leeds	2-1
January 17th 2004	Tottenham - Liverpool	2-1
January 17th 2004	Wolverhampton - Manchester United	1-0
January 11th 2004	Manchester United - Newcastle	0-0
January 11th 2004	Leicester City - Chelsea	0-4
January 10th 2004	Arsenal - Middlesbrough	4-1
January 10th 2004	Birmingham - Southampton	2-1
January 10th 2004	Blackburn - Bolton	3-4
January 10th 2004	Charlton Athletic - Wolverhampton	2-0
January 10th 2004	Fulham - Everton	2-1
January 10th 2004	Leeds - Tottenham	0-1
January 10th 2004	Liverpool - Aston Villa	1-0
January 10th 2004	Portsmouth - Manchester City	4-2
January 7th 2004	Everton - Arsenal	1-1
January 7th 2004	Manchester City - Charlton Athletic	1-1
January 7th 2004	Middlesbrough - Fulham	2-1
January 7th 2004	Newcastle - Leeds	1-0
January 7th 2004	Southampton - Leicester City	0-0
January 7th 2004	Tottenham - Birmingham	4-1
January 7th 2004	Wolverhampton - Blackburn	2-2
January 7th 2004	Chelsea - Liverpool	0-1
January 7th 2004	Bolton - Manchester United	1-2
January 6th 2004	Aston Villa - Portsmouth	2-1
December 29th 2003	Southampton - Arsenal	0-1
December 28th 2003	Aston Villa - Fulham	3-0
December 28th 2003	Bolton - Leicester City	2-2
December 28th 2003	Chelsea - Portsmouth	3-0
December 28th 2003	Everton - Birmingham	1-0
December 28th 2003	Manchester City - Liverpool	2-2
December 28th 2003	Middlesbrough - Manchester United	0-1
December 28th 2003	Newcastle - Blackburn	0-1
December 28th 2003	Tottenham - Charlton Athletic	0-1
December 28th 2003	Wolverhampton - Leeds	3-1
December 26th 2003	Arsenal - Wolverhampton	3-0
December 26th 2003	Birmingham - Manchester City	2-1
December 26th 2003	Blackburn - Middlesbrough	2-2
December 26th 2003	Charlton Athletic - Chelsea	4-2
December 26th 2003	Fulham - Southampton	2-0
December 26th 2003	Leeds - Aston Villa	0-0
December 26th 2003	Leicester City - Newcastle	1-1
December 26th 2003	Liverpool - Bolton	3-1
December 26th 2003	Manchester United - Everton	3-2
December 26th 2003	Portsmouth - Tottenham	2-0
December 22nd 2003	Manchester City - Leeds	1-1
December 21st 2003	Tottenham - Manchester United	1-2
December 21st 2003	Southampton - Portsmouth	3-0
December 20th 2003	Blackburn - Aston Villa	0-2
December 20th 2003	Bolton - Arsenal	1-1
December 20th 2003	Charlton Athletic - Newcastle	0-0
December 20th 2003	Everton - Leicester City	3-2
December 20th 2003	Fulham - Chelsea	0-1
December 14th 2003	Leeds - Fulham	3-2
December 14th 2003	Aston Villa - Wolverhampton	3-2
December 14th 2003	Arsenal - Blackburn	1-0
December 13th 2003	Chelsea - Bolton	1-2
December 13th 2003	Leicester City - Birmingham	0-2
December 13th 2003	Liverpool - Southampton	1-2
December 13th 2003	Manchester United - Manchester City	3-1
December 13th 2003	Middlesbrough - Charlton Athletic	0-0
December 13th 2003	Newcastle - Tottenham	4-0
December 13th 2003	Portsmouth - Everton	1-2
December 7th 2003	Southampton - Charlton Athletic	3-2
December 7th 2003	Everton - Manchester City	0-0
December 6th 2003	Birmingham - Blackburn	0-4
December 6th 2003	Fulham - Bolton	2-1
December 6th 2003	Leicester City - Arsenal	1-1
December 6th 2003	Leeds - Chelsea	1-1
December 6th 2003	Manchester United - Aston Villa	4-0
December 6th 2003	Middlesbrough - Portsmouth	0-0
December 6th 2003	Newcastle - Liverpool	1-1
December 6th 2003	Tottenham - Wolverhampton	5-2
November 30th 2003	Arsenal - Fulham	0-0
November 30th 2003	Chelsea - Manchester United	1-0
November 30th 2003	Liverpool - Birmingham	3-1
November 30th 2003	Manchester City - Middlesbrough	0-1
November 29th 2003	Aston Villa - Southampton	1-0
November 29th 2003	Blackburn - Tottenham	1-0
November 29th 2003	Bolton - Everton	2-0
November 29th 2003	Charlton Athletic - Leeds	0-1
November 29th 2003	Portsmouth - Leicester City	0-2
November 29th 2003	Wolverhampton - Newcastle	1-1
November 24th 2003	Fulham - Portsmouth	2-0
November 23rd 2003	Tottenham - Aston Villa	2-1
November 22nd 2003	Birmingham - Arsenal	0-3
November 22nd 2003	Everton - Wolverhampton	2-0
November 22nd 2003	Leeds - Bolton	0-2
November 22nd 2003	Leicester City - Charlton Athletic	1-1
November 22nd 2003	Manchester United - Blackburn	2-1
November 22nd 2003	Middlesbrough - Liverpool	0-0
November 22nd 2003	Newcastle - Manchester City	3-0
November 22nd 2003	Southampton - Chelsea	0-1
November 10th 2003	Blackburn - Everton	2-1
November 9th 2003	Chelsea - Newcastle	5-0
November 9th 2003	Liverpool - Manchester United	1-2
November 9th 2003	Manchester City - Leicester City	0-3
November 8th 2003	Arsenal - Tottenham	2-1
November 8th 2003	Aston Villa - Middlesbrough	0-2
November 8th 2003	Bolton - Southampton	0-0
November 8th 2003	Charlton Athletic - Fulham	3-1
November 8th 2003	Portsmouth - Leeds	6-1
November 8th 2003	Wolverhampton - Birmingham	1-1
November 3rd 2003	Birmingham - Charlton Athletic	1-2
November 2nd 2003	Leicester City - Blackburn	2-0
November 2nd 2003	Fulham - Liverpool	1-2
November 1st 2003	Everton - Chelsea	0-1
November 1st 2003	Leeds - Arsenal	1-4
November 1st 2003	Manchester United - Portsmouth	3-0
November 1st 2003	Middlesbrough - Wolverhampton	2-0
November 1st 2003	Newcastle - Aston Villa	1-1
November 1st 2003	Southampton - Manchester City	0-2
November 1st 2003	Tottenham - Bolton	0-1
October 26th 2003	Tottenham - Middlesbrough	0-0
October 26th 2003	Charlton Athletic - Arsenal	1-1
October 25th 2003	Aston Villa - Everton	0-0
October 25th 2003	Bolton - Birmingham	0-1
October 25th 2003	Chelsea - Manchester City	1-0
October 25th 2003	Liverpool - Leeds	3-1
October 25th 2003	Manchester United - Fulham	1-3
October 25th 2003	Newcastle - Portsmouth	3-0
October 25th 2003	Southampton - Blackburn	2-0
October 25th 2003	Wolverhampton - Leicester City	4-3
October 21st 2003	Fulham - Newcastle	2-3
October 20th 2003	Blackburn - Charlton Athletic	0-1
October 19th 2003	Birmingham - Aston Villa	0-0
October 19th 2003	Everton - Southampton	0-0
October 19th 2003	Leicester City - Tottenham	1-2
October 18th 2003	Arsenal - Chelsea	2-1
October 18th 2003	Fulham - Wolverhampton	0-0
October 18th 2003	Leeds - Manchester United	0-1
October 18th 2003	Manchester City - Bolton	6-2
October 18th 2003	Middlesbrough - Newcastle	0-1
October 18th 2003	Portsmouth - Liverpool	1-0
October 14th 2003	Birmingham - Chelsea	0-0
October 5th 2003	Middlesbrough - Chelsea	1-2
October 5th 2003	Aston Villa - Bolton	1-1
October 4th 2003	Fulham - Leicester City	2-0
October 4th 2003	Leeds - Blackburn	2-1
October 4th 2003	Liverpool - Arsenal	1-2
October 4th 2003	Newcastle - Southampton	1-0
October 4th 2003	Portsmouth - Charlton Athletic	1-2
October 4th 2003	Tottenham - Everton	3-0
October 4th 2003	Wolverhampton - Manchester City	1-0
October 4th 2003	Manchester United - Birmingham	3-0
September 28th 2003	Everton - Leeds	4-0
September 28th 2003	Manchester City - Tottenham	0-0
September 28th 2003	Charlton Athletic - Liverpool	3-2
September 28th 2003	Blackburn - Fulham	0-2
September 27th 2003	Birmingham - Portsmouth	2-0
September 27th 2003	Leicester City - Manchester United	1-4
September 27th 2003	Bolton - Wolverhampton	1-1
September 27th 2003	Chelsea - Aston Villa	1-0
September 27th 2003	Southampton - Middlesbrough	0-1
September 26th 2003	Arsenal - Newcastle	3-2
September 21st 2003	Middlesbrough - Everton	1-0
September 21st 2003	Manchester United - Arsenal	0-0
September 20th 2003	Aston Villa - Charlton Athletic	2-1
September 20th 2003	Fulham - Manchester City	2-2
September 20th 2003	Leeds - Birmingham	0-2
September 20th 2003	Liverpool - Leicester City	2-1
September 20th 2003	Newcastle - Bolton	0-0
September 20th 2003	Portsmouth - Blackburn	1-2
September 20th 2003	Wolverhampton - Chelsea	0-5
September 20th 2003	Tottenham - Southampton	1-3
September 15th 2003	Leicester City - Leeds	4-0
September 14th 2003	Manchester City - Aston Villa	4-1
September 14th 2003	Birmingham - Fulham	2-2
September 13th 2003	Arsenal - Portsmouth	1-1
September 13th 2003	Blackburn - Liverpool	1-3
September 13th 2003	Bolton - Middlesbrough	2-0
September 13th 2003	Charlton Athletic - Manchester United	0-2
September 13th 2003	Chelsea - Tottenham	4-2
September 13th 2003	Everton - Newcastle	2-2
September 13th 2003	Southampton - Wolverhampton	2-0
August 31st 2003	Southampton - Manchester United	1-0
August 31st 2003	Manchester City - Arsenal	1-2
August 30th 2003	Aston Villa - Leicester City	3-1
August 30th 2003	Bolton - Charlton Athletic	0-0
August 30th 2003	Chelsea - Blackburn	2-2
August 30th 2003	Everton - Liverpool	0-3
August 30th 2003	Middlesbrough - Leeds	2-3
August 30th 2003	Newcastle - Birmingham	0-1
August 30th 2003	Tottenham - Fulham	0-3
August 30th 2003	Wolverhampton - Portsmouth	0-0
August 27th 2003	Manchester United - Wolverhampton	1-0
August 27th 2003	Liverpool - Tottenham	0-0
August 27th 2003	Arsenal - Aston Villa	2-0
August 26th 2003	Leicester City - Middlesbrough	0-0
August 26th 2003	Charlton Athletic - Everton	2-2
August 26th 2003	Leeds - Southampton	0-0
August 26th 2003	Portsmouth - Bolton	4-0
August 25th 2003	Blackburn - Manchester City	2-3
August 24th 2003	Aston Villa - Liverpool	0-0
August 24th 2003	Middlesbrough - Arsenal	0-4
August 23rd 2003	Bolton - Blackburn	2-2
August 23rd 2003	Chelsea - Leicester City	2-1
August 23rd 2003	Everton - Fulham	3-1
August 23rd 2003	Manchester City - Portsmouth	1-1
August 23rd 2003	Newcastle - Manchester United	1-2
August 23rd 2003	Southampton - Birmingham	0-0
August 23rd 2003	Tottenham - Leeds	2-1
August 23rd 2003	Wolverhampton - Charlton Athletic	0-4
August 17th 2003	Liverpool - Chelsea	1-2
August 17th 2003	Charlton Athletic - Manchester City	0-3
August 17th 2003	Leeds - Newcastle	2-2
August 16th 2003	Arsenal - Everton	2-1
August 16th 2003	Manchester United - Bolton	4-0
August 16th 2003	Leicester City - Southampton	2-2
August 16th 2003	Fulham - Middlesbrough	3-2
August 16th 2003	Blackburn - Wolverhampton	5-1
August 16th 2003	Birmingham - Tottenham	1-0
August 16th 2003	Portsmouth - Aston Villa	2-1"
    let parserImp = new ResultDbParser()
    let parser = parserImp :> IParser
    let parsedLines = parser.parse linesToParse

    let firstResult = parsedLines.Head
    let secondResult = Seq.item 1 parsedLines
    Assert.AreEqual(MatchResult.Win, firstResult.AwayTeamResult.MatchResult)
    