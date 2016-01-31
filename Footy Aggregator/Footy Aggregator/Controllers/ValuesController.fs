namespace Footy_Aggregator.Controllers
open System
open System.Collections.Generic
open System.Linq
open System.Net.Http
open System.Web.Http
open Combinator
open Aggregator

[<RoutePrefix("api")>]
type ValuesController() =
    inherit ApiController()
    
    [<Route("league")>]
    member x.Get() = 
        let combinator = new Combinator()
        let league = combinator.createLeagueWithTeamsAndSort (new DateTime(1999,1,1)) (new DateTime(2019,1,1))
        league

    [<Route("league/{startDate:datetime}/{endDate:datetime}")>]
    member x.Get(startDate, endDate) =
//        let convertStringToDate (dateAsString:string) = 
//            let year = Int32.Parse(dateAsString.Substring(0, 4))
//            let month = Int32.Parse(dateAsString.Substring(4,2))
//            let day = Int32.Parse(dateAsString.Substring(6,2))
//            new DateTime(year, month, day)
//        let startDate = convertStringToDate startDateString
//        let endDate = convertStringToDate endDateString
        let combinator = new Combinator()
        let league = combinator.createLeagueWithTeamsAndSort startDate endDate
        league