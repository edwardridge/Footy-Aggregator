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
        let league = combinator.createLeagueWithTeamsAndSort (new DateTime(1999,1,1)) (new DateTime(2019,1,1)) []
        league

    [<Route("league/{startDate:datetime}/{endDate:datetime}/{filterType='None'?}")>]
    member x.Get(startDate, endDate, filterType) =
        let additionalFilters = 
            if(filterType = "Home") then [FilterType.HomeOnly]
                else if(filterType = "Away") then [FilterType.AwayOnly] else []
        let combinator = new Combinator()
        let league = combinator.createLeagueWithTeamsAndSort startDate endDate additionalFilters
        league