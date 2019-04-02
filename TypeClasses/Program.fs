open System
open FSharpPlus

type UserName = string
type DataResult<'t> = DataResult of 't with
    static member Map ( DataResult t  , f) = DataResult (f t)



type Cache() = 
    static member inline getOfCache cacheImpl data =
        ( ^T : (member getFromCache : 't -> DataResult<'t> option) (cacheImpl, data))
    static member inline storeOfCache cacheImpl data =
        ( ^T : (member storeToCache : 't -> unit) (cacheImpl, data))

type DataSource() =
    static member inline getOfSource dataSourceImpl data =
        ( ^T : (member getFromSource : 't -> DataResult<'t>) (dataSourceImpl, data))
    static member inline storeOfSource dataSourceImpl data =
        ( ^T : (member storeToSource : 't -> unit) (dataSourceImpl, data))

let inline requestData (cacheImpl: ^Cache) (dataSourceImpl: ^DataSource) (userName:UserName) = monad {
    match Cache.getOfCache cacheImpl userName with
    | Some dataResult -> 
            return! map ((+) "cache: ") dataResult
    | None -> 
            return! map ((+) "source: ") (DataSource.getOfSource dataSourceImpl userName) }


type Version = 
| NotInCache
| InCache

type CacheImpl (v:Version) = 
        member this.getFromCache user = 
            match v with
            | InCache -> monad { 
               return! DataResult user |> Some}
            | NotInCache -> None
        member this.storeCache _ = () 

       
type DataSourceImpl (v:Version) = 
          member this.getFromSource user =
            match v with
            | InCache -> 
              raise (NotImplementedException())  
            | NotInCache -> monad { 
               return! DataResult user }    
         
              
[<EntryPoint>]
let main argv =
    requestData (CacheImpl InCache) (DataSourceImpl InCache) "john" |> printfn "%A"
    requestData (CacheImpl NotInCache) (DataSourceImpl NotInCache) "john" |> printfn "%A"
    0 
