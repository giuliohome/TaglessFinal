open System

open System

type UserName = string

type DataResult = DataResult of string

type Cache =
    abstract member getFromCache : string -> DataResult option
    abstract member storeCache : string -> unit

type DataSource =
    abstract member getFromSource : string -> DataResult

let requestDate (cacheImpl:Cache) (dataSourceimpl:DataSource) (userName:UserName) = 
    match cacheImpl.getFromCache userName with
    | Some dataResult -> dataResult
    | None -> dataSourceimpl.getFromSource userName

let CacheNotInCache =
    { new Cache with
        member this.getFromCache _ = None
        member this.storeCache _ = () }

let DataSourceNotInCache = 
    { new DataSource with
          member this.getFromSource user = 
              "source: " + user |> DataResult}

let CacheInCache =
    { new Cache with
        member this.getFromCache user = 
           "cache: " + user |> DataResult |> Some
        member this.storeCache _ = () }

let DataSourceInCache = 
    { new DataSource with
          member this.getFromSource _  = 
              raise (NotImplementedException())}
              
[<EntryPoint>]
let main argv =
    printfn "%A" <| requestDate CacheNotInCache DataSourceNotInCache "john" 
    printfn "%A" <| requestDate CacheInCache DataSourceInCache "john" 
    0 // return an integer exit code
