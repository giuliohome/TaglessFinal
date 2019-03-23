open System

open System

type UserName = string

type DataResult = DataResult of string

type Cache =
    abstract member getFromCache : string -> DataResult option
    abstract member storeCache : string -> unit

[<AbstractClass>] // in case you want default implementation
type DataSource() = 
    abstract member getFromSource : string -> DataResult
    abstract member storeToSource : string -> unit // just to show more options
    default this.storeToSource _ = ()

let requestData (cacheImpl:Cache) (dataSourceimpl:DataSource) (userName:UserName) = 
    match cacheImpl.getFromCache userName with
    | Some dataResult -> dataResult
    | None -> dataSourceimpl.getFromSource userName

type Version = 
| NotInCache
| InCache

let cache = function
| NotInCache ->
    { new Cache with
        member this.getFromCache _ = None
        member this.storeCache _ = () }
 | InCache -> 
    { new Cache with
        member this.getFromCache user = 
           "cache: " + user |> DataResult |> Some
        member this.storeCache _ = () }

let dataSource = function
| NotInCache ->
    { new DataSource() with
          member this.getFromSource user = 
              "source: " + user |> DataResult}
 | InCache -> 
    { new DataSource() with
          member this.getFromSource _  = 
              raise (NotImplementedException())}
              
[<EntryPoint>]
let main argv =
    printfn "%A" <| requestData (cache NotInCache) (dataSource NotInCache) "john" 
    printfn "%A" <| requestData (cache InCache) (dataSource InCache) "john" 
    0 // return an integer exit code
