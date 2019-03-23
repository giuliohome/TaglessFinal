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

type Context = {cache: Cache; dataSource: DataSource }

let requestData (context:Context) (userName:UserName) = 
    match context.cache.getFromCache userName with
    | Some dataResult -> dataResult
    | None -> context.dataSource.getFromSource userName

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

let flip f a b = f b a
              
[<EntryPoint>]
let main argv =
    let confWithCache cached = if cached then InCache else NotInCache
    let context conf = { cache = cache conf; dataSource = dataSource conf }
    let requestData' = flip requestData
    confWithCache true |> context |> requestData' "john" |> printfn "%A"
    confWithCache false |> context |> requestData' "john" |> printfn "%A"
    0 // return an integer exit code
