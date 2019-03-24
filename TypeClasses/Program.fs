open System
open FSharpPlus

type UserName = string
type DataResult<'t> = DataResult of 't with
    static member Map ( x:DataResult<'t>  , f) =
        match x with 
        | DataResult t -> DataResult (f t)

type Cache<'t> =
    abstract member getFromCache : 't -> DataResult<'t> option
    abstract member storeCache : 't -> unit

[<AbstractClass>] // in case you want default implementation
type DataSource<'t>() = 
    abstract member getFromSource : 't -> DataResult<'t>
    abstract member storeToSource : 't -> unit // just to show more options
    default this.storeToSource _ = ()

type Context<'t> = {cache: Cache<'t>; dataSource: DataSource<'t> }

let requestData (context:Context<UserName>) (userName:UserName) = monad {
    match context.cache.getFromCache userName with
    | Some dataResult -> return! map ((+) "cache: ") dataResult
    | None -> return! map ((+) "source: ") (context.dataSource.getFromSource userName) }

type Version = 
| NotInCache
| InCache

let cache = function
| NotInCache ->
    { new Cache<'t> with
        member this.getFromCache _ = None
        member this.storeCache _ = () }
 | InCache -> 
    { new Cache<'t> with
        member this.getFromCache user = monad { 
           return! DataResult user |> Some}
        member this.storeCache _ = () }

let dataSource = function
| NotInCache ->
    { new DataSource<'t>() with
          member this.getFromSource user = monad { 
               return! DataResult user } }
 | InCache -> 
    { new DataSource<'t>() with
          member this.getFromSource _  = 
              raise (NotImplementedException())}
              
[<EntryPoint>]
let main argv =
    let confWithCache cached = if cached then InCache else NotInCache
    let context conf = { cache = cache conf; dataSource = dataSource conf }
    let requestData' = flip requestData
    confWithCache true |> context |> requestData' "john" |> printfn "%A"
    confWithCache false |> context |> requestData' "john" |> printfn "%A"
    0 // return an integer exit code
