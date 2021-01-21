open FSharp.Data
open System

let requestBody = "(
node(51.249,7.148,51.251,7.152);
<;
);
out meta;"

let url = "http://overpass-api.de/api/interpreter"

[<EntryPoint>]
let main _ =
    async { let! res = Http.AsyncRequestString(url, body=TextRequest requestBody, httpMethod=HttpMethod.Post)
            Console.Write res }
    |> Async.Start

    Console.ReadKey true |> ignore

    0
