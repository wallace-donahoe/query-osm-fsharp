module query_osm.tests

open NUnit.Framework

let requestBody = "(
node(51.249,7.148,51.251,7.152);
<;
);
out meta;"

[<SetUp>]
let Setup () =
    ()

[<Test>]
let TestAsyncCapabilities () =
    async { let! capabilities = Osm.AsyncGetCapabilities
            Assert.That(capabilities.MinVersion, Is.EqualTo(0.6))
            Assert.That(capabilities.MaxVersion, Is.EqualTo(0.6))
    } |> Async.RunSynchronously

[<Test>]
let TestAsyncResponseExists () =
    async { let! res = Osm.AsyncQuery requestBody
            res.Nodes |> Seq.head |> ignore
            res.Ways |> Seq.head |> ignore
            res.Relations |> Seq.head |> ignore
    } |> Async.RunSynchronously
