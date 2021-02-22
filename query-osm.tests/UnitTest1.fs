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
    async { let! capabilities = Osm.asyncGetCapabilities()
            Assert.That(capabilities.MinVersion, Is.EqualTo(0.6))
            Assert.That(capabilities.MaxVersion, Is.EqualTo(0.6))
    } |> Async.RunSynchronously

[<Test>]
let TestAsyncResponseExists () =
    async { let! res = Osm.asyncQuery requestBody
            res.entities |> Seq.head |> ignore
            res.relations |> Seq.head |> ignore
    } |> Async.RunSynchronously

[<Test>]
let TestFileRead () =
    let res = Osm.readFile "C:\\Users\\wallace\\Documents\\bluemont_osm.xml"
    let ways = res.entities |> Seq.filter Osm.isWay
    let home = res.entities |> Seq.filter(Osm.hasTagValue("1192"))
    ways |> ignore