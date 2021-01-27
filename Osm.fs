module Osm

open FSharp.Data
open NetTopologySuite.Geometries
open System.Collections.Generic
open System

let capabilitiesUrl = "https://master.apis.dev.openstreetmap.org/api/capabilities"
let killUrl = "http://overpass-api.de/api/kill_my_queries"
let apiUrl = "https://overpass-api.de/api/interpreter"

type Body =
    | TextBody of string
    | OverpassQL

type Capabilities = {
    MinVersion: decimal
    MaxVersion: decimal
}

type Info = {
    Id: int64
    Uid: uint32
    Version: byte
    Changeset: uint32
    User: string
    Timestamp: DateTimeOffset
}

type Node = {
    Info: Info
    Geometry: Point
    Tags: IDictionary<string, string>
}

type Way = {
    Info: Info
    Geometry: option<LineString>
    Tags: IDictionary<string, string>
}

type Area = {
    Info: Info
    Geometry: option<Polygon>
    Tags: IDictionary<string, string>
}

type Relation = {
    Info: Info
}

type Response = {
    Nodes: seq<Node>
    Ways: seq<Way>
    Areas: seq<Area>
    Notes: string[]
}

module private Helpers =
    module Async =
        let mapAsync func calcAsync = async { 
            let! result = calcAsync
            return func result
        }

    module Geometry =

        let srid = 4326

        let geometryFactory = NetTopologySuite.NtsGeometryServices.Instance.CreateGeometryFactory(PrecisionModel 1000000.0, srid)

        let createCoordinate lon lat = new Coordinate(Decimal.ToDouble lon, Decimal.ToDouble lat)

        let createPoint lon lat = createCoordinate lon lat |> geometryFactory.CreatePoint

        let createLineString (points:seq<Point>) =
            points
                |> Seq.map(fun point -> point.Coordinate)
                |> Seq.toArray
                |> geometryFactory.CreateLineString 


module private Xml =
    module Capabilities =
        type XmlCapabilities = XmlProvider< Schema = const(__SOURCE_DIRECTORY__ + "/Schemas/XML/capabilities.xsd") >

        let fromXml str =
            let xml = XmlCapabilities.Parse str
            { MinVersion = xml.Api.Version.Minimum; MaxVersion = xml.Api.Version.Maximum }

    module Response =
        type XmlResponse = XmlProvider< Schema = const(__SOURCE_DIRECTORY__ + "/Schemas/XML/response.xsd") >

        let getNodeInfo (node:XmlResponse.Node) =
            { Id        = Convert.ToInt64(node.Id)
              Timestamp = node.Timestamp
              User      = node.User
              Version   = Convert.ToByte(node.Version)
              Changeset = Convert.ToUInt32(node.Changeset)
              Uid       = Convert.ToUInt32(node.Uid) }
        let getWayInfo (way:XmlResponse.Way) =
            { Id        = Convert.ToInt64(way.Id)
              Timestamp = way.Timestamp
              User      = way.User
              Version   = Convert.ToByte(way.Version)
              Changeset = Convert.ToUInt32(way.Changeset)
              Uid       = Convert.ToUInt32(way.Uid) }
        let geRelationtInfo (relation:XmlResponse.Relation) =
            { Id        = Convert.ToInt64(relation.Id)
              Timestamp = relation.Timestamp
              User      = relation.User
              Version   = Convert.ToByte(relation.Version)
              Changeset = Convert.ToUInt32(relation.Changeset)
              Uid       = Convert.ToUInt32(relation.Uid) }

        let getTags (tags:XmlResponse.Tag[]) =
            tags
                |> Seq.map(fun tag -> tag.K, tag.V)
                |> dict

        let getNode (node:XmlResponse.Node) =
            { Info          = getNodeInfo node
              Tags          = getTags node.Tags
              Node.Geometry = Helpers.Geometry.createPoint node.Lon node.Lat }

        let isTagged (node:Node) = node.Tags |> Seq.isEmpty |> not

        let getWay (way:XmlResponse.Way) (nodes:IDictionary<int64, Node>) =
            { Info          = getWayInfo way
              Tags          = getTags way.Tags
              Way.Geometry  =
                try
                    way.Nds
                        |> Seq.map(fun nd -> nodes.[Convert.ToInt64 nd.Ref].Geometry)
                        |> Helpers.Geometry.createLineString
                        |> Some
                with
                    | :? KeyNotFoundException -> None }

        let isOpen (way:Way) =
            match way.Geometry with
                | Some lineString -> not lineString.IsClosed
                | None            -> true

        let isClosed = not << isOpen

        let isArea (way:Way) =
            match way.Tags.TryGetValue "area" with
                | true, value -> value = "yes"
                | _           -> false

        let createAreaFromWay (way:Way) =
            { Info          = way.Info;
              Tags          = new Dictionary<string, string>(way.Tags);
              Area.Geometry =
                match way.Geometry with
                    | Some lineString -> if (isClosed way) then lineString.Coordinates |> Helpers.Geometry.geometryFactory.CreatePolygon |> Some else None
                    | None            -> None }

        let fromXml str =
            let xml = XmlResponse.Parse str
            let nodes = xml.Nodes |> Seq.map(getNode)
            let nodesDict = nodes |> Seq.map(fun node -> node.Info.Id, node) |> dict
            let ways = xml.Ways
                     |> Seq.map(fun way -> getWay way nodesDict)
            { Notes = xml.Notes
              Nodes = nodes
                    |> Seq.filter(isTagged)
              Ways  = ways
                    |> Seq.filter(isOpen)
              Areas = ways
                    |> Seq.filter(isArea)
                    |> Seq.map(createAreaFromWay) }

let AsyncGetCapabilities = Helpers.Async.mapAsync
                               Xml.Capabilities.fromXml
                                   <| Http.AsyncRequestString capabilitiesUrl

let GetCapabilities = Async.RunSynchronously AsyncGetCapabilities

let AsyncKillQueries = Http.AsyncRequest killUrl

let KillQueries = Async.RunSynchronously AsyncKillQueries 

let AsyncQuery body = Helpers.Async.mapAsync
                          Xml.Response.fromXml
                              <| Http.AsyncRequestString(apiUrl, body=HttpRequestBody.TextRequest body, httpMethod=HttpMethod.Post)

let Query body = body
                     |> AsyncQuery
                     |> Async.RunSynchronously