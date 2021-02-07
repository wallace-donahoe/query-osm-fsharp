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

type IEntity =
    abstract member Id: int64 with get

[<Interface>]
type IGeographic =
    abstract member Geometry : option<Geometry> with get

type Node =
    { Info: Info
      Point: Point
      Tags: IDictionary<string, string>
    }

type Way =
    { Info: Info
      LineString: option<LineString>
      Tags: IDictionary<string, string>
    }

type Area =
    { Info: Info
      Polygon: option<Polygon>
      Tags: IDictionary<string, string>
    }


type Entity =
            | Node of Node
            | Way of Way
            | Area of Area
            with
                member this.Geometry = (this :> IGeographic).Geometry
                member this.Id = (this :> IEntity).Id
                interface IGeographic with
                    member this.Geometry
                        with get () =
                            match this with
                                | Node node -> node.Point :> Geometry |> Some
                                | Way  way  -> Option.map (fun lineString -> lineString :> Geometry) way.LineString
                                | Area area -> Option.map (fun polygon    -> polygon :> Geometry) area.Polygon
                interface IEntity with
                    member this.Id
                        with get() =
                            match this with
                                | Node node -> node.Info.Id
                                | Way way -> way.Info.Id
                                | Area area -> area.Info.Id

type RelationMember = {
    Role: option<string>
    Member: Entity
}

type Relation = {
    Info: Info
    Tags: IDictionary<string, string>
    Members: seq<RelationMember>
    Relations: seq<Relation>
}

type Response = {
    Nodes: seq<Node>
    Ways: seq<Way>
    Areas: seq<Area>
    Relations: seq<Relation>
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
              Timestamp = node   .Timestamp
              User      = node   .User
              Version   = Convert.ToByte(node.Version)
              Changeset = Convert.ToUInt32(node.Changeset)
              Uid       = Convert.ToUInt32(node.Uid) }
        let getWayInfo (way:XmlResponse.Way) =
            { Id        = Convert.ToInt64(way.Id)
              Timestamp = way    .Timestamp
              User      = way    .User
              Version   = Convert.ToByte(way.Version)
              Changeset = Convert.ToUInt32(way.Changeset)
              Uid       = Convert.ToUInt32(way.Uid) }
        let getRelationInfo (relation:XmlResponse.Relation) =
            { Id        = Convert .ToInt64(relation.Id)
              Timestamp = relation.Timestamp
              User      = relation.User
              Version   = Convert .ToByte(relation.Version)
              Changeset = Convert .ToUInt32(relation.Changeset)
              Uid       = Convert .ToUInt32(relation.Uid) }

        let getTags (tags:XmlResponse.Tag[]) =
            tags
                |> Seq.map(fun tag -> tag.K, tag.V)
                |> dict

        let getNode (node:XmlResponse.Node) =
            { Info       = getNodeInfo node
              Tags       = getTags node.Tags
              Node.Point = Helpers.Geometry.createPoint node.Lon node.Lat }

        let isTagged (node:Node) = node.Tags |> Seq.isEmpty |> not

        let getWay (way:XmlResponse.Way) (nodes:IDictionary<int64, Node>) =
            { Info          = getWayInfo way
              Tags          = getTags way.Tags
              Way.LineString  =
                try
                    way.Nds
                        |> Seq.map(fun nd -> nodes.[Convert.ToInt64 nd.Ref].Point)
                        |> Helpers.Geometry.createLineString
                        |> Some
                with
                    | :? KeyNotFoundException -> None }

        let isOpen (way:Way) =
            match way.LineString with
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
              Area.Polygon =
                match way.LineString with
                    | Some lineString -> if (isClosed way)
                                         then lineString.Coordinates |> Helpers.Geometry.geometryFactory.CreatePolygon |> Some
                                         else None
                    | None            -> None }

        let isNodeMember (mem:XmlResponse.Member) = mem.Type = "node"

        let getNodeMember (mem:XmlResponse.Member) (nodeDict:IDictionary<int64, Node>) =
            match nodeDict.TryGetValue (Convert.ToInt64 mem.Ref) with
                | true, value -> Some { Role = mem.Role; Member = Node value }
                | _           -> None
        
        let isWayMember (mem:XmlResponse.Member) = mem.Type = "way"

        let getWayMember (mem:XmlResponse.Member) (wayDict:IDictionary<int64, Way>) =
            match wayDict.TryGetValue (Convert.ToInt64 mem.Ref) with
                | true, value -> Some { Role = mem.Role; Member = Way value }
                | _           -> None

        let getRelation (relation:XmlResponse.Relation) nodesDict waysDict =
            let locGetNode n = getNodeMember n nodesDict
            let locGetWay w = getWayMember w waysDict
            let nodes = relation.Members
                      |> Seq.filter isNodeMember
                      |> Seq.map locGetNode
            let ways = relation.Members
                      |> Seq.filter isWayMember
                      |> Seq.map locGetWay

            { Info = getRelationInfo relation
              Tags = getTags relation.Tags
              Members = Seq.append nodes ways
                      |> Seq.filter(fun w -> w.IsSome)
                      |> Seq.map(Option.get)
                      |> Seq.sortBy(fun mem -> mem.Member.Id)
              Relations = Seq.empty<Relation> }

        let fromXml str =
            let xml = XmlResponse.Parse str
            let nodes = xml.Nodes |> Seq.map getNode
            let nodesDict = nodes |> Seq.map(fun node -> node.Info.Id, node) |> dict
            let ways = xml.Ways |> Seq.map(fun way -> getWay way nodesDict)
            let waysDict = ways |> Seq.map(fun way -> way.Info.Id, way) |> dict
            { Notes = xml.Notes
              Nodes = nodes
                    |> Seq.filter isTagged
              Ways  = ways
                    |> Seq.filter isOpen
              Areas = ways
                    |> Seq.filter isArea
                    |> Seq.map createAreaFromWay
              Relations = xml.Relations
                        |> Seq.map(fun m -> getRelation m nodesDict waysDict) }

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