module Osm

open FSharp.Data
open FSharpx.Collections
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

[<Interface>]
type IEntity =
    abstract member Id: int64 with get
    abstract member Tags: IDictionary<string, string> with get

[<Interface>]
type IGeographic =
    abstract member Geometry : option<Geometry> with get

type Node =
    { Info: Info
      Point: Point
      Tags: IDictionary<string, string>
    } with
        member this.Geometry = (this :> IGeographic).Geometry
        member this.Id = (this :> IEntity).Id
        interface IEntity with
            member this.Id
                with get () = this.Info.Id
            member this.Tags
                with get () = this.Tags
        interface IGeographic with
            member this.Geometry
                with get () = this.Point :> Geometry |> Some

type Way =
    { Info: Info
      LineString: option<LineString>
      Tags: IDictionary<string, string>
    } with
        member this.Geometry = (this :> IGeographic).Geometry
        member this.Id = (this :> IEntity).Id
        interface IEntity with
            member this.Id
                with get () = this.Info.Id
            member this.Tags
                with get () = this.Tags
        interface IGeographic with
            member this.Geometry
                with get () = Option.map(fun lineString -> lineString :> Geometry) this.LineString

type Area =
    { Info: Info
      Polygon: option<Polygon>
      Tags: IDictionary<string, string>
    } with
        member this.Geometry = (this :> IGeographic).Geometry
        member this.Id = (this :> IEntity).Id
        interface IEntity with
            member this.Id
                with get () = this.Info.Id
            member this.Tags
                with get () = this.Tags
        interface IGeographic with
            member this.Geometry
                with get () = Option.map(fun polygon -> polygon :> Geometry) this.Polygon

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
                                | Node node -> node.Geometry
                                | Way  way  -> way.Geometry
                                | Area area -> area.Geometry
                interface IEntity with
                    member this.Id
                        with get() =
                            match this with
                                | Node node -> node.Id
                                | Way way   -> way.Id
                                | Area area -> area.Id
                    member this.Tags
                        with get() =
                            match this with
                               | Node node -> node.Tags
                               | Way way   -> way.Tags
                               | Area area -> area.Tags

type RelationMember = {
    Role: option<string>
    Member: Entity
} with
    member this.Geometry = (this :> IGeographic).Geometry
    member this.Id = (this :> IEntity).Id
    member this.Tags = (this :> IEntity).Tags
    interface IEntity with
        member this.Id
            with get () = this.Member.Id
        member this.Tags
            with get () = (this.Member :> IEntity).Tags
    interface IGeographic with
        member this.Geometry
            with get() = this.Member.Geometry

type Relation = {
    Info: Info
    Tags: IDictionary<string, string>
    Members: seq<RelationMember>
    Relations: seq<Relation>
} with
   member this.Id = (this :> IEntity).Id
   interface IEntity with
       member this.Id
           with get () = this.Info.Id
       member this.Tags
           with get () = this.Tags

type Response = {
    Entities: seq<Entity>
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

        let isTagged (entity: 'a when 'a :> IEntity) = entity.Tags |> Seq.isEmpty |> not

        let getWay (nodes:IDictionary<int64, Entity>) (way:XmlResponse.Way) =
            { Info          = getWayInfo way
              Tags          = getTags way.Tags
              Way.LineString  =
                try
                    way.Nds
                        |> Seq.map(fun nd -> Option.get nodes.[Convert.ToInt64 nd.Ref].Geometry :?> Point)
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

        let isMemberType value (mem:XmlResponse.Member) = mem.Type = value

        let getRelationMember (entityDict:IDictionary<int64, Entity>) (mem:XmlResponse.Member) =
            match entityDict.TryGetValue (Convert.ToInt64 mem.Ref) with
                | true, value -> Some { Role = mem.Role; Member = value }
                | _           -> None
        
        let getRelation (entityDict:IDictionary<int64, Entity>) (relation:XmlResponse.Relation) =
            let nodes = relation.Members
                      |> Seq.filter (isMemberType "node")
                      |> Seq.map (getRelationMember entityDict)
            let ways = relation.Members
                     |> Seq.filter (isMemberType "way")
                     |> Seq.map (getRelationMember entityDict)

            { Info = getRelationInfo relation
              Tags = getTags relation.Tags
              Members = Seq.append nodes ways
                      |> Seq.filter Option.isSome
                      |> Seq.map Option.get
                      |> Seq.sortBy (fun mem -> mem.Id)
              Relations = Seq.empty<Relation> }

        let getAreaOrWay way =
            match isOpen way with
                | true -> Way way
                | _    -> match isArea way with
                            | true -> way |> createAreaFromWay |> Area
                            | _    -> Way way

        let getEntity entity =
            match entity with
                | Way way -> getAreaOrWay way
                | _       -> entity

        let fromXml str =
            let tup ctor (entity: 'a when 'a :> IEntity) = (entity.Id, ctor entity)

            let xml = XmlResponse.Parse str
            let nodesMap = xml.Nodes
                         |> Seq.map (getNode >> tup Node)
                         |> Map
            let entityDict = xml.Ways
                           |> Seq.map (getWay nodesMap >> tup Way)
                           |> Map
                           |> Map.union nodesMap

            { Notes = xml.Notes
              Entities = entityDict
                       |> Map.values
                       |> Seq.map getEntity
                       |> Seq.filter isTagged
                       |> Seq.sortBy (fun entity -> entity.Id)
              Relations = xml.Relations
                        |> Seq.map (getRelation entityDict) }

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