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

type Complete = Complete | Incomplete

type Node =
    { Info: Info
      Geometry: option<Geometry>
      Tags: IDictionary<string, string>
} with
    member this.Id = (this :> IEntity).Id
    interface IEntity with
        member this.Id
            with get () = this.Info.Id
        member this.Tags
            with get () = this.Tags

type Way =
    { Info: Info
      Geometry : option<Geometry>
      Tags: IDictionary<string, string>
} with
    member this.Id = (this :> IEntity).Id
    interface IEntity with
        member this.Id
            with get () = this.Info.Id
        member this.Tags
            with get () = this.Tags

type Entity =
            | Node of Node
            | Way of Way
            with
                member this.Geometry = (this :> IGeographic).Geometry
                member this.Id = (this :> IEntity).Id
                interface IGeographic with
                    member this.Geometry
                        with get () =
                            match this with
                                | Node node -> node.Geometry
                                | Way  way  -> way.Geometry
                interface IEntity with
                    member this.Id
                        with get() =
                            match this with
                                | Node node -> node.Info.Id
                                | Way way   -> way.Info.Id
                    member this.Tags
                        with get() =
                            match this with
                               | Node node -> node.Tags
                               | Way way   -> way.Tags

type Member = {
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
    Members: seq<Member>
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

        let someGeometry (geometry:'a when 'a :> Geometry) = geometry :> Geometry |> Some

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

        let getKvp (tag:XmlResponse.Tag) = (tag.K, tag.V)

        let getTags (tags:XmlResponse.Tag[]) =
            tags
                |> Seq.map getKvp
                |> dict

        let getNode (node:XmlResponse.Node) =
            { Info          = getNodeInfo node
              Tags          = getTags node.Tags
              Node.Geometry = Helpers.Geometry.createPoint node.Lon node.Lat :> Geometry |> Some }

        let isTagged (entity:IEntity) = entity.Tags |> Seq.isEmpty |> not

        let tryGetGeometry (nodes:IDictionary<int64, Entity>) (nd:XmlResponse.Nd) =
            match nd.Ref |> Convert.ToInt64 |> nodes.TryGetValue with
                | true, Node node -> node.Geometry |> Option.get :?> Point |> Some
                | _               -> None

        let createGeometryCollection (points:list<option<Point>>) = Helpers.Geometry.createPoint 0m 0m :> Geometry |> Some
        
        let getGeometryCollectionOrLineString (points:list<option<Point>>) = 
            match List.contains None points with
                | true  -> (Incomplete, createGeometryCollection points)
                | false -> (Complete, points
                                        |> Seq.map Option.get
                                        |> Helpers.Geometry.createLineString
                                        |> Helpers.Geometry.someGeometry)

        let getWayGeometry (nodes:IDictionary<int64, Entity>) (way:XmlResponse.Way) =
            let points = way.Nds
                       |> Seq.map(tryGetGeometry nodes)
                       |> Seq.toList
            
            match getGeometryCollectionOrLineString points with
                | (Complete, geometry) -> (Complete, match (geometry.Value :?> LineString).IsClosed with
                                                        | false -> geometry
                                                        | true  -> geometry.Value.Coordinates
                                                                    |> Helpers.Geometry.geometryFactory.CreatePolygon 
                                                                    |> Helpers.Geometry.someGeometry)
                | res -> res

        let getWay (nodes:IDictionary<int64, Entity>) (way:XmlResponse.Way) =
            { Info          = getWayInfo way
              Tags          = getTags way.Tags
              Way.Geometry  = getWayGeometry nodes way |> snd}

        let isMemberType value (mem:XmlResponse.Member) = mem.Type = value

        let getRelationMember (entityDict:IDictionary<int64, Entity>) (mem:XmlResponse.Member) =
            match entityDict.TryGetValue (Convert.ToInt64 mem.Ref) with
                | true, value -> Some { Role = mem.Role; Member = value }
                | _           -> None
        
        let getNestedRelationXml (relationsDict:IDictionary<int64, XmlResponse.Relation>) (mem:XmlResponse.Member) =
            match relationsDict.TryGetValue (Convert.ToInt64 mem.Ref) with
                | true, value -> Some value
                | _           -> None
            
        let rec getRelation (entityDict:IDictionary<int64, Entity>) (relationsDict:IDictionary<int64, XmlResponse.Relation>) (relation:XmlResponse.Relation) =
            let nodes = relation.Members
                      |> Seq.filter (isMemberType "node")
                      |> Seq.map (getRelationMember entityDict)
            let ways = relation.Members
                     |> Seq.filter (isMemberType "way")
                     |> Seq.map (getRelationMember entityDict)

            { Info = getRelationInfo relation
              Tags = getTags relation.Tags
              Members = Seq.append nodes ways
                      |> Seq.choose id
                      |> Seq.sortBy (fun mem -> mem.Id)
              Relations = relation.Members
                        |> Seq.filter (isMemberType "relation")
                        |> Seq.map (getNestedRelationXml relationsDict)
                        |> Seq.choose (getRelation entityDict relationsDict |> Option.bind)
                        |> Seq.sortBy (fun rel -> rel.Id) } |> Some

        let tup ctor (entity: 'a when 'a :> IEntity) = (entity.Id, ctor entity)

        let fromXml (xml:XmlResponse.Osm) =
            let nodesMap = xml.Nodes
                         |> Seq.map (getNode >> tup Node)
                         |> Map
            let entityDict = xml.Ways
                           |> Seq.map (getWay nodesMap >> tup Way)
                           |> Map
                           |> Map.union nodesMap
            let relationsDict = xml.Relations
                              |> Seq.map (fun rel -> Convert.ToInt64 rel.Id, rel)
                              |> dict

            { Notes = xml.Notes
              Entities = entityDict
                       |> Map.values
                       |> Seq.filter isTagged
                       |> Seq.sortBy (fun entity -> entity.Id)
              Relations = xml.Relations
                        |> Seq.choose (getRelation entityDict relationsDict) }

let AsyncGetCapabilities = Helpers.Async.mapAsync
                               Xml.Capabilities.fromXml
                                   <| Http.AsyncRequestString capabilitiesUrl

let GetCapabilities = Async.RunSynchronously AsyncGetCapabilities

let AsyncKillQueries = Http.AsyncRequest killUrl

let KillQueries = Async.RunSynchronously AsyncKillQueries 

let AsyncQuery body = Helpers.Async.mapAsync
                          (Xml.Response.XmlResponse.Parse >> Xml.Response.fromXml)
                              <| Http.AsyncRequestString(apiUrl, body=HttpRequestBody.TextRequest body, httpMethod=HttpMethod.Post)

let Query body = body
                     |> AsyncQuery
                     |> Async.RunSynchronously