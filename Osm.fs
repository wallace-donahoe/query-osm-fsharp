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

type info = {
    id: int64
    Uid: uint32
    Version: byte
    Changeset: uint32
    User: string
    Timestamp: DateTimeOffset
}

[<Interface>]
type IEntity =
    abstract member id: int64 with get
    abstract member tags: IDictionary<string, string> with get

[<Interface>]
type IGeographic =
    abstract member geometry : option<Geometry> with get

type Complete = Complete | Incomplete

[<StructuredFormatDisplay("Node(id={id} {tags})")>]
type Node =
    { info: info
      geometry: option<Geometry>
      tags: IDictionary<string, string>
} with
    member this.id = (this :> IEntity).id
    interface IEntity with
        member this.id
            with get () = this.info.id
        member this.tags
            with get () = this.tags

[<StructuredFormatDisplay("Way(id={id} {tags})")>]
type Way =
    { info: info
      geometry : option<Geometry>
      tags: IDictionary<string, string>
      isComplete: bool
} with
    member this.id = (this :> IEntity).id
    interface IEntity with
        member this.id
            with get () = this.info.id
        member this.tags
            with get () = this.tags

type Entity =
            | Node of Node
            | Way of Way
            with
                member this.geometry = (this :> IGeographic).geometry
                member this.id = (this :> IEntity).id
                member this.tags = (this :> IEntity).tags
                interface IGeographic with
                    member this.geometry
                        with get () =
                            match this with
                                | Node node -> node.geometry
                                | Way  way  -> way.geometry
                interface IEntity with
                    member this.id
                        with get() =
                            match this with
                                | Node node -> node.info.id
                                | Way way   -> way.info.id
                    member this.tags
                        with get() =
                            match this with
                               | Node node -> node.tags
                               | Way way   -> way.tags

[<StructuredFormatDisplay("{entity}")>]
type Member = {
    role: option<string>
    entity: Entity
} with
    member this.Geometry = (this :> IGeographic).geometry
    member this.id = (this :> IEntity).id
    member this.tags = (this :> IEntity).tags
    interface IEntity with
        member this.id
            with get () = this.entity.id
        member this.tags
            with get () = (this.entity :> IEntity).tags
    interface IGeographic with
        member this.geometry
            with get() = this.entity.geometry

[<StructuredFormatDisplay("Relation(id={info.id} {tags})")>]
type Relation = {
    info: info
    tags: IDictionary<string, string>
    members: seq<Member>
    relations: seq<Relation>
} with
   member this.id = (this :> IEntity).id
   interface IEntity with
       member this.id
           with get () = this.info.id
       member this.tags
           with get () = this.tags

type Response = {
    entities: seq<Entity>
    relations: seq<Relation>
    notes: string[]
}

module Helpers =
    module Async =
        let mapAsync func calcAsync = async { 
            let! result = calcAsync
            return func result
        }

    let chunkList (pred:('a -> bool)) (items:list<'a>) =
        let rec loop xs =
            let skipped = List.skipWhile (not << pred) xs
            [
                yield List.takeWhile pred skipped
                match skipped with
                    | [] -> ()
                    | _ -> yield! loop (List.skipWhile pred skipped)
            ]
        loop items

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

        let getNodeinfo (node:XmlResponse.Node) =
            { id        = Convert.ToInt64(node.Id)
              Timestamp = node   .Timestamp
              User      = node   .User
              Version   = Convert.ToByte(node.Version)
              Changeset = Convert.ToUInt32(node.Changeset)
              Uid       = Convert.ToUInt32(node.Uid) }
        let getWayinfo (way:XmlResponse.Way) =
            { id        = Convert.ToInt64(way.Id)
              Timestamp = way    .Timestamp
              User      = way    .User
              Version   = Convert.ToByte(way.Version)
              Changeset = Convert.ToUInt32(way.Changeset)
              Uid       = Convert.ToUInt32(way.Uid) }
        let getRelationinfo (relation:XmlResponse.Relation) =
            { id        = Convert .ToInt64(relation.Id)
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
            { info          = getNodeinfo node
              tags          = getTags node.Tags
              Node.geometry = Helpers.Geometry.createPoint node.Lon node.Lat :> Geometry |> Some }

        let tryGetGeometry (nodes:IDictionary<int64, Entity>) (nd:XmlResponse.Nd) =
            match nd.Ref |> Convert.ToInt64 |> nodes.TryGetValue with
                | true, Node node -> node.geometry |> Option.get :?> Point |> Some
                | _               -> None

        let getChunkGeometry (chunk:list<option<Point>>) =
            match chunk with
                | [] -> None
                | point::[] -> Helpers.Geometry.someGeometry point.Value
                | xs -> Helpers.Geometry.createLineString (List.map Option.get xs) |> Helpers.Geometry.someGeometry

        let getSingleGeometryOrCollection geometries =
            match geometries with
                | [|x|] -> Helpers.Geometry.someGeometry x
                |   _   -> geometries |> GeometryCollection |> Helpers.Geometry.someGeometry

        let getIncompleteWayGeometry (points:list<option<Point>>) =
            points
                |> Helpers.chunkList Option.isSome
                |> Seq.map getChunkGeometry
                |> Seq.filter Option.isSome
                |> Seq.map Option.get
                |> Seq.toArray
                |> getSingleGeometryOrCollection

        let getGeometryCollectionOrLineString (points:list<option<Point>>) = 
            if (List.contains None points) then
                (Incomplete, getIncompleteWayGeometry points)
            else
                (Complete, points
                             |> Seq.map Option.get
                             |> Helpers.Geometry.createLineString
                             |> Helpers.Geometry.someGeometry)

        let getWayGeometry (nodes:IDictionary<int64, Entity>) (way:XmlResponse.Way) =
            let points = way.Nds
                       |> Seq.map (tryGetGeometry nodes)
                       |> Seq.toList
            
            match getGeometryCollectionOrLineString points with
                | (Complete, geometry) -> (Complete, if (not (geometry.Value :?> LineString).IsClosed) then
                                                        geometry
                                                     else
                                                        geometry.Value.Coordinates
                                                            |> Helpers.Geometry.geometryFactory.CreatePolygon 
                                                            |> Helpers.Geometry.someGeometry)
                | res -> res

        let getWay (nodes:IDictionary<int64, Entity>) (way:XmlResponse.Way) =
            let completeness, geometry = getWayGeometry nodes way
            { info       = getWayinfo way
              tags       = getTags way.Tags
              geometry   = geometry
              isComplete = completeness = Complete }

        let isMemberType value (mem:XmlResponse.Member) = mem.Type = value

        let getRelationMember (entityDict:IDictionary<int64, Entity>) (mem:XmlResponse.Member) =
            match entityDict.TryGetValue (Convert.ToInt64 mem.Ref) with
                | true, value -> Some { role = mem.Role; entity = value }
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

            { info = getRelationinfo relation
              tags = getTags relation.Tags
              members = Seq.append nodes ways
                      |> Seq.choose id
                      |> Seq.sortBy (fun mem -> mem.id)
              relations = relation.Members
                        |> Seq.filter (isMemberType "relation")
                        |> Seq.map (getNestedRelationXml relationsDict)
                        |> Seq.choose (getRelation entityDict relationsDict |> Option.bind)
                        |> Seq.sortBy (fun rel -> rel.id) } |> Some

        let tup ctor (entity: 'a when 'a :> IEntity) = (entity.id, ctor entity)

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

            { notes = xml.Notes
              entities = entityDict
                       |> Map.values
                       |> Seq.sortBy (fun entity -> entity.id)
              relations = xml.Relations
                        |> Seq.choose (getRelation entityDict relationsDict) }

let asyncGetCapabilities () = Helpers.Async.mapAsync
                                 Xml.Capabilities.fromXml
                                   <| Http.AsyncRequestString capabilitiesUrl

let getCapabilities () = asyncGetCapabilities() |> Async.RunSynchronously

let asyncKillQueries () = Http.AsyncRequest killUrl

let killQueries () = asyncKillQueries() |> Async.RunSynchronously

let asyncQuery body = Helpers.Async.mapAsync
                          (Xml.Response.XmlResponse.Parse >> Xml.Response.fromXml)
                              <| Http.AsyncRequestString(apiUrl, body=HttpRequestBody.TextRequest body, httpMethod=HttpMethod.Post)

let query body = body
                     |> asyncQuery
                     |> Async.RunSynchronously

let readFile path = path
                        |> IO.File.ReadAllText
                        |> (Xml.Response.XmlResponse.Parse >> Xml.Response.fromXml)

let isWay entity =
    match entity with
        | Way _ -> true
        | _ -> false
let isNode = not << isWay

let hasTagValue tagVal (entity:Entity) = entity.tags.Values.Contains tagVal
let hasTag (key, value) (entity:Entity) = entity.tags.Contains(KeyValuePair(key, value))  