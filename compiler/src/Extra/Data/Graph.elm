module Extra.Data.Graph exposing
    ( SCC(..)
    , stronglyConnComp
    )

-- Parts ported from https://hackage.haskell.org/package/containers-0.7/docs/Data-Graph.html

import Extra.Type.List as MList exposing (TList)
import Extra.Type.Map as Map exposing (Map)
import Extra.Type.Set as Set exposing (Set)



-- STRONGLY CONNECTED COMPONENTS


type SCC vertex
    = AcyclicSCC vertex
    | CyclicSCC (TList vertex)


stronglyConnComp : TList ( node, comparable, TList comparable ) -> TList (SCC node)
stronglyConnComp edges0 =
    let
        get_node scc =
            case scc of
                AcyclicSCC ( n, _, _ ) ->
                    AcyclicSCC n

                CyclicSCC triples ->
                    CyclicSCC (MList.map (\( n, _, _ ) -> n) triples)
    in
    MList.map get_node (stronglyConnCompR edges0)


stronglyConnCompR :
    TList ( node, comparable, TList comparable )
    -- ^ The graph: a list of nodes uniquely identified by keys,
    -- with a list of keys of nodes this node has edges to.
    -- The out-list may contain keys that don't correspond to
    -- nodes of the graph; such edges are ignored.
    -> TList (SCC ( node, comparable, TList comparable )) -- ^ Reverse topologically sorted
stronglyConnCompR edges0 =
    let
        lt : ( node, comparable, TList comparable ) -> ( node, comparable, TList comparable ) -> Order
        lt ( _, v, _ ) ( _, w, _ ) =
            compare v w

        sorted : TList ( Int, ( node, comparable, TList comparable ) )
        sorted =
            MList.sortBy lt edges0 |> MList.indexedFrom 0

        keyMap : Map comparable Vertex
        keyMap =
            MList.foldl
                (\acc ( i, ( _, k, _ ) ) -> Map.insert k i acc)
                Map.empty
                sorted

        nodeMap : Map Vertex ( node, comparable, TList comparable )
        nodeMap =
            Map.fromList sorted

        getNode : Vertex -> ( node, comparable, TList comparable )
        getNode vertex =
            Map.ex nodeMap vertex

        edges : TList ( Vertex, TList Vertex )
        edges =
            MList.mapMaybe
                (\( _, from, neighbors ) ->
                    Map.lookup from keyMap
                        |> Maybe.map
                            (\vertex ->
                                ( vertex, MList.mapMaybe (\to -> Map.lookup to keyMap) neighbors )
                            )
                )
                edges0

        selfRefs : Set Vertex
        selfRefs =
            MList.foldl
                (\acc ( from, neighbors ) ->
                    if MList.elem from neighbors then
                        Set.insert from acc

                    else
                        acc
                )
                Set.empty
                edges

        graph : Graph
        graph =
            Map.fromList edges

        components : TList (TList Vertex)
        components =
            sccs graph

        toSCC : TList Vertex -> SCC ( node, comparable, TList comparable )
        toSCC component =
            case component of
                [ vertex ] ->
                    if Set.member vertex selfRefs then
                        CyclicSCC [ getNode vertex ]

                    else
                        AcyclicSCC (getNode vertex)

                _ ->
                    CyclicSCC (MList.map getNode component)
    in
    MList.map toSCC components



-- KOSARAJU'S ALGORITHM


type alias Vertex =
    Int


type alias Graph =
    Map Vertex (TList Vertex)


type VisitTask
    = CheckVisited Vertex
    | Output Vertex


sccs : Graph -> TList (TList Vertex)
sccs graph =
    assign (reverse graph) (visit graph)


visit : Graph -> TList Vertex
visit graph =
    let
        check : TList Vertex -> TList VisitTask
        check vertices =
            MList.map CheckVisited vertices

        go : TList VisitTask -> Set Vertex -> TList Vertex -> ( Set Vertex, TList Vertex )
        go tasks visited output =
            case tasks of
                [] ->
                    ( visited, output )

                (CheckVisited vertex) :: rest ->
                    if Set.member vertex visited then
                        go rest visited output

                    else
                        let
                            neighbors =
                                Map.lookup vertex graph |> Maybe.withDefault []

                            newTasks =
                                check neighbors ++ (Output vertex :: rest)

                            newVisited =
                                Set.insert vertex visited
                        in
                        go newTasks newVisited output

                (Output vertex) :: rest ->
                    go rest visited (vertex :: output)
    in
    go (check (Map.keys graph)) Set.empty [] |> Tuple.second


reverse : Graph -> Graph
reverse originalGraph =
    let
        addReversedEdge : Vertex -> Vertex -> Graph -> Graph
        addReversedEdge from to graph =
            Map.alter (\neighbors -> Just (from :: Maybe.withDefault [] neighbors)) to graph

        addReversedEdges : Graph -> Vertex -> TList Vertex -> Graph
        addReversedEdges graph from tos =
            MList.foldl (\acc to -> addReversedEdge from to acc) graph tos

        addAllReversedEdges : Graph -> Graph
        addAllReversedEdges graph =
            Map.foldlWithKey addReversedEdges Map.empty graph
    in
    addAllReversedEdges originalGraph


type AssignTask
    = CheckAssigned Vertex
    | FinishComponent


assign : Graph -> TList Vertex -> TList (TList Vertex)
assign graph orderedVertices =
    let
        check : TList Vertex -> TList AssignTask
        check vertices =
            MList.map CheckAssigned vertices

        go : TList AssignTask -> Set Vertex -> TList Vertex -> TList (TList Vertex) -> ( Set Vertex, TList Vertex, TList (TList Vertex) )
        go tasks assigned scc output =
            case tasks of
                [] ->
                    ( assigned, scc, output )

                (CheckAssigned vertex) :: rest ->
                    if Set.member vertex assigned then
                        go rest assigned scc output

                    else
                        let
                            neighborTasks =
                                Map.lookup vertex graph |> Maybe.withDefault [] |> check

                            finishTasks =
                                case scc of
                                    [] ->
                                        [ FinishComponent ]

                                    _ ->
                                        []

                            newTasks =
                                neighborTasks ++ finishTasks ++ rest

                            newAssigned =
                                Set.insert vertex assigned

                            newScc =
                                vertex :: scc
                        in
                        go newTasks newAssigned newScc output

                FinishComponent :: rest ->
                    go rest assigned [] (scc :: output)
    in
    go (check orderedVertices) Set.empty [] [] |> (\( _, _, output ) -> output)
