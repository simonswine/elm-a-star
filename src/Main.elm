module Main exposing (..)

import Time exposing (second)
import Html exposing (h1, div, button, p, Html, button)
import Html.Attributes as Attr
import Html.Events exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Color.Manipulate exposing (lighten)
import Color.Convert exposing (colorToHex)
import Color exposing (purple, green, rgb)
import Array
import Random.Array exposing (shuffle)
import Random
import Set


-- material design lite

import Material.Button as Button exposing (..)
import Material.Options as Options exposing (Style)
import Material
import Material.List as Lists
import Material.Chip as Chip
import Material.Color
import Material.Slider as Slider
import Material.Layout as Layout
import Material.Spinner as Loading


type alias Model =
    { n : Int
    , nodes : Array.Array NodeType
    , mdl : Material.Model
    , drawSize : Int
    , gScore : Array.Array Score
    , fScore : Array.Array Score
    , openSet : Set.Set Pos
    , closedSet : Set.Set Pos
    , cameFrom : Array.Array Pos
    , wallRatio : Float
    , state : State
    , tick : Float
    }


type alias Pos =
    Int


type alias N =
    Int


type State
    = Solved
    | Solving
    | Solvable
    | Unsolvable


type Score
    = Infinity
    | Int Int


type alias Coordinates =
    { x : Int, y : Int }


type Msg
    = Shuffle
    | Step
    | Solve
    | SolveUpdate Time.Time
    | Mdl (Material.Msg Msg)
    | NewNodes (Array.Array NodeType)
    | SliderN Float
    | SliderWallRatio Float
    | SliderTick Float


type NodeType
    = Start
    | End
    | Wall
    | Path


allNodeTypes : List NodeType
allNodeTypes =
    [ Start
    , End
    , Wall
    , Path
    ]


solveStep : Model -> Model
solveStep m =
    let
        endPos =
            Maybe.withDefault 0 (firstNodeType m.nodes End)

        current =
            currentNode m
    in
        case current of
            Nothing ->
                { m
                    | state = Unsolvable
                }

            Just current ->
                if current == endPos then
                    { m
                        | state = Solved
                    }
                else
                    let
                        neighbours =
                            Set.diff
                                (Set.fromList (walkableNeighboursOfPos m current))
                                m.closedSet

                        m2 =
                            solveStepNeighbours m (Set.toList neighbours)
                    in
                        { m2
                            | openSet =
                                Set.union
                                    (Set.remove current m.openSet)
                                    neighbours
                            , closedSet = Set.insert current m.closedSet
                        }


addScore : Score -> Score -> Score
addScore s1 s2 =
    case ( s1, s2 ) of
        ( Int x, Int y ) ->
            Int (x + y)

        _ ->
            Infinity


gtScore : Score -> Score -> Bool
gtScore s1 s2 =
    case ( s1, s2 ) of
        ( Int x, Int y ) ->
            x > y

        ( Infinity, _ ) ->
            True

        ( _, Infinity ) ->
            False


solveStepNeighbours : Model -> List Pos -> Model
solveStepNeighbours m neigh =
    case neigh of
        [] ->
            m

        n :: neigh ->
            let
                endPos =
                    Maybe.withDefault 0 (firstNodeType m.nodes End)

                current =
                    Maybe.withDefault 0 (currentNode m)

                gScore =
                    Array.get current m.gScore
                        |> Maybe.withDefault Infinity
                        |> addScore (Int 1)

                fScore =
                    distanceEstimate m.n n endPos
                        |> addScore gScore

                gScoreExisting =
                    Array.get n m.gScore |> Maybe.withDefault Infinity
            in
                solveStepNeighbours
                    (if (gtScore gScore gScoreExisting) then
                        m
                     else
                        { m
                            | cameFrom = Array.set n current m.cameFrom
                            , gScore = Array.set n gScore m.gScore
                            , fScore = Array.set n fScore m.fScore
                        }
                    )
                    neigh



-- reconstruct the path and return it from start to finish


reconstructPath : Model -> List Pos
reconstructPath m =
    case m.state of
        Solved ->
            let
                posStart =
                    (firstNodeType m.nodes Start)

                posEnd =
                    (firstNodeType m.nodes End)
            in
                case ( posStart, posEnd ) of
                    ( Just posStart, Just posEnd ) ->
                        reconstructPathH posStart posEnd m.cameFrom []

                    _ ->
                        []

        _ ->
            []


reconstructPathH : Pos -> Pos -> Array.Array Pos -> List Pos -> List Pos
reconstructPathH start end cameFrom list =
    if start == end then
        (end :: list)
    else
        case (Array.get end cameFrom) of
            Just newEnd ->
                reconstructPathH start newEnd cameFrom (end :: list)

            _ ->
                []



-- the node in openSet having the lowest fScore[] value


currentNode : Model -> Maybe Pos
currentNode m =
    List.filterMap
        (\x ->
            let
                score =
                    Array.get x m.fScore
            in
                case score of
                    Nothing ->
                        Nothing

                    Just Infinity ->
                        Nothing

                    Just (Int score) ->
                        Just ( x, score )
        )
        (Set.toList m.openSet)
        |> List.sortBy Tuple.second
        |> List.head
        |> Maybe.map Tuple.first


numberOfWalls : N -> Float -> Int
numberOfWalls n ratio =
    round (toFloat ((n ^ 2) - 2) * ratio)


minPerNodeType : Model -> NodeType -> Int
minPerNodeType m t =
    case t of
        Start ->
            1

        End ->
            1

        Wall ->
            numberOfWalls m.n m.wallRatio

        Path ->
            (m.n ^ 2) - 2 - (numberOfWalls m.n m.wallRatio)


distanceEstimate : N -> Pos -> Pos -> Score
distanceEstimate n pos1 pos2 =
    let
        c1 =
            posToCoordinates n pos1

        c2 =
            posToCoordinates n pos2
    in
        case c1 of
            Just c1 ->
                case c2 of
                    Just c2 ->
                        Int (abs (c1.x - c2.x) + abs (c1.y - c2.y))

                    Nothing ->
                        Infinity

            Nothing ->
                Infinity


firstNodeTypeIL : List ( Int, NodeType ) -> NodeType -> Maybe Int
firstNodeTypeIL nodes nodeType =
    case nodes of
        [] ->
            Nothing

        ( pos, ntype ) :: ril ->
            if ntype == nodeType then
                Just pos
            else
                firstNodeTypeIL ril nodeType


firstNodeType : Array.Array NodeType -> NodeType -> Maybe Int
firstNodeType nodes nodeType =
    firstNodeTypeIL (Array.toIndexedList nodes) nodeType


colourPerPos : Model -> Pos -> Color.Color
colourPerPos m n =
    let
        nodeType =
            Maybe.withDefault Wall (Array.get n m.nodes)

        colour =
            colourPerNodeType nodeType
    in
        case ( nodeType, m.state ) of
            ( Start, _ ) ->
                colour

            ( End, _ ) ->
                colour

            ( _, Solved ) ->
                let
                    solution =
                        Set.fromList (reconstructPath m)
                in
                    if Set.member n solution then
                        colour
                    else
                        lighten 0.3 colour

            ( _, Unsolvable ) ->
                lighten 0.3 colour

            _ ->
                colour


colourPerNodeType : NodeType -> Color.Color
colourPerNodeType t =
    case t of
        Start ->
            rgb 96 181 204

        End ->
            rgb 240 173 0

        Wall ->
            rgb 100 100 100

        Path ->
            rgb 127 209 59


textPerPos : Model -> Pos -> String
textPerPos m n =
    let
        nodeType =
            Maybe.withDefault Wall (Array.get n m.nodes)

        inClosedSet =
            Set.member n m.closedSet

        current =
            Maybe.withDefault False (Maybe.map (\x -> x == n) (currentNode m))
    in
        case ( nodeType, inClosedSet, current ) of
            ( Start, _, _ ) ->
                textPerNodeType nodeType

            ( End, _, _ ) ->
                textPerNodeType nodeType

            ( _, _, True ) ->
                "⁕"

            ( _, True, _ ) ->
                "•"

            _ ->
                textPerNodeType nodeType


textPerNodeType : NodeType -> String
textPerNodeType t =
    case t of
        Start ->
            "S"

        End ->
            "E"

        _ ->
            ""


posToCoordinates : N -> Pos -> Maybe Coordinates
posToCoordinates n pos =
    if pos >= 0 && pos < n ^ 2 then
        Just
            { x = pos % n
            , y = pos // n
            }
    else
        Nothing


walkableNeighboursOfPos : Model -> Pos -> List Pos
walkableNeighboursOfPos m pos =
    neighboursOfPos m.n pos
        |> List.map (\x -> Array.get x m.nodes |> Maybe.map (\y -> ( x, y )))
        |> List.filterMap identity
        |> List.filter (\x -> (Tuple.second x) /= Wall)
        |> List.map Tuple.first


neighboursOfPos : N -> Pos -> List Pos
neighboursOfPos n pos =
    let
        o =
            posToCoordinates n pos
    in
        case o of
            Just v ->
                [ coordinatesToPos n { x = v.x, y = v.y + 1 }
                , coordinatesToPos n { x = v.x, y = v.y - 1 }
                , coordinatesToPos n { x = v.x + 1, y = v.y }
                , coordinatesToPos n { x = v.x - 1, y = v.y }
                ]
                    |> List.filterMap identity

            Nothing ->
                []


coordinatesToPos : N -> Coordinates -> Maybe Pos
coordinatesToPos n pos =
    if pos.x >= 0 && pos.y >= 0 && pos.x < n && pos.y < n then
        Just
            (pos.x + (n * pos.y))
    else
        Nothing


newNodes : Model -> Model
newNodes m =
    { m
        | nodes =
            allNodeTypes
                |> List.map2 List.repeat (List.map (minPerNodeType m) allNodeTypes)
                |> List.concat
                |> Array.fromList
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        Mdl msg_ ->
            Material.update Mdl msg_ model

        Shuffle ->
            ( model, Random.generate NewNodes (Random.Array.shuffle model.nodes) )

        SliderN newNF ->
            let
                newN =
                    round newNF

                modelN =
                    { model
                        | n = newN
                    }
                        |> newNodes
            in
                ( modelN
                , Random.generate NewNodes (Random.Array.shuffle modelN.nodes)
                )

        SliderWallRatio newR ->
            let
                modelN =
                    { model
                        | wallRatio = newR
                    }
                        |> newNodes
            in
                ( modelN
                , Random.generate NewNodes (Random.Array.shuffle modelN.nodes)
                )

        SliderTick tick ->
            ( { model | tick = tick }, Cmd.none )

        Step ->
            ( solveStep { model | state = Solvable }, Cmd.none )

        Solve ->
            ( { model | state = Solving }, Cmd.none )

        SolveUpdate _ ->
            ( solveStep model, Cmd.none )

        NewNodes newNodes ->
            let
                nodes =
                    newNodes

                startPos =
                    Maybe.withDefault 0 (firstNodeType nodes Start)

                endPos =
                    Maybe.withDefault 0 (firstNodeType nodes End)
            in
                ( { model
                    | nodes = nodes
                    , fScore = Array.repeat (model.n ^ 2) Infinity |> Array.set startPos (distanceEstimate model.n startPos endPos)
                    , gScore = Array.repeat (model.n ^ 2) Infinity |> Array.set startPos (Int 0)
                    , closedSet = Set.empty
                    , openSet = Set.fromList ([ startPos ])
                    , cameFrom = (model.n ^ 2) |> List.range 0 |> Array.fromList
                    , state = Solvable
                  }
                , Cmd.none
                )


viewButtons : Model -> List Msg -> List (Html Msg)
viewButtons m msgs =
    List.indexedMap
        (\i x ->
            Button.render Mdl
                [ i ]
                m.mdl
                ([ Button.raised
                 , Button.colored
                 , Button.ripple
                 , Options.onClick x
                 , Options.css "margin" "5px"
                 , Options.css "width" "190px"
                 , Options.css "height" "40px"
                 ]
                    |> List.append
                        (case ( x, m.state ) of
                            ( Shuffle, _ ) ->
                                []

                            ( Solve, Solving ) ->
                                [ Button.disabled, Options.css "padding" "6px" ]

                            ( _, Solved ) ->
                                [ Button.disabled ]

                            ( _, Unsolvable ) ->
                                [ Button.disabled ]

                            _ ->
                                []
                        )
                )
                [ (case ( x, m.state ) of
                    ( Solve, Solving ) ->
                        Loading.spinner
                            [ Loading.active True
                            , Loading.singleColor True
                            ]

                    ( Solve, Solved ) ->
                        Html.span
                            [ Attr.style [ ( "color", "green" ) ] ]
                            [ text
                                (toString m.state)
                            ]

                    ( Solve, Unsolvable ) ->
                        Html.span
                            [ Attr.style [ ( "color", "red" ) ] ]
                            [ text
                                (toString m.state)
                            ]

                    _ ->
                        text (toString x)
                  )
                ]
        )
        msgs


header : Model -> List (Html Msg)
header model =
    [ Layout.row
        []
        [ Layout.title []
            [ text
                "A* search algorithm in elm"
            ]
        , Layout.spacer
        , Layout.navigation []
            [ Layout.link
                [ Layout.href "https://github.com/simonswine/elm-a-star" ]
                [ Html.span [] [ text "github" ] ]
            , Layout.link
                [ Layout.href "https://en.wikipedia.org/wiki/A*_search_algorithm" ]
                [ text "wikipedia" ]
            , Layout.link
                [ Layout.href "https://twitter.com/simonswine" ]
                [ text "twitter" ]
            ]
        ]
    ]


view : Model -> Html Msg
view model =
    Layout.render Mdl
        model.mdl
        [ Layout.fixedHeader
        ]
        { header = (header model)
        , drawer = []
        , tabs = ( [], [] )
        , main = [ viewMain model ]
        }


viewMain : Model -> Html Msg
viewMain model =
    div [ Attr.style [ ( "margin", "15px auto" ), ( "width", (toString model.drawSize) ++ "px" ) ] ]
        [ viewDraw model
        , div []
            (viewButtons
                model
                [ Shuffle, Step, Solve ]
            )
        , Lists.ul []
            [ Lists.li []
                [ Lists.content [] [ "Size " ++ (toString model.n) |> text ]
                , Lists.content2 []
                    [ p [ Attr.style [ ( "width", "400px" ) ] ]
                        [ Slider.view
                            [ Slider.onChange SliderN
                            , Slider.value (toFloat model.n)
                            , Slider.max 30
                            , Slider.min 2
                            , Slider.step 1
                            ]
                        ]
                    ]
                ]
            , Lists.li []
                [ Lists.content [] [ "Wall ratio " ++ (toString model.wallRatio) |> text ]
                , Lists.content2 []
                    [ p [ Attr.style [ ( "width", "400px" ) ] ]
                        [ Slider.view
                            [ Slider.onChange SliderWallRatio
                            , Slider.value model.wallRatio
                            , Slider.max 0.9
                            , Slider.min 0.1
                            , Slider.step 0.1
                            ]
                        ]
                    ]
                ]
            , Lists.li []
                [ Lists.content [] [ "Tick " ++ (toString model.tick) ++ "ms" |> text ]
                , Lists.content2 []
                    [ p [ Attr.style [ ( "width", "400px" ) ] ]
                        [ Slider.view
                            [ Slider.onChange SliderTick
                            , Slider.value model.tick
                            , Slider.max 1000
                            , Slider.min 50
                            , Slider.step 50
                            ]
                        ]
                    ]
                ]
            ]
        , viewTable model
        ]


drawNodes : Model -> List (Svg msg)
drawNodes m =
    m.nodes
        |> Array.indexedMap (drawNode m)
        |> Array.toList
        |> List.concat


drawNode : Model -> Int -> NodeType -> List (Svg msg)
drawNode model pos nodeType =
    let
        gap =
            2.0

        nodeMaybe =
            posToCoordinates model.n pos
    in
        case nodeMaybe of
            Just node ->
                let
                    nodeWidth =
                        (toFloat model.drawSize) / (toFloat model.n) - gap

                    nodeHeight =
                        (toFloat model.drawSize) / (toFloat model.n) - gap

                    fsize =
                        22.0 * (16.0 / (toFloat model.n)) |> round |> toString
                in
                    [ (rect
                        [ fill (colourPerPos model pos |> colorToHex)
                        , (toFloat node.x) * (nodeWidth + gap) |> toString |> x
                        , (toFloat node.y) * (nodeHeight + gap) |> toString |> y
                        , width (toString nodeWidth)
                        , height (toString nodeHeight)
                        , Svg.Attributes.title "X"
                        ]
                        []
                      )
                    , (Svg.text_
                        [ fill "#000000"
                        , (toFloat node.x) * (nodeWidth + gap) + (nodeWidth / 2.0) |> toString |> x
                        , (toFloat node.y) * (nodeHeight + gap) + (nodeHeight / 1.4) |> toString |> y
                        , alignmentBaseline "middle"
                        , textAnchor "middle"
                        , (fsize ++ "px" |> fontSize)
                        ]
                        [ (text (textPerPos model pos)) ]
                      )
                    ]

            Nothing ->
                []


viewPosition : Model -> Pos -> Html Msg
viewPosition model pos =
    Chip.span []
        [ let
            nodeType =
                Array.get pos model.nodes
          in
            Chip.contact Html.span
                [ (case nodeType of
                    Just nodeType ->
                        colourPerNodeType nodeType |> colorToHex |> Options.css "background-color"

                    Nothing ->
                        Options.css "background-color" "red"
                  )
                , Material.Color.text Material.Color.black
                ]
                [ case nodeType of
                    Just nodeType ->
                        textPerNodeType nodeType |> text

                    Nothing ->
                        text ("")
                ]
        , Chip.content [ Options.css "width" "32px" ]
            [ let
                coords =
                    posToCoordinates model.n pos
              in
                case coords of
                    Just coords ->
                        text ((toString coords.x) ++ "/" ++ (toString coords.y))

                    Nothing ->
                        text ("?/?")
            ]
        ]


viewTable : Model -> Html Msg
viewTable model =
    let
        posStart =
            (firstNodeType model.nodes Start)

        posEnd =
            (firstNodeType model.nodes End)

        posCurrent =
            (currentNode model)

        neighbours =
            Maybe.map (walkableNeighboursOfPos model) posCurrent

        solution =
            reconstructPath model
    in
        div
            []
            [ Lists.ul []
                (List.filterMap
                    identity
                    [ case posStart of
                        Just posStart ->
                            Just
                                (Lists.li
                                    []
                                    [ Lists.content [] [ text "Start" ]
                                    , Lists.content2 [] [ viewPosition model posStart ]
                                    ]
                                )

                        Nothing ->
                            Nothing
                    , case posEnd of
                        Just posEnd ->
                            Just
                                (Lists.li
                                    []
                                    [ Lists.content [] [ text "End" ]
                                    , Lists.content2 [] [ viewPosition model posEnd ]
                                    ]
                                )

                        Nothing ->
                            Nothing
                    , case ( posCurrent, model.state ) of
                        ( _, Unsolvable ) ->
                            Nothing

                        ( _, Solved ) ->
                            Nothing

                        ( Just posCurrent, _ ) ->
                            Just
                                (Lists.li
                                    []
                                    [ Lists.content [] [ text "Current" ]
                                    , Lists.content2 [] [ viewPosition model posCurrent ]
                                    ]
                                )

                        _ ->
                            Nothing
                    , case ( neighbours, model.state ) of
                        ( _, Unsolvable ) ->
                            Nothing

                        ( _, Solved ) ->
                            Nothing

                        ( Just neighbours, _ ) ->
                            Just
                                (Lists.li
                                    []
                                    [ Lists.content [] [ text "Walkable neighbours" ]
                                    , Lists.content2 []
                                        (List.map
                                            (viewPosition model)
                                            neighbours
                                        )
                                    ]
                                )

                        _ ->
                            Nothing
                    , case ( solution, model.state ) of
                        ( [], Solved ) ->
                            Nothing

                        ( solution, Solved ) ->
                            Just
                                (Lists.li
                                    []
                                    [ Lists.content [] [ text ("Solution (" ++ (toString (List.length solution)) ++ " steps)") ]
                                    , Lists.content2 []
                                        (List.map
                                            (viewPosition model)
                                            solution
                                        )
                                    ]
                                )

                        _ ->
                            Nothing
                    ]
                )
            ]


viewDraw : Model -> Html Msg
viewDraw model =
    div
        [ onClick Shuffle
        , Attr.style [ ( "margin", "15px auto" ), ( "width", (toString model.drawSize) ++ "px" ), ( "height", (toString model.drawSize) ++ "px" ), ( "cursor", "pointer" ) ]
        ]
        [ svg
            [ version "1.1"
            , x "0"
            , y "0"
            , viewBox ("0 0 " ++ (toString model.drawSize) ++ " " ++ (toString model.drawSize))
            ]
          <|
            drawNodes model
        ]


init : ( Model, Cmd Msg )
init =
    let
        n =
            16

        model =
            { n = n
            , nodes = Array.empty
            , mdl = Material.model
            , drawSize = 600
            , fScore = Array.repeat (n ^ 2) Infinity
            , gScore = Array.repeat (n ^ 2) Infinity
            , closedSet = Set.empty
            , openSet = Set.empty
            , cameFrom = Array.empty
            , wallRatio = 0.2
            , state = Solvable
            , tick = 200
            }
                |> newNodes
    in
        ( model
        , Random.generate NewNodes (Random.Array.shuffle model.nodes)
        )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.state of
        Solving ->
            Time.every (model.tick * Time.millisecond) SolveUpdate

        _ ->
            Sub.none


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
