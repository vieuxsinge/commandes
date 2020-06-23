module Stock exposing (Stock, StockItem, decode, empty, viewTableStock)

import Html exposing (..)
import Html.Attributes exposing (class, colspan)
import Json.Decode
import Json.Decode.Pipeline exposing (required)
import List
import List.Extra
import String


type BeerFormat
    = Bottle75
    | Bottle33
    | Keg20L
    | NoFormat


type alias Stock =
    List StockItem


type alias StockItem =
    { name : String
    , quantity : Int
    , format : BeerFormat
    }


empty =
    []


formatToString : BeerFormat -> String
formatToString beerFormat =
    case beerFormat of
        Bottle75 ->
            "75cl"

        Bottle33 ->
            "33cl"

        Keg20L ->
            "20L"

        NoFormat ->
            "?"


getEachQuantity : Stock -> List BeerFormat -> String -> List Int
getEachQuantity stock formats name =
    let
        findFormat format =
            List.Extra.find (\x -> .format x == format && .name x == name) stock
                |> (\x ->
                        case x of
                            Just value ->
                                value.quantity

                            Nothing ->
                                0
                   )
    in
    List.map findFormat formats


viewTableStock : Stock -> Html msg
viewTableStock stock =
    let
        beerNames =
            List.filter (\x -> x.quantity > 0) stock
                |> List.map .name
                |> List.Extra.unique

        formats =
            [ Bottle75, Bottle33, Keg20L ]
    in
    table [ class "table" ]
        [ thead []
            [ tr []
                [ th [] []
                , th [] [ text "75cl" ]
                , th [] [ text "33cl" ]
                , th [] [ text "20L" ]
                ]
            ]
        , tbody [] (List.map (viewStockForBeer stock formats) beerNames)
        ]


viewStockForBeer : Stock -> List BeerFormat -> String -> Html msg
viewStockForBeer stock formats beerName =
    let
        cell value =
            td [] [ value |> String.fromInt |> text ]

        headers =
            [ td [] [ text beerName ] ]

        quantities =
            getEachQuantity stock formats beerName

        cells =
            List.map cell quantities
    in
    tr [] (td [] [ text beerName ] :: cells)



-- oldviewTableStock : Stock -> Html msg
-- oldviewTableStock stockItemList =
--     let
--         beerNames =
--             List.filter (\x -> x.quantity > 0) stockItemList
--                 |> List.map .name
--                 |> List.Extra.unique
--
--         formats =
--             [ 75, 33, 20 ]
--
--         quantities =
--             List.map (getEachQuantity stockItemList formats) beerNames
--                 |> List.concat
--     in
--     case beerNames of
--         [] ->
--             div [] []
--
--         items ->
--             table [ class "table" ]
--                 [ thead []
--                     [ tr []
--                         (List.map (\x -> th [ colspan 3 ] [ text x ]) items)
--                     , tr []
--                         (List.repeat (List.length items) (viewListFormats formats)
--                             |> List.concat
--                         )
--                     ]
--                 , tbody []
--                     [ tr [] (List.map (\x -> td [] [ x |> String.fromInt |> text ]) quantities)
--                     ]
--                 ]


viewListFormats : List BeerFormat -> List (Html msg)
viewListFormats formats =
    List.map (\x -> th [] [ x |> formatToString |> text ]) formats



-- Decoders


decode : String -> Stock
decode stock =
    case Json.Decode.decodeString stockDecoder stock of
        Ok value ->
            value

        Err _ ->
            []


stockDecoder : Json.Decode.Decoder (List StockItem)
stockDecoder =
    Json.Decode.list stockItemDecoder


toBeerFormat : Json.Decode.Decoder BeerFormat
toBeerFormat =
    let
        process =
            \x ->
                if x == 0.33 then
                    Bottle33

                else if x == 0.75 then
                    Bottle75

                else if x == 20.0 then
                    Keg20L

                else
                    NoFormat
    in
    Json.Decode.map process Json.Decode.float


stockItemDecoder : Json.Decode.Decoder StockItem
stockItemDecoder =
    Json.Decode.succeed StockItem
        |> required "x_beername" Json.Decode.string
        |> required "qty_available" Json.Decode.int
        |> required "x_volume" toBeerFormat
