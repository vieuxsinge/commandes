module Stock exposing (Stock, StockItem, decodeFromServer, decodeStock, empty, encodeStock, viewTableStock)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (class, colspan)
import Json.Decode
import Json.Decode.Pipeline exposing (required)
import Json.Encode
import List
import List.Extra
import String


type BeerFormat
    = Bottle75
    | Bottle33
    | Keg20L
    | NoFormat


type alias Stock =
    Dict String StockItem


type alias StockItem =
    List ( BeerFormat, Int )


empty =
    Dict.empty


asBoxes : ( BeerFormat, Int ) -> Int
asBoxes ( format, quantity ) =
    case format of
        Bottle75 ->
            quantity // 12

        Bottle33 ->
            quantity // 24

        Keg20L ->
            quantity

        NoFormat ->
            0


hasStock : ( String, StockItem ) -> Bool
hasStock ( string, stockItem ) =
    stockItem
        |> List.map Tuple.second
        |> List.sum
        |> (<=) 1


viewStockForBeer : ( String, StockItem ) -> Html msg
viewStockForBeer ( beerName, stockItem ) =
    let
        getFormat format =
            List.filter (\item -> Tuple.first item == format) stockItem
                |> List.head
                |> Maybe.withDefault ( NoFormat, 0 )

        asStock =
            asBoxes >> String.fromInt >> text
    in
    tr []
        [ td [] [ text beerName ]
        , td [] [ getFormat Bottle75 |> asStock ]
        , td [] [ getFormat Bottle33 |> asStock ]
        , td [] [ getFormat Keg20L |> asStock ]
        ]


viewTableStock : Stock -> Html msg
viewTableStock stock =
    table [ class "table" ]
        [ thead []
            [ tr []
                [ th [] [ text "Stock" ]
                , th [] [ formatToString Bottle75 |> text ]
                , th [] [ formatToString Bottle33 |> text ]
                , th [] [ formatToString Keg20L |> text ]
                ]
            ]
        , tbody []
            (Dict.toList stock
                |> List.filter hasStock
                |> List.map viewStockForBeer
            )
        ]


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


stringToFormat : String -> BeerFormat
stringToFormat string =
    case string of
        "75cl" ->
            Bottle75

        "33cl" ->
            Bottle33

        "20L" ->
            Keg20L

        _ ->
            NoFormat



-- Decoders ↓


encodeStock : Stock -> Json.Encode.Value
encodeStock stock =
    Json.Encode.list encodeStockData (Dict.toList stock)


encodeStockData : ( String, StockItem ) -> Json.Encode.Value
encodeStockData ( beerName, stockItem ) =
    Json.Encode.object
        [ ( "name", Json.Encode.string beerName )
        , ( "items", encodeStockItem stockItem )
        ]


encodeStockItem : List ( BeerFormat, Int ) -> Json.Encode.Value
encodeStockItem stockItem =
    Json.Encode.list encodeStockItemData stockItem


encodeStockItemData : ( BeerFormat, Int ) -> Json.Encode.Value
encodeStockItemData ( format, count ) =
    Json.Encode.object
        [ ( "format", Json.Encode.string (formatToString format) )
        , ( "count", Json.Encode.int count )
        ]


transformJsonToStockItem : JsonStock -> ( String, StockItem )
transformJsonToStockItem jsonStock =
    ( .name jsonStock
    , .items jsonStock
        |> List.map (\x -> ( .format x, .count x ))
    )


decodeStock : String -> Stock
decodeStock encoded =
    Json.Decode.decodeString stockListDecoder encoded
        |> Result.withDefault []
        |> List.map transformJsonToStockItem
        |> Dict.fromList


type alias JsonStock =
    { name : String
    , items : List JsonStockItem
    }


type alias JsonStockItem =
    { format : BeerFormat
    , count : Int
    }


stockListDecoder : Json.Decode.Decoder (List JsonStock)
stockListDecoder =
    Json.Decode.list stockDecoder


stockDecoder : Json.Decode.Decoder JsonStock
stockDecoder =
    Json.Decode.succeed JsonStock
        |> required "name" Json.Decode.string
        |> required "items" (Json.Decode.list stockItemDecoder)


stockItemDecoder : Json.Decode.Decoder JsonStockItem
stockItemDecoder =
    Json.Decode.succeed JsonStockItem
        |> required "format" (Json.Decode.map stringToFormat Json.Decode.string)
        |> required "count" Json.Decode.int


type alias SourceStockFormat =
    { name : String, quantity : Int, format : BeerFormat }


decodeFromServer : String -> Stock
decodeFromServer encoded =
    let
        source =
            Json.Decode.decodeString serverStockDecoder encoded
                |> Result.withDefault []
    in
    source
        |> List.foldl transformSources []
        |> Dict.fromList


serverStockDecoder : Json.Decode.Decoder (List SourceStockFormat)
serverStockDecoder =
    Json.Decode.list serverStockItemDecoder


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


serverStockItemDecoder : Json.Decode.Decoder SourceStockFormat
serverStockItemDecoder =
    Json.Decode.succeed SourceStockFormat
        |> required "x_beername" Json.Decode.string
        |> required "qty_available" Json.Decode.int
        |> required "x_volume" toBeerFormat


transformSources : SourceStockFormat -> List ( String, StockItem ) -> List ( String, StockItem )
transformSources { name, format, quantity } =
    Dict.fromList
        >> (\dict ->
                if Dict.member name dict then
                    Dict.update name (Maybe.map (\v -> ( format, quantity ) :: v)) dict

                else
                    Dict.insert name [ ( format, quantity ) ] dict
           )
        >> Dict.toList
