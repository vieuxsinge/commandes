module Stock exposing (..)

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
    Dict String (List StockItem)


type alias StockItem =
    { id : Int
    , format : BeerFormat
    , available : Int
    , code : String
    , name : String
    }


nullStockItem =
    StockItem 0 NoFormat 0 "" ""


empty =
    Dict.empty


convertToBoxes : BeerFormat -> Int -> Int
convertToBoxes format number =
    case format of
        Bottle75 ->
            number // 12

        Bottle33 ->
            number // 24

        Keg20L ->
            number

        NoFormat ->
            0


convertToUnits : BeerFormat -> Int -> Int
convertToUnits format number =
    case format of
        Bottle75 ->
            number * 12

        Bottle33 ->
            number * 24

        Keg20L ->
            number

        NoFormat ->
            number


hasStock : ( String, List StockItem ) -> Bool
hasStock ( string, items ) =
    items
        |> List.map .available
        |> List.sum
        |> (<=) 1


viewStockForBeer : ( String, List StockItem ) -> Html msg
viewStockForBeer ( beerName, items ) =
    let
        getFormat format =
            List.filter (\item -> .format item == format) items
                |> List.head

        asStock item =
            case item of
                Just a ->
                    let
                        available =
                            convertToBoxes a.format a.available
                    in
                    case available of
                        0 ->
                            text "-"

                        _ ->
                            String.fromInt available |> text

                Nothing ->
                    text "-"
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


encodeStockData : ( String, List StockItem ) -> Json.Encode.Value
encodeStockData ( beerName, items ) =
    Json.Encode.object
        [ ( "name", Json.Encode.string beerName )
        , ( "items", encodeStockItem items )
        ]


encodeStockItem : List StockItem -> Json.Encode.Value
encodeStockItem stockItem =
    Json.Encode.list encodeStockItemData stockItem


encodeStockItemData : StockItem -> Json.Encode.Value
encodeStockItemData { id, format, available, code, name } =
    Json.Encode.object
        [ ( "format", Json.Encode.string (formatToString format) )
        , ( "available", Json.Encode.int available )
        , ( "id", Json.Encode.int id )
        , ( "code", Json.Encode.string code )
        , ( "name", Json.Encode.string name )
        ]


transformJsonToStockItem : JsonStock -> ( String, List StockItem )
transformJsonToStockItem jsonStock =
    ( .name jsonStock
    , .items jsonStock
        |> List.map
            (\x ->
                { id = x.id
                , format = x.format
                , available = x.available
                , code = x.code
                , name = x.name
                }
            )
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
    { id : Int
    , format : BeerFormat
    , available : Int
    , code : String
    , name : String
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
        |> required "id" Json.Decode.int
        |> required "format" (Json.Decode.map stringToFormat Json.Decode.string)
        |> required "available" Json.Decode.int
        |> required "code" Json.Decode.string
        |> required "name" Json.Decode.string


type alias SourceStockFormat =
    { id : Int, name : String, available : Int, format : BeerFormat, code : String }


decodeFromServer : String -> Stock
decodeFromServer encoded =
    let
        source =
            Json.Decode.decodeString serverStockDecoder encoded
                |> Debug.log "decodedString from server"
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
        |> required "id" Json.Decode.int
        |> required "x_beername" Json.Decode.string
        |> required "qty_available" Json.Decode.int
        |> required "x_volume" toBeerFormat
        |> required "default_code" Json.Decode.string


transformSources : SourceStockFormat -> List ( String, List StockItem ) -> List ( String, List StockItem )
transformSources { id, name, format, available, code } =
    Dict.fromList
        >> (\dict ->
                if Dict.member name dict then
                    Dict.update name
                        (Maybe.map
                            (\v ->
                                { format = format
                                , available = available
                                , id = id
                                , code = code
                                , name = name
                                }
                                    :: v
                            )
                        )
                        dict

                else
                    Dict.insert name
                        [ { format = format
                          , available = available
                          , id = id
                          , code = code
                          , name = name
                          }
                        ]
                        dict
           )
        >> Dict.toList
