port module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, src, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Json.Decode
import Json.Decode.Pipeline exposing (required)
import Json.Encode
import List
import List.Extra
import Maybe.Extra exposing (isJust, join, values)
import Parser exposing ((|.), (|=), Parser, chompWhile, getChompedString, int, run, spaces, succeed, symbol)
import Time
import Time.Format
import Time.Format.Config.Config_fr_fr exposing (config)



---- MODEL ----


type alias Model =
    { orderInput : String
    , currentDate : Time.Posix
    , editedItemNumber : Maybe Int
    , currentOrder : Maybe Order
    , orders : List Order
    }


type alias OrderLine =
    { quantity : Int
    , beer : String
    , format : Int
    }


type alias Order =
    { customer : String
    , orders : List OrderLine
    , date : Time.Posix
    }


init : String -> ( Model, Cmd Msg )
init encodedOrders =
    let
        decodedOrders =
            Json.Decode.decodeString ordersDecoder encodedOrders

        orders =
            case decodedOrders of
                Ok decoded ->
                    decoded

                Err error ->
                    let
                        _ =
                            Debug.log "Error while decoding JSON values" error
                    in
                    []
    in
    ( { orderInput = ""
      , editedItemNumber = Nothing
      , currentDate = Time.millisToPosix 0
      , currentOrder = Nothing
      , orders = orders
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = UpdateInput String
    | SaveOrder
    | EditOrder Order Int
    | Tick Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateInput content ->
            ( { model
                | currentOrder = Just (parseInput content)
                , orderInput = content
              }
            , Cmd.none
            )

        EditOrder order itemNumber ->
            let
                stringOrder =
                    orderToString order

                updatedModel =
                    { model
                        | orderInput = stringOrder
                        , editedItemNumber = Just itemNumber
                    }
            in
            update (UpdateInput stringOrder) updatedModel

        SaveOrder ->
            case model.currentOrder of
                Just order ->
                    let
                        newOrder =
                            { order | date = model.currentDate }

                        orders =
                            case model.editedItemNumber of
                                Just int ->
                                    List.Extra.setAt int newOrder model.orders

                                Nothing ->
                                    model.orders ++ [ newOrder ]
                    in
                    ( { model
                        | orders = orders
                        , currentOrder = Nothing
                        , orderInput = ""
                        , editedItemNumber = Nothing
                      }
                    , storeOrders (encodeOrders orders)
                    )

                Nothing ->
                    ( model, Cmd.none )

        Tick date ->
            ( { model | currentDate = date }, Cmd.none )


parseItems : Maybe String -> List OrderLine
parseItems stringItems =
    case stringItems of
        Nothing ->
            []

        Just string ->
            String.split "," string
                |> List.map String.trim
                |> List.map (\x -> run orderline x |> Result.toMaybe)
                |> Debug.log "parsed"
                |> List.filterMap identity


orderToString : Order -> String
orderToString order =
    let
        orders =
            String.join ", " <|
                List.map
                    (\x ->
                        (.quantity x |> String.fromInt)
                            ++ "x"
                            ++ .beer x
                            ++ (.format x |> String.fromInt)
                    )
                    order.orders
    in
    order.customer ++ " : " ++ orders



-- Parses "2xST33"


orderline : Parser OrderLine
orderline =
    succeed OrderLine
        |= int
        |. symbol "x"
        |= (getChompedString <| chompWhile Char.isUpper)
        |= int


parseInput : String -> Order
parseInput input =
    let
        splits =
            String.split ":" input |> List.map String.trim

        customer =
            splits |> List.head |> Maybe.withDefault ""

        items =
            splits |> List.drop 1 |> List.head |> parseItems
    in
    { customer = customer
    , orders = items
    , date = Time.millisToPosix 0
    }



---- VIEW ----


displayBeerName : String -> String
displayBeerName str =
    case str of
        "ST" ->
            "Souffle Tropical"

        "NM" ->
            "Nouveau Monde"

        "EQD" ->
            "L'Eau Qui Dort"

        "EPT" ->
            "En Pleine Tempête"

        beername ->
            beername


getBeers : List Order -> List String
getBeers orders =
    orders |> List.map .orders |> List.concat |> List.map .beer |> List.Extra.unique


getBeerFromOrder : String -> List OrderLine -> Maybe OrderLine
getBeerFromOrder beerName orders =
    List.filter (\x -> .beer x == beerName) orders |> List.head


viewOrderLine : OrderLine -> Html Msg
viewOrderLine orderLine =
    li [] [ text (displayBeerName orderLine.beer ++ " " ++ String.fromInt orderLine.format ++ " x " ++ String.fromInt orderLine.quantity) ]


viewOrder : Order -> Html Msg
viewOrder order =
    div []
        [ div [] [ text order.customer ]
        , ul [] (List.map viewOrderLine order.orders)
        ]


viewBeerList : List String -> Html Msg
viewBeerList beers =
    ul [] (List.map (\b -> li [] [ displayBeerName b |> text ]) beers)


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Entrez une nouvelle commande" ]
        , form [ onSubmit SaveOrder ]
            [ input [ class "order", onInput UpdateInput, value model.orderInput ] []
            , button [ class "submit" ] []
            , div [ class "current-order" ]
                [ case model.currentOrder of
                    Just order ->
                        viewOrder order

                    Nothing ->
                        text ""
                ]
            , section [ class "section" ]
                [ div [ class "columns is-centered" ]
                    [ div [ class "column is-narrow" ]
                        [ viewTableOrders model.orders ]
                    ]
                ]
            ]
        ]


viewTableOrders : List Order -> Html Msg
viewTableOrders orders =
    let
        beerNames =
            getBeers orders

        headers =
            List.foldr (::) (List.map displayBeerName beerNames) [ "Date", "Client" ]

        lines =
            viewSums beerNames orders
                :: List.indexedMap (viewLine beerNames) orders
    in
    table [ class "table" ]
        [ thead []
            [ tr [] (List.map (\h -> th [] [ text h ]) headers)
            ]
        , tbody [] lines
        ]


viewSums : List String -> List Order -> Html Msg
viewSums beerNames orders =
    let
        headers =
            [ td [] [ text "Totaux" ]
            , td [] []
            ]

        sumCell amount =
            td [] [ String.fromInt amount |> text ]

        cells =
            List.map (getSum orders) beerNames
                |> List.map sumCell
    in
    tr [ class "totaux" ] (List.foldr (::) cells headers)


getSum : List Order -> String -> Int
getSum orders beerName =
    List.map .orders orders
        |> List.concat
        |> List.filter (\x -> .beer x == beerName)
        |> List.map .quantity
        |> List.sum


viewLine : List String -> Int -> Order -> Html Msg
viewLine beerNames itemNumber order =
    let
        orderInfo =
            [ th []
                [ text
                    (Time.Format.format
                        config
                        "%-d %B"
                        Time.utc
                        order.date
                    )
                ]
            , th [] [ text order.customer ]
            ]

        columns =
            List.foldr (::) (viewColumnsForBeers beerNames order.orders) orderInfo
    in
    tr [ onClick (EditOrder order itemNumber) ] columns


viewColumnsForBeers : List String -> List OrderLine -> List (Html Msg)
viewColumnsForBeers beerNames orders =
    let
        filteredOrders =
            List.map (\beerName -> getBeerFromOrder beerName orders) beerNames

        getLine order =
            case order of
                Just o ->
                    td [] [ .quantity o |> String.fromInt |> text ]

                Nothing ->
                    td [] []
    in
    List.map getLine filteredOrders



---- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick



---- Encoders & Decoders ----


encodeOrders : List Order -> Json.Encode.Value
encodeOrders orders =
    Json.Encode.list encodeOrder orders


encodeOrder : Order -> Json.Encode.Value
encodeOrder order =
    Json.Encode.object
        [ ( "customer", Json.Encode.string order.customer )
        , ( "orders", Json.Encode.list encodeOrderLine order.orders )
        , ( "date", order.date |> Time.posixToMillis |> Json.Encode.int )
        ]


encodeOrderLine : OrderLine -> Json.Encode.Value
encodeOrderLine orderLine =
    Json.Encode.object
        [ ( "quantity", Json.Encode.int orderLine.quantity )
        , ( "beer", Json.Encode.string orderLine.beer )
        , ( "format", Json.Encode.int orderLine.format )
        ]


ordersDecoder : Json.Decode.Decoder (List Order)
ordersDecoder =
    Json.Decode.list orderDecoder


orderDecoder : Json.Decode.Decoder Order
orderDecoder =
    Json.Decode.succeed Order
        |> required "customer" Json.Decode.string
        |> required "orders" (Json.Decode.list orderLineDecoder)
        |> required "date" (Json.Decode.map Time.millisToPosix Json.Decode.int)


orderLineDecoder : Json.Decode.Decoder OrderLine
orderLineDecoder =
    Json.Decode.succeed OrderLine
        |> required "quantity" Json.Decode.int
        |> required "beer" Json.Decode.string
        |> required "format" Json.Decode.int


port storeOrders : Json.Encode.Value -> Cmd msg



---- PROGRAM ----


main : Program String Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
