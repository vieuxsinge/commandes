port module Main exposing (..)

import Browser
import Html exposing (Html, text, div, h1, img, ul, li, map, input, form, button)
import Html.Events exposing (onInput, onSubmit)
import Html.Attributes exposing (src, class, value)
import List
import Time

import Json.Encode
import Json.Decode

import Maybe.Extra exposing (join, isJust, values)
import Parser exposing (Parser, (|.), (|=), succeed, symbol, int, spaces, chompWhile, getChompedString, run)

import Json.Decode.Pipeline exposing (required)

---- MODEL ----

type alias Model =
    {  
        orderInput: String
        , currentDate: Time.Posix
        , currentOrder: Maybe Order
        , orders: List Order
    }

type alias Order =
    { customer: String
    , orders: List OrderLine
    , date: Time.Posix    }

type alias OrderLine =
  {
      quantity: Int 
      , beer: String
      , format: Int
  }



init : String -> ( Model, Cmd Msg )
init encodedOrders =
    let
        decodedOrders = Json.Decode.decodeString ordersDecoder encodedOrders
        orders = case decodedOrders of
            Ok decoded -> decoded
            Err error ->
                let
                    _ = Debug.log "Error while decoding JSON values" error
                in []
        
    in
    ({
        orderInput = ""
        , currentDate = Time.millisToPosix 0
        , currentOrder = Nothing
        , orders = orders
    }, Cmd.none )



---- UPDATE ----


type Msg
    = UpdateInput String
    | SaveOrder
    | Tick Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateInput content ->
            ({ model
                | currentOrder = Just (parseInput content)
                , orderInput = content
            }, Cmd.none)

        SaveOrder ->
            case model.currentOrder of
                Just order -> 
                    let
                        orders = model.orders ++ [{ order | date = model.currentDate}]
                    in 
                    ({ model
                       | orders = orders
                       , currentOrder = Nothing
                       , orderInput = ""
                      }, storeOrders (encodeOrders orders))

                Nothing -> (model, Cmd.none)
        
        Tick date ->
            ({ model | currentDate = date}, Cmd.none)
        

parseItems : Maybe String -> List OrderLine
parseItems stringItems =
    case stringItems of
        Nothing -> []
        Just string ->
            String.split "," string
            |> List.map String.trim
            |> List.map (\x -> run orderline x |> Result.toMaybe) |> Debug.log "parsed"
            |> List.filterMap identity
            

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
        splits = String.split ":" input |> List.map String.trim
        customer = splits |> List.head |> Maybe.withDefault ""
        items = splits |> List.drop 1 |> List.head |> parseItems
    in
    
    {
        customer = customer
        , orders = items
        , date = Time.millisToPosix 0
    }


---- VIEW ----
displayBeerName : String -> String
displayBeerName str =
    case str of
        "ST" -> "Souffle Tropical"
        "NM" -> "Nouveau Monde"
        "EQD" -> "L'Eau Qui Dort"
        "EPT" -> "En Pleine Tempête"
        beername -> beername


viewOrderLine : OrderLine -> Html Msg
viewOrderLine orderLine =
    li [] [text (displayBeerName orderLine.beer ++ " " ++ String.fromInt orderLine.format ++ " x " ++ String.fromInt orderLine.quantity)]

viewOrder : Order -> Html Msg
viewOrder order =
    div []
        [ div [] [ text order.customer ]
        , ul [] ( List.map viewOrderLine order.orders )
        ]


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Entrez une nouvelle commande"]
        , form [onSubmit SaveOrder] [
            input [ class "order", onInput UpdateInput, value model.orderInput] [ ]
            , button [class "submit"] []
            , div [class "current-order"] [(
                case model.currentOrder of
                    Just order -> viewOrder order
                    Nothing -> text ""
                )]
            , div [] (List.map viewOrder model.orders)
        ]
        ]

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
        [
            ("customer", Json.Encode.string order.customer)
           ,("orders", Json.Encode.list encodeOrderLine order.orders)
           ,("date", order.date |> Time.posixToMillis  |> Json.Encode.int )
        ]

encodeOrderLine : OrderLine -> Json.Encode.Value
encodeOrderLine orderLine =
    Json.Encode.object
        [
              ("quantity", Json.Encode.int orderLine.quantity)
            , ("beer", Json.Encode.string orderLine.beer)
            , ("format", Json.Encode.int orderLine.format)
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
