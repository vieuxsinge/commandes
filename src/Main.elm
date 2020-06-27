port module Main exposing (..)

import Browser
import Dict
import Html exposing (..)
import Html.Attributes exposing (class, colspan, id, placeholder, src, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Json.Decode
import Json.Decode.Pipeline exposing (required)
import Json.Encode
import List
import List.Extra
import Maybe.Extra exposing (isJust, join, values)
import Parser exposing ((|.), (|=), Parser, chompWhile, getChompedString, int, run, spaces, succeed, symbol)
import Select
import Simple.Fuzzy
import Stock
import Time
import Time.Format
import Time.Format.Config.Config_fr_fr exposing (config)


type alias Model =
    { orderInput : String
    , clientInput : String
    , selectedCustomerId : Maybe Int
    , selectedCustomer : Maybe Customer
    , selectState : Select.State
    , currentDate : Time.Posix
    , editedItemNumber : Maybe Int
    , currentOrder : Maybe Order
    , orders : List Order
    , customers : List Customer
    , serverPassword : Maybe String
    , serverPasswordInput : String
    , stock : Stock.Stock
    }


type alias OrderLine =
    { quantity : Int
    , beer : String
    , format : Int
    }


type alias Order =
    { customer : Customer
    , lines : List OrderLine
    , date : Time.Posix
    }


type alias Customer =
    { id : Int
    , name : String
    }


noCustomer =
    Customer 0 "NO CUSTOMER"


init : ( String, String ) -> ( Model, Cmd Msg )
init ( encodedOrders, serverPassword ) =
    let
        decodedOrders =
            Json.Decode.decodeString ordersDecoder encodedOrders

        decodedPassword =
            case serverPassword of
                "" ->
                    Nothing

                string ->
                    Just string

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
      , serverPassword = decodedPassword
      , serverPasswordInput = ""
      , stock = Stock.empty
      , customers = []
      , clientInput = ""
      , selectState = Select.newState ""
      , selectedCustomerId = Nothing
      , selectedCustomer = Nothing
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = UpdateInput String
    | UpdateClientInput String
    | SaveOrder
    | EditOrder Order Int
    | ResetOrders
    | SaveServerPassword
    | UpdateServerPassword String
    | Tick Time.Posix
    | RetrieveStock
    | UpdateStock String
    | RetrieveCustomers
    | UpdateCustomers String
    | OnSelect (Maybe Customer)
    | SelectMsg (Select.Msg Customer)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnSelect maybeCustomer ->
            ( { model | selectedCustomer = maybeCustomer }, Cmd.none )

        SelectMsg subMsg ->
            let
                ( updated, cmd ) =
                    Select.update selectConfig subMsg model.selectState
            in
            ( { model | selectState = updated }, cmd )

        UpdateServerPassword content ->
            ( { model
                | serverPasswordInput = content
              }
            , Cmd.none
            )

        SaveServerPassword ->
            let
                password =
                    model.serverPasswordInput
            in
            ( { model
                | serverPassword = Just password
                , serverPasswordInput = ""
              }
            , storePassword password
            )

        UpdateInput content ->
            let
                customer =
                    model.selectedCustomer |> Maybe.withDefault noCustomer

                order =
                    Order
                        customer
                        (parseItems (Just content))
                        model.currentDate
            in
            ( { model
                | currentOrder = Just order
                , orderInput = content
              }
            , Cmd.none
            )

        UpdateClientInput content ->
            ( { model
                | clientInput = content
              }
            , Cmd.none
            )

        ResetOrders ->
            ( { model | orders = [] }, storeOrders (encodeOrders []) )

        EditOrder order itemNumber ->
            let
                stringOrder =
                    orderToString order
            in
            update (UpdateInput stringOrder)
                { model
                    | orderInput = stringOrder
                    , editedItemNumber = Just itemNumber
                    , selectedCustomer = Just order.customer
                }

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
                        , selectedCustomer = Nothing
                      }
                    , storeOrders (encodeOrders orders)
                    )

                Nothing ->
                    ( model, Cmd.none )

        Tick date ->
            ( { model | currentDate = date }, Cmd.none )

        RetrieveStock ->
            ( model, retrieveStockFromServer "" )

        RetrieveCustomers ->
            ( model, retrieveCustomersFromServer "" )

        UpdateStock stock ->
            ( { model | stock = Stock.decode stock }, Cmd.none )

        UpdateCustomers encodedCustomers ->
            let
                customers =
                    Json.Decode.decodeString customersDecoder encodedCustomers |> Result.withDefault []
            in
            ( { model | customers = customers }, storeCustomers (encodeCustomers customers) )


parseItems : Maybe String -> List OrderLine
parseItems stringItems =
    case stringItems of
        Nothing ->
            []

        Just string ->
            String.split "," string
                |> List.map String.trim
                |> List.map (\x -> run orderline x |> Result.toMaybe)
                |> List.filterMap identity


orderToString : Order -> String
orderToString order =
    let
        orders =
            String.join ", " <|
                List.map
                    (\x ->
                        (.quantity x |> String.fromInt)
                            ++ .beer x
                            ++ (.format x |> String.fromInt)
                    )
                    order.lines
    in
    orders



-- Parses "2xST33"


orderline : Parser OrderLine
orderline =
    succeed OrderLine
        |= int
        |= (getChompedString <| chompWhile Char.isUpper)
        |= int


fuzzyFilter : Int -> (a -> String) -> String -> List a -> Maybe (List a)
fuzzyFilter minChars toLabel query items =
    if String.length query < minChars then
        Nothing

    else
        items
            |> Simple.Fuzzy.filter toLabel query
            |> Just



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
    orders |> List.map .lines |> List.concat |> List.map .beer |> List.Extra.unique


getBeerFromOrder : String -> List OrderLine -> Maybe OrderLine
getBeerFromOrder beerName lines =
    List.filter (\x -> .beer x == beerName) lines |> List.head


viewOrderLine : OrderLine -> Html Msg
viewOrderLine orderLine =
    li [] [ text (displayBeerName orderLine.beer ++ " " ++ String.fromInt orderLine.format ++ " x " ++ String.fromInt orderLine.quantity) ]


viewOrder : Order -> Html Msg
viewOrder order =
    div []
        [ div [] [ text order.customer.name ]
        , ul [] (List.map viewOrderLine order.lines)
        ]


viewBeerList : List String -> Html Msg
viewBeerList beers =
    ul [] (List.map (\b -> li [] [ displayBeerName b |> text ]) beers)


view : Model -> Html Msg
view model =
    case model.serverPassword of
        Just string ->
            mainView model

        Nothing ->
            enterPasswordView model


enterPasswordView : Model -> Html Msg
enterPasswordView model =
    div []
        [ form [ onSubmit SaveServerPassword ]
            [ input [ placeholder "Merci de rentrer le code d'accès a Odoo", onInput UpdateServerPassword, value model.serverPasswordInput ] []
            ]
        ]


mainView : Model -> Html Msg
mainView model =
    div [ class "section" ]
        [ div [ class "container" ]
            [ nav [ class "level" ]
                [ div [ class "level-left" ]
                    [ div [ class "level-item" ]
                        [ h1 [] [ text "Liste des commandes" ]
                        ]
                    ]
                , div [ class "level-right" ]
                    [ p [ class "level-item", onClick ResetOrders ] [ text "reset" ]
                    , p [ class "level-item", onClick RetrieveStock ] [ text "récup le stock !" ]
                    , p [ class "level-item", onClick RetrieveCustomers ] [ text "get customers" ]
                    ]
                ]
            , div [ class "columns" ]
                [ form [ id "order-form", onSubmit SaveOrder ]
                    [ div [ class "column is-one-third" ] [ customerInputView model ]
                    , div [ class "column" ]
                        [ input
                            [ placeholder "Commande ici, par ex \"10ST20, 3NM75\""
                            , class "order-input"
                            , onInput UpdateInput
                            , value model.orderInput
                            ]
                            []
                        , button [ class "submit" ] []
                        ]
                    ]
                ]
            , div [ class "columns" ]
                [ section [ class "column is-one-third" ]
                    [ div [ class "columns is-centered" ]
                        [ div [ class "column is-narrow" ]
                            [ Stock.viewTableStock model.stock
                            ]
                        ]
                    ]
                , div [ class "column current-order" ]
                    [ case model.currentOrder of
                        Just order ->
                            viewOrder order

                        Nothing ->
                            text ""
                    , viewTableOrders model.orders
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
    case orders of
        [] ->
            p [] [ text "" ]

        items ->
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
    List.map .lines orders
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
            , th [] [ text order.customer.name ]
            ]

        columns =
            List.foldr (::) (viewColumnsForBeers beerNames order.lines) orderInfo
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


customerInputView : Model -> Html Msg
customerInputView model =
    let
        selectedCustomers : List Customer
        selectedCustomers =
            case model.selectedCustomerId of
                Nothing ->
                    []

                Just id ->
                    List.filter (\customer -> customer.id == id) model.customers

        select =
            Select.view
                selectConfig
                model.selectState
                model.customers
                selectedCustomers
    in
    Html.map SelectMsg select


selectConfig : Select.Config Msg Customer
selectConfig =
    Select.newConfig
        { onSelect = OnSelect
        , toLabel = .name
        , filter = fuzzyFilter 2 .name
        }
        |> Select.withNotFound "Aucun client correspondant"
        |> Select.withInputWrapperClass "customer-input"
        |> Select.withItemClass "customer-menu-item"
        |> Select.withMenuClass "customer-menu"
        |> Select.withHighlightedItemClass "item-higlighted"
        |> Select.withPrompt "Nom dula client⋅e"
        |> Select.withUnderlineClass "underline"



---- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 30000 Tick
        , updateStock UpdateStock
        , updateCustomers UpdateCustomers
        ]



---- Encoders & Decoders ----


encodeOrders : List Order -> Json.Encode.Value
encodeOrders orders =
    Json.Encode.list encodeOrder orders


encodeOrder : Order -> Json.Encode.Value
encodeOrder order =
    Json.Encode.object
        [ ( "customer", encodeCustomer order.customer )
        , ( "orders", Json.Encode.list encodeOrderLine order.lines )
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
        |> required "customer" customerDecoder
        |> required "orders" (Json.Decode.list orderLineDecoder)
        |> required "date" (Json.Decode.map Time.millisToPosix Json.Decode.int)


orderLineDecoder : Json.Decode.Decoder OrderLine
orderLineDecoder =
    Json.Decode.succeed OrderLine
        |> required "quantity" Json.Decode.int
        |> required "beer" Json.Decode.string
        |> required "format" Json.Decode.int


customersDecoder : Json.Decode.Decoder (List Customer)
customersDecoder =
    Json.Decode.list customerDecoder


customerDecoder : Json.Decode.Decoder Customer
customerDecoder =
    Json.Decode.succeed Customer
        |> required "id" Json.Decode.int
        |> required "name" Json.Decode.string


encodeCustomers : List Customer -> Json.Encode.Value
encodeCustomers customers =
    Json.Encode.list encodeCustomer customers


encodeCustomer : Customer -> Json.Encode.Value
encodeCustomer customer =
    Json.Encode.object
        [ ( "id", Json.Encode.int customer.id )
        , ( "name", Json.Encode.string customer.name )
        ]


port storeOrders : Json.Encode.Value -> Cmd msg


port storeCustomers : Json.Encode.Value -> Cmd msg


port storePassword : String -> Cmd msg


port retrieveStockFromServer : String -> Cmd msg


port retrieveCustomersFromServer : String -> Cmd msg


port updateStock : (String -> msg) -> Sub msg


port updateCustomers : (String -> msg) -> Sub msg



---- PROGRAM ----


main : Program ( String, String ) Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
