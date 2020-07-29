# Outil de prise de commandes

Ceci est un petit outil de gestion des stocks, à utiliser sur une base Odoo.
Voici les fonctionnalités souhaitées :

- Afficher le stock en fonction de la date (voir l'évolution du stock)

- **Créer des factures rapidement** avec quelques raccourcis (ajout
  automatique des consignes sur les fûts, par exemple), générer automatiquement
  la facture et le BL, et pouvoir les imprimer.

- **Gérer des pré-commandes**. Deux options : commandes récurrentes (au mois,
  en fonction des types de brassin) et commandes ponctuelles. Il doit par
  exemple être possible de spécifier qu'un client veut toujours 3 cartons d'un
  brassin unique.

- **Envoi d'une sélection de commandes sur Odoo**, suppression des lignes qui ont
  été synchronisées pour ne laisser que ce qui doit rester. A cette étape, il
  faut pouvoir spécifier les bières qui sont disponibles pour que la synchro se
  fasse en fonction.

- **Gestion des brassins à venir** : indiquer une date ainsi que le code des
  articles prévus.  Une fois la mise en bouteille effective, il est possible
  d'envoyer l'information sur Odoo pour que les stocks soient pris en compte.

## Fonctionnalités pour une v2

- Calcul automatique de ce qui doit être déclaré aux douanes : en fonction de
  l'état des stock précédent, rentrer l'état des stocks actuel, et les calculs
  seront faits automatiquement.

- Gestion des commandes de fûts pour les particuliers. La source de données est
  le calendrier.

- Ajout d'un nouveau client automatiquement depuis notre interface.

- Gestion des tournées de livraison, en fonction des bons de commande qui ont
  été validés dans Odoo, affiche une carte avec les points à livrer, et permet
  d'organiser plusieurs tournées en fonction de la taille disponible dans le
  camion.

## Écrans

A. Un écran principal qui liste :

- les pré-commandes ;
- les commandes récurrentes avec leur date de dernière livraison ;
- l'état des stocks au moment T.

Sur cet écran il est possible d'ajouter une nouvelle commande.

B. Un écran avec les brassins à venir, avec :

- une liste des brassins à venir, leur date et les quantités qui seraient
  nécessaires
- la possibilité, pour chaque brassin, de dire : « C'est embouteillé ! » en
  précisant les quantités, et que l'info soit envoyée sur Odoo.

C. Un écran « Préparation de livraison »

Dans lequel on peut :

- dire quel sont les bières qui sont prêtes à partir ;
- sélectionner les commandes qui vont faire partie de la livraison ;
- un bouton « générer les bons de commande » qui envoie l'info à Odoo.

## Modèle de données

```Elm

type alias Model =
    {
    -- inputs
    orderInput : String -- could have two options here : add to orders or create invoice.
    , customerInput : String
    , incomingBrewsInput : String
    , serverPasswordInput : String

    -- need an input for the selected beers here.

    -- selections
    , selectedCustomer : Maybe Customer
    , currentDate : Time.Posix
    , editedItemNumber : Maybe Int
    , currentOrder : Maybe Order

    -- data
    , orders : List Order
    , customers : List Customer
    , serverPassword : Maybe String
    , realStock : Stock.Stock
    , incomingBrews : List OrderLine -- Could be List StockItem ? which could have a date added.
    , currentSeed : Seed
    , currentUuid : Maybe Uuid.Uuid

    }


type alias OrderLine =
    { quantity : Int
    , beer : Stock.StockItem
    }


type alias Order =
    { customer : Customer
    , lines : List OrderLine
    , date : Time.Posix -- rename to creationDate or createdOn.
    , localId : Maybe Uuid.Uuid
    , remoteId : Maybe Int
    -- could add here the type of the order, so it can repeat itself. In which case we would need to keep track of when the last time was ? hmm. maybe a lastSyncDate?

    }


type alias OrderId =
    { localId : Uuid.Uuid
    , remoteId : Int -- ID of the order in Odoo.
    }


type alias Customer =
    { id : Int
    , name : String
    }



type BeerFormat
    = Bottle75
    | Bottle33
    | Keg20L
    | NoFormat


-- Not sure if I should group them by name or not.

type alias Stock =
    Dict String (List StockItem)


type alias StockItem =
    { id : Int -- from Odoo
    , format : BeerFormat
    , available : Int -- available could be quantity?
    , code : String
    , name : String
    }

type alias Flags =
    { encodedOrders : String
    , encodedPassword : String
    , encodedCustomers : String
    , encodedStock : String
    , encodedIncomingBrews : String
    , seed : Int
    }


```
