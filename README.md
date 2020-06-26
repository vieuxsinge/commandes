# Commandes

Objectifs pour cet outil :

- Entrer des commandes rapidement
- Pouvoir les convertir en devis sur Odoo, voire en devis validés.
- Afficher l'état des stocks

## Exemple d'utilisation

Un client appelle, et me demande ce que j'ai en stock :

- L'outil m'affiche le nombre de cartons disponibles à la vente (= stock - bouteilles commandées)
- Je peux entrer facilement une commande pour un client

Une fois toutes les commandes entrées, je peux les convertir en devis dans Odoo, et imprimer les bons de commande. Idéalement, l'outil est capable de me préparer des tournées de livraisons en fonction de la capacité du camion et de l'emplacement des clients.

## Modèle

```elm

type BeerFormat =
  Bottle75
  | Bottle33
  | Keg20L

type alias Model = {
      beers : Dict String (BeerFormat, Int)
  , orders : List ({ customer : Customer, lines: List OrderLine, date: Posix.time })
  ,
}
```

## Vues

1. Stocks dispo

                        75cl  33cl  20L

//// Stock réel /////////////////////

Monstrueuse Normalité   -     2935  10
Nouveau Monde           19    16    4

//// Commandes ////////////////////////

Souffle Tropical    20       20        10
Nouveau Monde       20       30        5

//// Stock anticipé //////////////////

Souffle Tropical    3       2        1
Nouveau Monde       3       3        0


2. Entrer une commande

[ Chez Alain : 2xST33, 2xNM33, 2xEPT33, 2xST75, 2xNM75, 2xEPT75 ] [ OK ]

Commandes passées : [ Créer les bons de commande ]
<ul>
  <li>Chez Alain : 2xST33, 2xNM33, 2xEPT33, 2xST75, 2xNM75, 2xEPT75 (12 cartons)</li>
</ul>

Mettre une indication en rouge sur le champ de commandes si le stock est bas (limite à déterminer)
