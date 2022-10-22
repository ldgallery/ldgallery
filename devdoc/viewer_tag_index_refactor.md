---
title: "Viewer: tag index refactoring"
author: Pacien
date: 2022-10-15 (v1)
---

# Issue

The internal tag index (generated from `index.json`) of the viewer has been
written without proper specifications and went through a lot of changes since
its first implementation.

This results in a lot of coupling and surprises due to unspecified behaviours.

# Goals

Make this simpler.


# Current state of affairs

## galleryStore.ts

galleryStore -> point d'entrée, qui charge le index.json
json mis dans galleryIndex

Deux types d'indexation :
- indexTags : indexe que les tags (tout)
  - tags (and all tag parts) mapping to items
  - +other things?
  - rootPart ? ne semble pas être utilisé, peut-être à simplifier
  - children : tag enfants (sous une catégorie de tags)

- indexTagCategories : index des catégories pour l'affichage à gauche


TODO:
- faire un glossaire pour les tags et catégories de tags, etc… + renommage
- à voir pour les tags à >2 parties : `a:b:c`, b child de a, mais pas c
- éviter de relancer la recherche à chaque fois (pousse nouvelle url, etc)
- retirer `rootPart`


## indexSearch.ts

- parse.ts : parse une requête de recherche en composants avec opérations
- indexSearch : mange cela
- priorités : séparation par opération, l'ordre dépend de l'opération
- `_aggregateAll` définit la priorité opératoire
- indexSearch ne cherche pas dans l'index
- searchTags cherche dans l'index de indexFactory :
  - strict : tag en entier, sinon accepte mot infix

call stack pour une recherche
- taginput -> string
- -> indexFactory.searchTags (non stricte, "fuzzy search")
- select box (affichage), auto-completion, choix stocké dans le composant
- objet TagSearch référencé par composant tag input
- bouton search -> génère la nouvelle url avec les tags, affiche le résultat

call stack affichage de l'url
- url mise à jour
- reparse de la requête depuis l'URL
- refait la recherche (en mode stricte) :
  - stock le résultat dans galleryStore dans currentSearch
  - stock aussi le résultat dans le layoutLeft
    - (propagation vers tagInput + propositions)

- Affichage dans la partie de droite à partir de currentSearch :
- si query pas vide :
  - GallerySearch utilise currentSearch déjà recherché
  - indexSearch de indexSearch.ts pour les opérations
  - filtrage par le répertoire courant
  - affiche les résultats (et nombre résultats des autres dossiers)

Notes
- fuzzy search global, pas restreint au répertoire courant
- fuzzy normalise

# Implementation plan

- Write new specs, decoupling obviously different parts.
- Maybe implement the stuff in PureScript instead of TS, and have actual tests.
- Run some performance benchmarks just due to curiosity.


