# Toilettes publiques d'Amsterdam

Script R qui télécharge les données ouvertes des toilettes publiques d'Amsterdam,
traduit les noms de colonnes et les catégories en français, exporte un CSV
et trace une carte avec les quartiers de la ville en fond.

## Données

- **Source :** Gemeente Amsterdam, [Maps Data](https://maps.amsterdam.nl/open_geodata/),
  couche « Openbare toiletten » (format GeoJSON, coordonnées WGS84).
- **Fond de carte :** couche « Amsterdam Wijken excl. water » (quartiers, sans les plans d'eau),
  même source.
- Le téléchargement de ces données vaut acceptation des
  [conditions d'utilisation](https://maps.amsterdam.nl/open_geodata/terms.php) de la Gemeente Amsterdam.
  Les données ne sont pas incluses dans ce dépôt : elles sont téléchargées à chaque exécution du script.

## Prérequis

- R (version 4.x recommandée)
- Packages : `sf`, `ggplot2`

```r
install.packages(c("sf", "ggplot2"))
```

## Utilisation

1. Ouvrir `toilettes-amsterdam.Rproj` dans RStudio (le répertoire de travail devient la racine du projet).
2. Lancer le script :

```r
source("R/toilettes_amsterdam.R")
```

Le script :

1. télécharge les deux couches GeoJSON dans `data/` ;
2. renomme les colonnes et ajoute une colonne `categorie_fr` ;
3. retire les points dont la longitude ou la latitude vaut 0 ;
4. exporte `data/toilettes_amsterdam.csv` (avec colonnes `lon` et `lat`) ;
5. affiche la carte.

## Structure du dépôt

```
toilettes-amsterdam/
├── R/
│   └── toilettes_amsterdam.R
├── data/                      # créé par le script, ignoré par Git
├── .gitignore
├── README.md
└── toilettes-amsterdam.Rproj
```

## Colonnes du jeu de données

| Colonne d'origine   | Colonne renommée   | Contenu                                  |
|---------------------|--------------------|------------------------------------------|
| `Soort`             | `type`             | Type d'équipement (texte libre, en néerlandais) |
| `Prijs_per_gebruik` | `prix_par_usage`   | Prix par usage                           |
| `Dagen_geopend`     | `jours_ouverture`  | Jours d'ouverture                        |
| `Openingstijden`    | `horaires`         | Horaires d'ouverture                     |
| `Omschrijving`      | `emplacement`      | Description de l'emplacement             |
| `Foto`              | `photo`            | Nom du fichier photo                     |
| `SELECTIE`          | `categorie`        | Code de catégorie                        |
| `geometry`          | `geometry`         | Géométrie (point)                        |

## Catégories

| Code       | Libellé français                    |
|------------|-------------------------------------|
| `KRUL`     | Urinoir krul (fer forgé)            |
| `OPENBAAR` | Toilette publique                   |
| `OVERIG`   | Autre                               |
| `PARKEER`  | Toilette en parking                 |
| `TOEGANG`  | Toilette à accès contrôlé           |
| `URILIFT`  | Urinoir escamotable                 |
| `ZORG`     | Toilette en établissement de soins  |

Les traductions de `TOEGANG` et `ZORG` sont des interprétations. Le fichier `Legenda.csv`
disponible sur la [carte « Openbare toiletten »](https://maps.amsterdam.nl/openbare_toiletten/)
décrit les catégories officielles : à consulter pour vérifier ou corriger le vecteur `traduction`
dans le script.

## Licence

À définir. Les données restent soumises aux conditions de la Gemeente Amsterdam.
