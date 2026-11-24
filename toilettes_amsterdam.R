library(sf)
library(ggplot2)

# --- 1. Paramètres -------------------------------------------------
url_toilettes <- paste0(
  "https://maps.amsterdam.nl/open_geodata/geojson_lnglat.php",
  "?KAARTLAAG=OPENBARE_TOILETTEN&THEMA=openbare_toiletten"
)
url_wijken <- paste0(
  "https://maps.amsterdam.nl/open_geodata/geojson_lnglat.php",
  "?KAARTLAAG=INDELING_WIJK_EXWATER&THEMA=gebiedsindeling"
)

dossier         <- "data"
fichier_geojson <- file.path(dossier, "toilettes_amsterdam.geojson")
fichier_wijken  <- file.path(dossier, "wijken_amsterdam.geojson")
fichier_csv     <- file.path(dossier, "toilettes_amsterdam.csv")

dir.create(dossier, showWarnings = FALSE, recursive = TRUE)

# --- 2. Téléchargement ---------------------------------------------
options(timeout = 120)  # secondes

telecharger <- function(url, destination) {
  tryCatch(
    download.file(url, destfile = destination, mode = "wb", quiet = TRUE),
    error = function(e) {
      stop("Échec du téléchargement : ", conditionMessage(e), call. = FALSE)
    }
  )
}

telecharger(url_toilettes, fichier_geojson)
telecharger(url_wijken, fichier_wijken)

# --- 3. Lecture des données ----------------------------------------
toilettes <- st_read(fichier_geojson, quiet = TRUE)
wijken    <- st_read(fichier_wijken, quiet = TRUE)

# Les coordonnées sont en WGS84 (longitude / latitude)
if (is.na(st_crs(toilettes))) st_crs(toilettes) <- 4326
if (is.na(st_crs(wijken)))    st_crs(wijken)    <- 4326

cat("Nombre d'objets téléchargés :", nrow(toilettes), "\n")
cat("Colonnes d'origine :", paste(names(toilettes), collapse = ", "), "\n")

# --- 4. Traduction des noms de colonnes ----------------------------
# Ordre d'origine : Soort, Prijs_per_gebruik, Dagen_geopend,
# Openingstijden, Omschrijving, Foto, SELECTIE, geometry
names(toilettes) <- c("type", "prix_par_usage", "jours_ouverture",
                      "horaires", "emplacement", "photo", "categorie",
                      "geometry")

# --- 5. Traduction des catégories ----------------------------------
traduction <- c(
  KRUL     = "Urinoir krul (fer forgé)",
  OPENBAAR = "Toilette publique",
  OVERIG   = "Autre",
  PARKEER  = "Toilette en parking",
  TOEGANG  = "Toilette à accès contrôlé",
  URILIFT  = "Urinoir escamotable",
  ZORG     = "Toilette en établissement de soins"
)

toilettes$categorie_fr <- unname(traduction[as.character(toilettes$categorie)])

# Vérification : aucune valeur ne doit être NA
print(table(toilettes$categorie, toilettes$categorie_fr, useNA = "ifany"))

# --- 6. Filtre des coordonnées nulles + export CSV ------------------
coords <- st_coordinates(toilettes)
toilettes_ok <- toilettes[which(coords[, "X"] != 0 & coords[, "Y"] != 0), ]

cat("Points retirés (lon ou lat = 0) :",
    nrow(toilettes) - nrow(toilettes_ok), "\n")

coords_ok <- st_coordinates(toilettes_ok)
toilettes_df <- cbind(
  st_drop_geometry(toilettes_ok),
  lon = coords_ok[, "X"],
  lat = coords_ok[, "Y"]
)

write.csv(toilettes_df, fichier_csv, row.names = FALSE, fileEncoding = "UTF-8")

print(head(toilettes_df))

# --- 7. Carte --------------------------------------------------------
bbox <- st_bbox(toilettes_ok)   # cadrage sur les toilettes

carte <- ggplot() +
  geom_sf(data = wijken, fill = "grey95", color = "grey70", linewidth = 0.2) +
  geom_sf(data = toilettes_ok, aes(color = categorie_fr), size = 1.5) +
  coord_sf(xlim = as.numeric(bbox[c("xmin", "xmax")]),
           ylim = as.numeric(bbox[c("ymin", "ymax")])) +
  labs(title = "Toilettes publiques – Amsterdam",
       color = "Catégorie") +
  theme_minimal()

print(carte)
