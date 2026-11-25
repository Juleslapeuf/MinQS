library(sf)
library(ggplot2)
library(osmdata)   # install.packages("osmdata") si absent

# =====================================================================
# Hôtels (OpenStreetMap) + Airbnb (Inside Airbnb) + toilettes publiques
# par quartier (wijk) d'Amsterdam
# Réutilise les fichiers déjà téléchargés par le script des toilettes.
# =====================================================================

# --- 1. Paramètres -------------------------------------------------
dossier <- "data"
dir.create(dossier, showWarnings = FALSE, recursive = TRUE)

fichier_toilettes <- file.path(dossier, "toilettes_amsterdam.geojson")
fichier_wijken    <- file.path(dossier, "wijken_amsterdam.geojson")
fichier_airbnb    <- file.path(dossier, "airbnb_amsterdam_listings.csv")
fichier_hotels    <- file.path(dossier, "hotels_amsterdam_osm.geojson")

# Instantané Inside Airbnb : la date change à chaque trimestre.
# Vérifier la dernière sur https://insideairbnb.com/get-the-data/
url_airbnb <- paste0(
  "https://data.insideairbnb.com/the-netherlands/north-holland/amsterdam/",
  "2026-09-16/visualisations/listings.csv"
)

# Types d'hébergement OSM comptés comme "hôtels"
# (ajouter par ex. "hostel", "guest_house", "apartment")
types_hotels <- c("hotel")

# Catégories de toilettes à exclure (colonne SELECTIE), ex. c("ZORG")
exclure_categories <- character(0)

# Distance (en mètres) sous laquelle un hôtel est "proche" d'une toilette
seuil_m <- 300

# Colonne du nom de quartier : NULL = détection automatique (voir étape 3)
col_wijk <- NULL

crs_metrique <- 28992   # RD New (mètres) : surfaces et distances

# Serveurs Overpass essayés dans l'ordre (le premier renvoie parfois des 502
# quand il est surchargé)
serveurs_overpass <- c(
  "https://overpass-api.de/api/interpreter",
  "https://overpass.private.coffee/api/interpreter"
)

options(timeout = 120)

# --- 2. Téléchargements --------------------------------------------
telecharger <- function(url, destination) {
  tryCatch(
    download.file(url, destfile = destination, mode = "wb", quiet = TRUE),
    error = function(e) {
      stop("Échec du téléchargement : ", conditionMessage(e), call. = FALSE)
    }
  )
}

if (!file.exists(fichier_wijken)) {
  telecharger(paste0(
    "https://maps.amsterdam.nl/open_geodata/geojson_lnglat.php",
    "?KAARTLAAG=INDELING_WIJK_EXWATER&THEMA=gebiedsindeling"
  ), fichier_wijken)
}
if (!file.exists(fichier_toilettes)) {
  telecharger(paste0(
    "https://maps.amsterdam.nl/open_geodata/geojson_lnglat.php",
    "?KAARTLAAG=OPENBARE_TOILETTEN&THEMA=openbare_toiletten"
  ), fichier_toilettes)
}
if (!file.exists(fichier_airbnb)) telecharger(url_airbnb, fichier_airbnb)

# --- 3. Quartiers --------------------------------------------------
wijken <- st_read(fichier_wijken, quiet = TRUE)
if (is.na(st_crs(wijken))) st_crs(wijken) <- 4326
wijken <- st_transform(wijken, crs_metrique)

cat("Colonnes des quartiers :", paste(names(wijken), collapse = ", "), "\n")

if (is.null(col_wijk)) {
  autres <- setdiff(names(wijken), attr(wijken, "sf_column"))
  noms   <- grep("naam|name|wijk", autres, ignore.case = TRUE, value = TRUE)
  noms   <- noms[!grepl("code|_id$|^id$", noms, ignore.case = TRUE)]
  col_wijk <- if (length(noms) > 0) noms[1] else autres[1]
}
cat("Colonne retenue pour le nom du quartier :", col_wijk, "\n")
print(head(wijken[[col_wijk]]))

wijken$nom_wijk    <- as.character(wijken[[col_wijk]])
wijken$surface_km2 <- as.numeric(st_area(wijken)) / 1e6   # hors eau

# Rattache chaque point à un quartier (indice de ligne dans `wijken`)
affecter <- function(points, zones) {
  idx <- st_within(points, zones)
  vapply(idx, function(i) if (length(i) > 0) i[1] else NA_integer_, integer(1))
}

# Nombre de points par quartier
compter <- function(wijk_id) {
  tabulate(wijk_id[!is.na(wijk_id)], nbins = nrow(wijken))
}

# Distance (m) de chaque point à l'élément le plus proche de `cible`
distance_plus_proche <- function(points, cible) {
  i <- st_nearest_feature(points, cible)
  as.numeric(st_distance(points, cible[i, ], by_element = TRUE))
}

# --- 4. Toilettes publiques ----------------------------------------
toilettes <- st_read(fichier_toilettes, quiet = TRUE)
if (is.na(st_crs(toilettes))) st_crs(toilettes) <- 4326

# Même filtre que précédemment : on retire les points en (0, 0)
coords <- st_coordinates(toilettes)
toilettes <- toilettes[which(coords[, "X"] != 0 & coords[, "Y"] != 0), ]

if ("SELECTIE" %in% names(toilettes) && length(exclure_categories) > 0) {
  toilettes <- toilettes[!(toilettes$SELECTIE %in% exclure_categories), ]
}
toilettes <- st_transform(toilettes, crs_metrique)
toilettes$wijk_id <- affecter(toilettes, wijken)
cat("Toilettes retenues :", nrow(toilettes), "\n")

# --- 5. Hôtels (OpenStreetMap via Overpass) -------------------------
# Le résultat est mis en cache : l'API Overpass peut être lente.
# Interroge Overpass en changeant de serveur et en réessayant après une pause
# si une requête échoue (502, 504, délai dépassé...).
interroger_overpass <- function(requete, serveurs = serveurs_overpass,
                                max_tentatives = 3, pause_s = 10) {
  for (tentative in seq_len(max_tentatives)) {
    for (serveur in serveurs) {
      set_overpass_url(serveur)
      cat("Overpass :", serveur, "- tentative", tentative, "\n")
      resultat <- tryCatch(osmdata_sf(requete), error = function(e) {
        message("  échec : ", conditionMessage(e))
        NULL
      })
      if (!is.null(resultat)) return(resultat)
    }
    if (tentative < max_tentatives) Sys.sleep(pause_s * tentative)
  }
  stop("Tous les serveurs Overpass ont échoué. Réessayez plus tard, ou ",
       "déposez un export GeoJSON dans ", fichier_hotels, call. = FALSE)
}

if (!file.exists(fichier_hotels)) {
  bb <- as.numeric(st_bbox(st_transform(wijken, 4326)))  # xmin, ymin, xmax, ymax
  requete <- opq(bbox = bb, timeout = 180)
  requete <- add_osm_feature(requete, key = "tourism", value = types_hotels)
  osm <- interroger_overpass(requete)
  
  # Les hôtels peuvent être des points, des bâtiments (polygones) ou
  # des multipolygones : on ramène tout à un point en projection métrique.
  normaliser <- function(x) {
    if (is.null(x) || nrow(x) == 0 || !"tourism" %in% names(x)) return(NULL)
    if (!"name" %in% names(x)) x$name <- NA_character_
    x <- x[x$tourism %in% types_hotels, ]
    if (nrow(x) == 0) return(NULL)
    g <- st_transform(st_geometry(x), crs_metrique)
    g <- suppressWarnings(st_point_on_surface(g))
    st_sf(osm_id = as.character(x$osm_id), nom = as.character(x$name),
          type = as.character(x$tourism), geometry = g)
  }
  liste <- Filter(Negate(is.null), list(
    normaliser(osm$osm_points),
    normaliser(osm$osm_polygons),
    normaliser(osm$osm_multipolygons)
  ))
  if (length(liste) == 0) stop("Aucun hôtel renvoyé par OpenStreetMap.")
  hotels_osm <- do.call(rbind, liste)
  st_write(st_transform(hotels_osm, 4326), fichier_hotels,
           delete_dsn = TRUE, quiet = TRUE)
}

hotels <- st_transform(st_read(fichier_hotels, quiet = TRUE), crs_metrique)
if (!all(st_geometry_type(hotels) == "POINT")) {   # export manuel : bâtiments
  st_geometry(hotels) <- suppressWarnings(st_point_on_surface(st_geometry(hotels)))
}
hotels$wijk_id <- affecter(hotels, wijken)
hotels <- hotels[!is.na(hotels$wijk_id), ]    # exclut les communes voisines
cat("Hôtels dans les quartiers :", nrow(hotels), "\n")

# --- 6. Airbnb (Inside Airbnb) --------------------------------------
airbnb <- read.csv(fichier_airbnb, stringsAsFactors = FALSE, encoding = "UTF-8")
airbnb <- airbnb[!is.na(airbnb$latitude) & !is.na(airbnb$longitude), ]
airbnb <- st_as_sf(airbnb, coords = c("longitude", "latitude"), crs = 4326)
airbnb <- st_transform(airbnb, crs_metrique)
# Pour ne garder que les logements entiers :
# airbnb <- airbnb[airbnb$room_type == "Entire home/apt", ]
airbnb$wijk_id <- affecter(airbnb, wijken)
airbnb <- airbnb[!is.na(airbnb$wijk_id), ]
cat("Annonces Airbnb dans les quartiers :", nrow(airbnb), "\n")

# --- 7. Indicateurs par quartier ------------------------------------
wijken$n_hotels    <- compter(hotels$wijk_id)
wijken$n_airbnb    <- compter(airbnb$wijk_id)
wijken$n_toilettes <- compter(toilettes$wijk_id)

wijken$hotels_km2    <- wijken$n_hotels    / wijken$surface_km2
wijken$airbnb_km2    <- wijken$n_airbnb    / wijken$surface_km2
wijken$toilettes_km2 <- wijken$n_toilettes / wijken$surface_km2
wijken$hotels_par_toilette <- ifelse(wijken$n_toilettes > 0,
                                     wijken$n_hotels / wijken$n_toilettes, NA)

# Lien hôtels <-> toilettes : distance à la toilette la plus proche
hotels$dist_toilette_m <- distance_plus_proche(hotels, toilettes)
airbnb$dist_toilette_m <- distance_plus_proche(airbnb, toilettes)

par_wijk <- function(valeurs, wijk_id, fonction) {
  as.numeric(tapply(valeurs, factor(wijk_id, levels = seq_len(nrow(wijken))),
                    fonction))
}
wijken$dist_med_hotel_m    <- par_wijk(hotels$dist_toilette_m, hotels$wijk_id, median)
wijken$part_hotels_proches <- par_wijk(hotels$dist_toilette_m <= seuil_m,
                                       hotels$wijk_id, mean)
wijken$dist_med_airbnb_m   <- par_wijk(airbnb$dist_toilette_m, airbnb$wijk_id, median)

cat("\nDistance médiane hôtel -> toilette la plus proche :",
    round(median(hotels$dist_toilette_m)), "m\n")
cat("Part des hôtels à moins de", seuil_m, "m d'une toilette :",
    round(100 * mean(hotels$dist_toilette_m <= seuil_m)), "%\n")
cat("Corrélation de Spearman hôtels/km2 ~ toilettes/km2 :",
    round(cor(wijken$hotels_km2, wijken$toilettes_km2, method = "spearman"), 2), "\n")

# --- 8. Exports -----------------------------------------------------
tableau <- st_drop_geometry(wijken)
tableau <- tableau[, c("nom_wijk", "surface_km2", "n_hotels", "hotels_km2",
                       "n_airbnb", "airbnb_km2", "n_toilettes", "toilettes_km2",
                       "hotels_par_toilette", "dist_med_hotel_m",
                       "part_hotels_proches", "dist_med_airbnb_m")]
write.csv(tableau, file.path(dossier, "indicateurs_par_wijk.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

hotels_wgs <- st_transform(hotels, 4326)
hotels_df  <- cbind(st_drop_geometry(hotels_wgs),
                    lon = st_coordinates(hotels_wgs)[, "X"],
                    lat = st_coordinates(hotels_wgs)[, "Y"])
write.csv(hotels_df, file.path(dossier, "hotels_amsterdam.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

# --- 9. Cartes ------------------------------------------------------
# Échelle de couleurs en racine carrée (les densités sont très asymétriques).
# L'argument s'appelle `transform` depuis ggplot2 3.5.0, `trans` avant.
echelle <- function(nom) {
  args <- list(name = nom, option = "magma", direction = -1, na.value = "grey90")
  if (packageVersion("ggplot2") >= "3.5.0") args$transform <- "sqrt" else args$trans <- "sqrt"
  do.call(scale_fill_viridis_c, args)
}

# Carte 1 : densité d'hôtels + toilettes
carte1 <- ggplot() +
  geom_sf(data = wijken, aes(fill = hotels_km2), color = "white", linewidth = 0.2) +
  geom_sf(data = toilettes, color = "#1b6ca8", size = 0.7, alpha = 0.8) +
  echelle("Hôtels / km²") +
  labs(title = "Densité d'hôtels et toilettes publiques – Amsterdam",
       subtitle = "Points bleus : toilettes publiques",
       caption = "Hôtels : © OpenStreetMap contributors · Toilettes : Gemeente Amsterdam") +
  theme_minimal()

# Carte 2 : densité d'annonces Airbnb
carte2 <- ggplot() +
  geom_sf(data = wijken, aes(fill = airbnb_km2), color = "white", linewidth = 0.2) +
  echelle("Annonces / km²") +
  labs(title = "Densité d'annonces Airbnb – Amsterdam",
       caption = "Inside Airbnb (CC BY 4.0)") +
  theme_minimal()

# Carte 3 : hôtels selon la distance à la toilette publique la plus proche
hotels$classe_dist <- cut(hotels$dist_toilette_m,
                          breaks = c(-Inf, 150, seuil_m, 500, Inf),
                          labels = c("< 150 m", paste0("150 – ", seuil_m, " m"),
                                     paste0(seuil_m, " – 500 m"), "> 500 m"))
carte3 <- ggplot() +
  geom_sf(data = wijken, fill = "grey95", color = "grey75", linewidth = 0.2) +
  geom_sf(data = toilettes, shape = 17, color = "grey20", size = 0.9) +
  geom_sf(data = hotels, aes(color = classe_dist), size = 1.4) +
  scale_color_viridis_d(name = "Distance à la\ntoilette la plus proche",
                        option = "plasma", end = 0.9) +
  labs(title = "Hôtels et accès aux toilettes publiques",
       subtitle = "Triangles gris : toilettes publiques") +
  theme_minimal()

print(carte1)
print(carte2)
print(carte3)