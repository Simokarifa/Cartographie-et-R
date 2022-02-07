#Utilisation de la library mapsf

#importation des données========================================================
library(sf)
# import des communes du Lot
com <- st_read("data/lot46.gpkg", layer = "commune", quiet = TRUE)
# import des départments français
dep <- st_read("data/lot46.gpkg", layer = "departement", quiet = TRUE)
# import des restaurants
resto <- st_read("data/lot46.gpkg", layer = "restaurant", quiet = TRUE)
# import des routes
route <- st_read("data/lot46.gpkg", layer = "route", quiet = TRUE)


#Cartographie de base des données====================================================
library(mapsf)
# Communes
mf_map(x = com, border = "white")
mf_map(x = dep, lwd = 2, col = NA, add = TRUE)
mf_map(x = route, lwd = .5, col = "ivory4", add = TRUE)
mf_map(x = resto, pch = 20, cex = .7, col = "darkred", add = TRUE)


#Carte de symboles proportionnels=================================================
# Communes
mf_map(x = com)
# Symboles proportionnels
mf_map(
  x = com,
  var = "POPULATION",
  type = "prop",
  leg_title = "Population totale\n(2015)"
)
# Titre
mf_title("Distribution de la population dans le Lot")

#Comparer plusieurs cartes==============================================================

# Afficher deux cartes en vis-à-vis
par(mfrow = c(1,2))
# Communes
mf_map(x = com, border = "grey90", lwd = .5)
# Population active occupée dans l'industrie, les hommes
mf_map(
  x = com,
  var = "IND_H",
  type = "prop",
  inches = .2,
  val_max = 600,
  leg_title = "Hommes",
  leg_val_cex = .5,
)
# ajout d'un titre
mf_title("Population active occupée dans l'industrie")

# Communes
mf_map(x = com, border = "grey90", lwd = .5)
# Population active occupée dans l'industrie, les femmes
mf_map(
  x = com,
  var = "IND_F",
  type = "prop",
  inches = .2,
  val_max = 600,
  leg_title ="Femmes",
  leg_val_cex = .5
)

# ajout d'un titre
mf_title("Population active occupée dans l'industrie")


#Carte choroplèthe=================================================================
# Densité de population (hab./km2) en utilisant la fonction sf::st_area()
com$DENS <- 1e6 * com$POP / as.numeric(st_area(com))
mf_map(
  x = com,
  var = "DENS",
  type = "choro",
  breaks = "quantile",
  pal = "Dark Mint",
  lwd = 1,
  leg_title = "Densité de population\n(habitants par km2)",
  leg_val_rnd = 0
)
mf_title("Distribution de la population dans le Lot (2018)")


#Discrétisations===============================================================
com$POP_ACT <-
  com$AGR_H + com$AGR_F +
  com$IND_H + com$IND_F +
  com$BTP_H + com$BTP_F +
  com$TER_H + com$TER_F
com$SHARE_ACT <- 100 * com$POP_ACT / com$POPULATION

d1 <- mf_get_breaks(com$SHARE_ACT, nbreaks = 6, breaks = "equal", freq = T)
d2 <- mf_get_breaks(com$SHARE_ACT, nbreaks = 6, breaks = "quantile")
d3 <- mf_get_breaks(com$SHARE_ACT, nbreaks = 6, breaks = "geom")
d4 <- mf_get_breaks(com$SHARE_ACT, breaks = "msd", central = FALSE)


#Palettes de couleurs
mf_map(x = com, var = "SHARE_ACT", type = "choro",
       breaks = d4, pal = "Blues 3")

#carton
mf_init(x = com_sel, theme = "agolalight", expandBB = c(0,.1,0,.5))
mf_map(com, add = TRUE)
mf_map(com_sel, col = "tomato4", border = "tomato1", lwd = 2, add = TRUE)
# Carton France
mf_inset_on(x = dep, pos = "topright", cex = .3)
mf_map(dep, lwd = .5, border= "grey90")
mf_map(com_sel, col = "tomato4", border = "tomato1", lwd = .5, add = TRUE)
mf_scale(size = 200, pos = "bottomleft", cex = .6, lwd = .5)
mf_inset_off()
# Carton Lot
mf_inset_on(x = com, pos = "bottomright", cex = .3)
mf_map(com, lwd = .5, border= "grey90")
mf_map(com_sel, col = "tomato4", border = "tomato1", lwd = .5, add = TRUE)
mf_scale(size = 20, pos = "bottomright", cex = .6, lwd = .5)
mf_inset_off()
# Carton Monde
mf_inset_on(x = "worldmap", pos = "topleft")
mf_worldmap(com_sel, land_col = "#cccccc",border_col = NA,
            water_col =  "#e3e3e3", col = "tomato4")
mf_inset_off()
mf_title("Cahors et ses environs")
mf_scale(1, pos = 'bottomleft')

#Carte 3D========================================================
library(linemap)
library(mapsf)
# import des communes du Lot
com <- st_read("data/lot46.gpkg", layer = "commune", quiet = TRUE)
com <- st_transform(com, 3035)
# import du data.frame de population
pop <- read.csv("data/pop.csv")
# cartographie
th <- mf_theme("green")
mf_init(com)
linemap(
  x = pop,
  var = "Ind",
  k = 10,
  threshold = 15,
  lwd = 1,
  col = th$bg,
  border = th$fg,
  add = T)
mf_title("Population du Lot, 2015")
mf_credits("Giraud & Pecout, 2021\nDonnées carroyées – Carreau de 1km, INSEE - 2019")




