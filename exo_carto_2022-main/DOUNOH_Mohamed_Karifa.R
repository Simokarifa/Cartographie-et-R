################################################################################
# Importer les couches d’information et les cartographier (4 points)
################################################################################ 
library(mapsf) #Importation de la librairy
library(sf)
st_layers("data/dvf.gpkg") #Verification du contenu du geopackage

#Importation des couches
com=st_read("data/dvf.gpkg",layer="com", quiet = TRUE)
route=st_read("data/dvf.gpkg",layer="route", quiet = TRUE)
rail=st_read("data/dvf.gpkg",layer="rail", quiet = TRUE)
parc=st_read("data/dvf.gpkg",layer="parc", quiet = TRUE)
apt=st_read("data/dvf.gpkg",layer="dvf", quiet = TRUE)

#=====================Cartographie de la region==========================
dev.off()

mf_export(com, "img/maCarte1.png", width = 800)   #Exportation de la carte

#Affichage des données sur la carte
mf_theme("darkula")
mf_map(x= com ,col="NA",border = "white",lwd=3)
mf_map(x = route, lwd = .5, col = "dimgray", add = TRUE)
mf_map(x = rail, lwd =2, col = "dimgray", add = TRUE)
mf_map(x = parc, pch = 20, cex = .3, col = "chartreuse4", add = TRUE)
mf_map(x = apt,pch = 20,cex=.7,col = "darkred",add = TRUE)

#Habillage de la carte 
mf_title("Ventes d'appartements à Vincennes et Montreuil(2016 à 2021)", cex = .7)
mf_arrow(adjust = com,col = "White")
mf_scale(1)
mf_credits(paste0("Source : BD CARTO®, IGN, 2021 &\n",
                  "© les contributeurs d’OpenStreetMap,2021",
                  "Etalab 2021,\n",
                  "Réalisation : DOUNOH M.K G2M P8"
                  ))

dev.off()

################################################################################
# Carte des prix de l’immobilier (4 points)
################################################################################ 
## Quelle est la forme de cette (nouvelle) distribution

#La distribution de la variable Prix etant normal la methode de discretisation utilisé est les quantiles
# Justification de la discrétisation (statistiques, boxplot, histogramme, 
# beeswarm...)

hist(apt$prix)
boxplot(apt$prix, horizontal = T)
summary(apt$prix)



dev.off()

mf_export(com, "img/maCarte2.png", width = 800)   #Exportation de la carte


#Cartographie des prix en ajoutant les autres couches
mf_theme("darkula")
mf_map(x= com ,col="NA",border = "white",lwd=3)
mf_map(x = route, lwd = .5, col = "dimgray", add = TRUE)
mf_map(x = rail, lwd =2, col = "dimgray", add = TRUE)
mf_map(x = parc, pch = 20, cex = .3, col = "chartreuse4", add = TRUE)

#Utilisation de la discretisation quantile sur la donné Prix de la couche apt
mf_map(x = apt, lwd = .5, pch = 20 , cex = .1, col = "red", var = "prix", 
       type = "choro", nbreaks = 6, breaks = "quantile", pal = "Dark Mint", add = TRUE )


#Habillage de la carte
mf_title("Prix de l'immobilier à Vincennes et Montreuil (2016-2021)", 
         cex = .5,
         tab = TRUE,
         pos = "right",
         line = 1)

mf_arrow(adjust = com,col = "White")
mf_scale(size=1,pos = c(656629,6863481),cex = 0.7,col = "white",unit = "km")
mf_credits(paste0("Source : BD CARTO®, IGN, 2021 &\n",
                  "© les contributeurs d’OpenStreetMap,2021",
                  "Etalab 2021,\n",
                  "Réalisation : DOUNOH M.K G2M P8"
))

dev.off()

################################################################################ 
# Prix de l’immobilier dans le voisinnage de la Mairie de Montreuil (4 points)
#######################
montreuil=st_as_sf(data.frame(x = 2.4410, y = 48.8624),
                      coords = c("x", "y"), crs = 4326)
montreuil93=st_transform(montreuil, "epsg:2154")
montreuilBuffer=st_buffer(x = montreuil93, dist = 500)


inter=st_intersection(apt,montreuilBuffer)
head(inter)
prixMedian=median(inter$prix)

cat(paste0("Le prix de l'immobilier dans un voisinnage de 500 mètres ",
           "autour de la mairie de Montreuil est de ", 
           round(prixMedian, 0), 
           " euros par m²"))
######################################################### 




################################################################################ 
# Utilisation d’un maillage régulier (4 points)
################################################################################ 
# Créer une grille régulière avec st_make_grid()
grid <- st_make_grid(x = com, cellsize = 250)
# Transformer la grille en objet sf avec st_sf()
grid <- st_sf(ID = 1:length(grid), geom = grid)

plot(st_geometry(grid), col = "grey", border = "white")
plot(st_geometry(com), border = "grey50", add = TRUE)

#intersection entre la commune et les grid
intergrid <- st_intersection(grid, com, sparse = FALSE)
grid<-grid[intergrid,]
plot(st_geometry(grid))
# selection des carreaux de la grille qui intersectent le département 
inter_app <- st_intersects(grid, dvf, sparse = TRUE)

grid$nb_transaction <- sapply(X = inter_app, FUN = length)
plot(grid["nb_transaction"])



# Calculez le prix median par carreau, voir chapitre 3.7.8
inter_grid_app <- st_intersection(grid, dvf)

prix_median <- aggregate(x = list(prix_median_carreau = inter_grid_app$prix), 
                         by = list(ID = inter_grid_app$ID), 
                         FUN = "median")

# dans https://rcarto.github.io/geomatique_avec_r/
# st_intersection(), aggregate(), merge()

# Selectionner les carreaux ayant plus de 10 transactions, voir chapitre 3.5
somme_app_careau <- merge(grid , prix_median, by = "ID", all.x=TRUE)
somme_app_careau
grid1<- somme_app_careau[somme_app_careau$nb_transaction > 10,]
grid1

# dans https://rcarto.github.io/geomatique_avec_r/


# Justification de la discrétisation (statistiques, boxplot, histogramme, 
# beeswarm...)

mf_export(com, "img/carte4.png", width = 800)
mf_theme("darkula")
# mf_map(x = grid1, lwd = .5, pch = 20 , cex = 1, col = "red", var = "prix_median_carreau", 
#        type = "choro", nbreaks = 5, breaks = "quantile", pal = "TealRose")

mf_map(x = grid1, lwd = .5, pch = 20 , cex = 1, col = "red", var = "prix_median_carreau", 
       type = "choro", nbreaks = 5, breaks = "equal", pal = "TealRose")

#mf_map(x = parc, pch = 20, cex = .7, col = "black", add = TRUE)
#mf_map(x = route, lwd = .01, col = "black",cex= 0.05, add = TRUE)
#mf_map(x = rail, lwd = 0.01, col = "black", add = TRUE)
#mf_map(grid1)

mf_map(x = dvf,lwd=0.5, pch = 20, cex = .1, col = "white", add = TRUE)

mf_map(x = com, col = NA, border = "white",  lwd = 2, add = TRUE)

dev.off()








