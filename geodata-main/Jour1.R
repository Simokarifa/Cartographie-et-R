#importation des données spatiales dans R
library(sf)
#Visualisation des objet de la données gpkg
st_layers("data/lot46.gpkg")
#Lecture de la données Commune
com=st_read("data/lot46.gpkg",layer="commune")

#======exportation
#Si c'est en gpkg
#st_write(obj = com, dsn = "data/com.gpkg", layer = "commune", delete_layer = TRUE)
#Si c'est en shape
#st_write(obj = com, "data/com.shp", layer_options = "ENCODING=UTF-8", delete_layer = TRUE)

#Visualisation des données attributaire
head(com)

#Visualisation des données spatiales
plot(com)

#Extraction de la colonne géometrie de la donnée
plot(st_geometry(com))

#Avoir acces au crs
st_crs(com)

#modiffication de la projection
reprojection=st_transform(com,"espg:2154")

#Selction des lignes
com[1:2,]
com[com$NOM_COM == "Gramat", ]

# selection de colonnes
com[com$NOM_COM == "Gramat", 1:4]


#Sélection des routes intesectant la commune de Gramat.
route <- st_read("data/lot46.gpkg", layer = "route", quiet = TRUE)
gramat <-  com[com$NOM_COM == "Gramat", ]
inter <- st_intersects(x = route, y = gramat, sparse = FALSE)
head(inter)
dim(inter)

##Selection des routes qui intersect la commune
route$intersect_gramat <- inter
plot(st_geometry(gramat), col = "lightblue") #affichage de la commune
plot(st_geometry(route), add = TRUE)#affichage de la route
plot(st_geometry(route[route$intersect_gramat, ]), col = "tomato", lwd = 2, add = TRUE) #creation de la colonne avec TRUE et affichage des ligne dont le resultat est TRUE

##Selection des routes que contient la commune
route$within_gramat <- st_within(route, gramat, sparse = FALSE)
plot(st_geometry(gramat), col = "lightblue")
plot(st_geometry(route), add = TRUE)
plot(st_geometry(route[route$within_gramat, ]), col = "tomato", lwd = 2, add = TRUE)

##Extraction des centroïdes
com_c <- st_centroid(com)
plot(st_geometry(com))
plot(st_geometry(com_c), add = TRUE, cex = 1.2, col = "red", pch = 20)


##Agregation des Polygones
dep_46 <- st_union(com)
plot(st_geometry(com), col = "lightblue")
plot(st_geometry(dep_46), add = TRUE, lwd = 2, border = "red")

#Agreger selon une colonne
com_u <- aggregate(x = com[,c("POPULATION")],
                   by = list(STATUT = com$STATUT),
                   FUN = "sum")
plot(com_u)


##Creation d'un Buffer
gramat_b <- st_buffer(x = gramat, dist = 5000)
plot(st_geometry(gramat_b), col = "lightblue", lwd=2, border = "red")
plot(st_geometry(gramat), add = TRUE, lwd = 2)

##En utilisant la fonction st_intersection() on va découper une couche par une autre.
# création d'une zone tampon autour du centroid de la commune de Gramat
# en utilisant le pipe
zone <- st_geometry(gramat) |>
  st_centroid() |>
  st_buffer(10000)
plot(st_geometry(com))
plot(zone, border = "red", lwd = 2, add = TRUE)

##Selection par location et decoupage de la zone
com_z <- st_intersection(x = com, y = zone)
plot(st_geometry(com))
plot(st_geometry(com_z), col="red", border="green", add=T)


##Affichage de zone decouper
plot(st_geometry(com_z))

#Creation d'une Grille reguliere autour d'une couche
grid <- st_make_grid(x = com, cellsize = 2500) #Creation de l'objet
grid <- st_sf(ID = 1:length(grid), geom = grid) #Ajout d'une table attri
plot(st_geometry(grid), col = "grey", border = "white") #affichage de la grille
plot(st_geometry(com), border = "grey50", add = TRUE) # Affichage de l'objet


#Compter le nombre de point dans un polygon
#selection des carreaux de la grille qui intersectent le département
inter <- st_intersects(grid, dep_46, sparse = FALSE)
grid <- grid[inter, ]
restaurant <- st_read("data/lot46.gpkg", layer = "restaurant", quiet = TRUE)
plot(st_geometry(grid), col = "grey", border = "white")
plot(st_geometry(restaurant), pch = 20, col = "red", add = TRUE, cex = .2)
inter <- st_intersects(grid, restaurant, sparse = TRUE)
length(inter)
#inter[795]

#affichage d'un exemple
plot(st_geometry(grid[795, ]))
plot(st_geometry(restaurant), add = T)
plot(st_geometry(restaurant[c(2716, 2718), ]), col = "red", pch = 19, add = TRUE)

#Comptage du nombre de point dans les carreaux
grid$nb_restaurant <-sapply(X = inter, FUN = length)
plot(grid["nb_restaurant"])


#Aggréger les valeurs de points dans des polygones
# Import du ficier
pop_raw <- read.csv("data/pop.csv")
# ajout d'un identifiant unique
pop_raw$id <- 1:nrow(pop_raw)
# transformation en objet sf
pop <- st_as_sf(pop_raw, coords = c("x", "y"), crs = 3035)
# gestion de la projection
pop <- st_transform(pop, st_crs(com))
# intersection
inter <- st_intersection(pop, com)
inter


#Agregation des deux couches
resultat <- aggregate(x = list(pop_from_grid = inter$Ind),
                      by = list(INSEE_COM = inter$INSEE_COM),
                      FUN = "sum")
head(resultat)

#Jointure et creation d'une nouvelle couche
com_res <- merge(com, resultat, by = "INSEE_COM", all.x = TRUE)
com_res


#Créer une matrice de distances
mat <- st_distance(x = com_c, y = com_c)
mat[1:5,1:5]
