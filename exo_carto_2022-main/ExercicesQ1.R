#Importation des library
library(sf)
#Lecture des données Commune
com=st_read("data/lot46.gpkg",layer="commune")
rest=st_read("data/lot46.gpkg", layer = "restaurant", quiet = TRUE)
st_crs(com)
st_crs(rest)

#Quelles communes ont plus de 10 restaurants et moins de 1000 habitants ?
plot(st_geometry(com))
plot(st_geometry(rest), pch = 20, col = "red", add = TRUE, cex = .2)

#Creation de l'Intersection
inter=st_intersects(x=com,y=rest,parse=TRUE) #Creation d'une liste de restorant qui intersect les communes
inter
#Recuperation des restaurants et creation d'une colonne sur la couche commune
com$resto=sapply(inter, FUN=length)
com

#Creation de la selection de communes
com_sel<-com[com$resto > 10 & com$POPULATION < 1000, ]
com

#Jointure des non à la selection
com_sel$NOM_COM

#Affichage des communes
plot(st_geometry(com),col ="grey" )
plot(st_geometry(com_sel),col="red", add = TRUE)










