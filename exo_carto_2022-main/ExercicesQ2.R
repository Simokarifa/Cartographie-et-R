#Mesurer la densité du réseau routier par commune
#Importation des library
library(sf)
#Lecture des données Commune
com=st_read("data/lot46.gpkg",layer="commune",
            quiet = TRUE)

route=st_read("data/lot46.gpkg",
              layer = "route", quiet = TRUE)

#intersection
route_inter=st_intersection #Selection et decoupage des routes qui inetrsect les communes
route_inter

#Calcul de la longueur
route_inter$longueur=st_length(route_inter)
route_inter

#Calcul longueur total de route commune
long_total=aggregate(x=list(long_total=route_inter$longueur),
                     by=list(INSEE_COM=route_inter$INSEE_COM), FUN="sum")
head(long_total)

#Joindre les longueur à la table des communes
comlong_route=merge(x=com,
      y=long_total,
      by="INSEE_COM",
      all.x=TRUE)
head(comlong_route)
#Calcul superficie com
comlong_route$surf=st_area(comlong_route)
comlong_route


#Conversion des unité des differents mesures
units(comlong_route$long_total)="km"
units(comlong_route$surf)="km2"

#Calcul des indicateurs
comlong_route$densite=comlong_route$long_total/comlong_route$surf
comlong_route$densePop=comlong_route$long_total/comlong_route$POPULATION

#Affichage
plot(comlong_route["densite"])
plot(comlong_route["densePop"])












