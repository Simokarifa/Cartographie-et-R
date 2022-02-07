#Importation
library(mapsf)
# import des communes du Lot
com <- st_read("data/lot46.gpkg", layer = "commune", quiet = TRUE)
# import des départments français
dep <- st_read("data/lot46.gpkg", layer = "departement", quiet = TRUE)

#Créez une carte représentant la population active travaillant dans l’industrie==========
com$pop_active=com$IND_H+com$IND_F
com$pop_tot_active=com$AGR_H+com$AGR_F+
    com$IND_H+com$IND_F+
    com$BTP_H+com$BTP_F+
    com$TER_H+com$TER_F
bks=mf_get_breaks(com$pop_tot_active,nbreaks = 5,breaks = "quantile")
com

dev.off()
# Population active occupée dans l'industrie, les hommes
#par(mfrow = c(2,2 ))
mf_map(com)
mf_map(
  x = com,
  var = "pop_active",
  type = "prop",
  inches = .2,
  val_max = 600,
  leg_title = "Proportion de population",
  leg_val_cex = .5
)
# ajout d'un titre
mf_title("Population active dans l'industrie 2017")
# Fleche du nord
mf_arrow(adjust = com,"topright")
# ajout echelle
mf_scale(
  size = 20,
  lwd = 2,
  cex = 1.2,
)
# ajout source
mf_credits("source : IGN, 2020\n Réalisation: DOUNOH M.K\n Date:Janvier 2022 ")


mf_map(x=com,var="pop_tot_active",
       type = "choro",breaks = bks,
       leg_title = "Part de la population\n active par commune"
)

#Ajoutez les éléments d’habillage indispensables========================================
# ajout d'un titre
mf_title("Population active totale par Communes")
# Fleche du nord
mf_arrow(adjust = com,"topright")
# ajout echelle
mf_scale(
  size = 20,
  lwd = 2,
  cex = 1.2,
)
# ajout source
mf_credits("source : IGN, 2020\n Réalisation: DOUNOH M.K\n Date:Janvier 2022 ")

#Utilisez un thème personnalisé.



#Ajoutez un carton de localisation du Lot




#Exportez la carte au format PNG avec 800 pixels de large.



#Comment rendre la carte plus intelligible ? Allez-y !
