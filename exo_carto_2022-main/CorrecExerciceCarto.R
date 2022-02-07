library(mapsf)

# Import des données
com <- st_read("data/lot46.gpkg", layer = "commune", quiet = TRUE)
dep <- st_read("data/lot46.gpkg", layer = "departement", quiet = TRUE)
fic=st_layers("data/lot46.gpkg")

# 1. Créez une carte représentant la population active travaillant dans l’industrie.
# Nombre d'actifs dans l'industrie
com$IND <- com$IND_F + com$IND_H
# Nombre total d'actifs
com$ACT <- com$AGR_H + com$AGR_F +
  com$IND_H + com$IND_F +
  com$BTP_H + com$BTP_F +
  com$TER_H + com$TER_F
# Part des actifs travaillant dans l'industrie
com$PART_ACT_IND <- 100 * com$IND / com$ACT

# Cartographie du nombre total de travailleurs de l'industrie
mf_map(com)
mf_map(x = com, var = "IND", type = "prop",
       leg_title = "Nombre d'actifs\ntravaillant dans\nl'industrie")
mf_title("Répartition des travailleurs de l'industrie")
#### > Utiliser /n pour sauter une ligne

# Cartographie de la part des actifs travaillant dans l'industrie
## Quelle est la forme de la distribution que nous voulons cartographier ?
hist(com$PART_ACT_IND)
boxplot(com$PART_ACT_IND, horizontal = T)
summary(com$PART_ACT_IND)

#### > Seules 2 communes ont 100% de travailleurs dans l'industrie
#### Ces communes ont moins de 15 actifs

# Sélection des communes ayant plus de 15 actifs
com_sel <- com[com$ACT > 15, ]

## Quelle est la forme de cette (nouvelle) distribution
hist(com_sel$PART_ACT_IND)
boxplot(com_sel$PART_ACT_IND, horizontal = T)
summary(com_sel$PART_ACT_IND)

# Creation d'un vecteur contenant les limites de classes en
# utilisant la méthode des quantiles
bks <- mf_get_breaks(com_sel$PART_ACT_IND, nbreaks = 5,
                     breaks = "quantile")
hist(com_sel$PART_ACT_IND, bks)


# 3. Utilisez un thème personnalisé.

th <- mf_theme("green", mar = c(0,0,1.5,0))


# 5. Exportez la carte au format PNG avec 800 pixels de large.
mf_export(com, theme = th, width=800, type = "cairo")


# Cartographie
mf_map(x = com_sel,
       var = "PART_ACT_IND",
       type = "choro",
       breaks = bks,         # Utilisation des bornes de classes créées précédement
       leg_val_rnd = 0,      # arrondir les valeurs dans la légende
       pal = "Red-Yellow",   # Utilisation d'une palette de couleur
       leg_title = "Part des actifs\ntravaillant dans\nl'industrie")
mf_title("Répartition des actifs travaillant dans l'industrie - 2017")



# 2. Ajoutez les éléments d’habillage indispensables.

# Titre
mf_title("Répartition des actifs travaillant dans l'industrie - 2017")

# Echelle
mf_scale(5)

# Sources
mf_credits(paste0("Admin Express COG Carto 3.0, IGN - 2021 & ",
                  "BD CARTO® 4.0, IGN - 2021 ; Recensements harmonisés - ",
                  "Séries départementales et communales, INSEE - 2020\n",
                  "Auteurs : T. Giraud & H. Pecout, 2022"))
# Texte d'information
text(x = 538000, y = 6396101,
     label = "* Ne sont représentées\n  que les communes de\n  plus de 15 actifs",
     cex = .6, col = th$fg, adj = c(0,0), xpd = TRUE)


# 4. Ajoutez un carton de localisation du Lot
mf_inset_on(fig = c(.8,0.98,0.1,0.3))
mf_map(dep, lwd = .1)
mf_map(com, border = NA, add = T, col = th$fg)
box(col =th$fg, lwd = .5)
mf_inset_off()


# 6. Comment rendre la carte plus intelligible ? Allez-y !

mf_annotation(x = com[com$NOM_COM=="Biars-sur-Cère",],
              txt = "Andros",
              col_arrow = th$fg, halo = T, cex = 1)
mf_annotation(x = com[com$NOM_COM=="Figeac",],
              txt = "Industrie\naéronautique",
              col_arrow = th$fg, pos = "bottomright", halo = T, cex = 1)
mf_annotation(x = com[com$NOM_COM=="Gramat",],
              txt = "La Quercynoise",
              col_arrow = th$fg, pos = "topleft", s = 1, halo = T, cex = 1)

dev.off()

