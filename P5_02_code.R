library('reshape2')
library("FactoMineR")
library("factoextra")
library("tidyverse")
library("ggdendro")
library("corrplot")



#'Fichier population :

population_2018 <- read.csv("population_2018.csv", sep = ",")
population_2015 <- read.csv("population_2015.csv", sep = ",")

#'Fichier bilan alimentaire :
bilan_alim_veg <- read.csv("bilan_alim_veg.csv",sep = ",")
bilan_alim_anim <- read.csv("bilan_alim_anim.csv",sep = ",")

#'Fichier PIB:
pib <- read_csv("pib_2018.csv")
pib_pivot <- dcast(pib, Zone  ~ Produit,sum, value.var = "Valeur" )
pib_pivot <-rename(pib_pivot, "PIB" = "PIB par habitant, ($ PPA internationaux constants de 2011)")

#'Fichier importation :
importation <- read.csv("importation.csv")
importation_pivot <- dcast(importation, Zone  ~ Année,sum, value.var = "Valeur" )
importation_pivot$diff_import <- ((importation_pivot$`2018` - importation_pivot$`2015`) / importation_pivot$`2015`) * 100


# Traitement des valeurs "inf" et "NaN"
for (i in 1:length(importation_pivot$diff_import )) {
  if (importation_pivot$diff_import[i] == "Inf") {
    importation_pivot$diff_import[i] = importation_pivot$`2018`[i]
  }
  if (importation_pivot$diff_import[i] == "NaN") {
    importation_pivot$diff_import[i] = 0
  }
  
}

importation_pivot <- select(importation_pivot, "Zone", "diff_import")

#'Fichier élevage :
elevage <- read.csv("elevage.csv")
elevage_pivot <- dcast(elevage, Zone  ~ Produit,sum, value.var = "Valeur" )



#'Traitement du fichier Population :
population_pivot_2018 <- dcast(population_2018, Zone + Année ~ Élément,sum, value.var = "Valeur" )
population_pivot_2015 <- dcast(population_2015, Zone + Année ~ Élément,sum, value.var = "Valeur" )

population_pivot_2015$`Population totale` <- population_pivot_2015$`Population totale`* 1000
population_pivot_2018$`Population totale` <- population_pivot_2018$`Population totale`* 1000


population <- left_join(population_pivot_2018,population_pivot_2015,by="Zone")
population <- select(population, "Zone","Population totale.x", "Population totale.y")
population$diff_pop <- ((population$`Population totale.x` - population$`Population totale.y`) /population$`Population totale.y`)* 100
population <- select(population, "Zone", "Population totale.x", "diff_pop")

#'Traitement du fichier Bilan alimentaire :

bilan_alim_veg_pivot <- dcast(bilan_alim_veg, Zone + Produit ~ Élément,sum, value.var = "Valeur" )
bilan_alim_anim_pivot <- dcast(bilan_alim_anim, Zone + Produit ~ Élément,sum, value.var = "Valeur" )

bilan_veg <- aggregate(bilan_alim_veg_pivot[,3:5], by=list(Zone=bilan_alim_veg_pivot$Zone), FUN=sum)
bilan_anim <- aggregate(bilan_alim_anim_pivot[,3:5], by=list(Zone=bilan_alim_anim_pivot$Zone), FUN=sum)

#'Jointure des dataframes "bilan_veg" et "bilan_anim"
df_general <- left_join(bilan_veg, bilan_anim,by="Zone")

#'Jointure dataframe population avec dataframe génerale
df_general <- left_join(df_general, population,by="Zone")

#'Jointure du dataframe PIB avec dataframe générale
df_general <- left_join(df_general, pib_pivot,by="Zone")

#'Jointure du dataframe importation  avec le dafaframe générale
df_general <- left_join(df_general, importation_pivot,by="Zone")

#'Jointure du dataframe elevage avec le dataframe générale
df_general <- left_join(df_general, elevage_pivot,by="Zone")

#'Création des variables 

df_general$`Disponibilité alimentaire (kcal/an)` <- (df_general$`Disponibilité alimentaire (Kcal/personne/jour).x` +
                                                   df_general$`Disponibilité alimentaire (Kcal/personne/jour).y`)*
                                                   df_general$`Population totale.x` * 365



df_general$`Disponibilité alimentaire en quantité (kg/an)` <-(df_general$`Disponibilité alimentaire en quantité (kg/personne/an).x`+
                                                                df_general$`Disponibilité alimentaire en quantité (kg/personne/an).y`)*
                                                                df_general$`Population totale.x`
                  
df_general$`Disponibilité de protéines en quantité (kg/an)` <- ((df_general$`Disponibilité de protéines en quantité (g/personne/jour).x`+
                                                                  df_general$`Disponibilité de protéines en quantité (g/personne/jour).y`)
                                                                 * df_general$`Population totale.x`*365)

df_general$ratio_prot <- ((df_general$`Disponibilité de protéines en quantité (g/personne/jour).y`)*365 *df_general$`Population totale.x`)/ df_general$`Disponibilité de protéines en quantité (kg/an)`


df_general$diff_pop <- (df_general$diff_pop / df_general$`Population totale.x`)*100 

df_general$`Disponibilité de protéines en quantité (g/habitant)` <- ((df_general$`Disponibilité de protéines en quantité (g/personne/jour).x`+
                                                                         df_general$`Disponibilité de protéines en quantité (g/personne/jour).y`)
                                                                      *365)

df_general$`Disponibilité alimentaire (kcal/habitant)` <- (df_general$`Disponibilité alimentaire (Kcal/personne/jour).x` +
                                                       df_general$`Disponibilité alimentaire (Kcal/personne/jour).y`)*365

row.names(df_general) <- df_general$Zone

#'Selection des variables pour le dataframe générale :
df_general <- select(df_general, "diff_pop", "ratio_prot", "Disponibilité alimentaire (kcal/habitant)", "Disponibilité de protéines en quantité (g/habitant)", "diff_import", "PIB", "Poulets")

# suppréssion des valeurs nulles 
df_general <-drop_na(df_general)




#'classification hierarchique 

#'Suppréssion des pays étant de gros producteurs de poulets 
row.names.remove <- c("Brésil", "Chine, continentale", "Chine - RAS de Macao",
                      "Chine - RAS de Hong-Kong", "Chine", "États-Unis d'Amérique", "Pologne", "Fédération de Russie", "France")
new_df <- df_general[!(row.names(df_general) %in% row.names.remove), ]

new_df <- select(new_df, "diff_pop", "ratio_prot", "Disponibilité alimentaire (kcal/habitant)", "Disponibilité de protéines en quantité (g/habitant)", "diff_import", "PIB", "Poulets")


#Centrage réduction des données
bilan_alim.cr <- scale(new_df, center=T, scale=T)

#Matrice des distances entre individus
d.bilan_alim <- dist(bilan_alim.cr)

#Cah - critère de Ward
#Method = « ward.D2 » correspond au vrai critère de Ward
#utilisant le carré de la distance
cah.ward <- hclust(d.bilan_alim,method="ward.D2")

# Affichage du dendogramme
plot(cah.ward, labels = FALSE, main = "Dendrogramme")

#Dendrogramme avec matérialisation des groupes
rect.hclust(cah.ward,k=5)

#Découpage en 5 groupes
groupes.cah <- cutree(cah.ward,k=5)

#'Liste des groupes
groupes <- data.frame(sort(groupes.cah))
write.csv2(groupes, file = 'groupe_pays.csv')
print(groupes)

#Fonction de calcul des stats
stat.comp <- function(x,y){
  #nombre de groupes
  K <- length(unique(y))
  #moyennes conditionnelles
  mk <- tapply(x,y,mean)
  #moyennes + prop. variance expliquée
  result <- c(mk)
  #nommer les élements du vecteur
  names(result) <- c(paste("G",1:K))
  #renvoyer le vecteur résultat
  return(result)
}







print(sapply(new_df,stat.comp,y=groupes.cah))

mean_study <- sapply(new_df,stat.comp,y=groupes.cah)
write.csv2(mean_study, file = 'centroïde.csv')

#'Le cluster 1 est majoritairement composé  de pays d’Afrique subsaharienne on constate que ces pays ont un PIB assez faible.
#' une disponibilité en protéines qui est aussi la plus faible de tout les clusters et la plus inégalement répartie  
#’ en effet les protéines d’origines animales ne représentent, en moyenne, que 28% de l’ensemble de leur disponibilité en protéines.

#'Le cluster 2 est constitué principalement de pays occidentaux on remarque que les pays qui constituent ce cluster ont un PIB relativement élevé.
#’ une disponibilité alimentaire en kcal et en protéines aussi élevé 
#’ cette disponibilité en protéines est équilibré puisque 51% de la disponibilité en protéines est d’origine animale


#'Le cluster   3 est composé d’une multitude de pays en voie de développement avec notamment beaucoup de petites îles. 
#'On remarque que ces pays ont un PIB plus important que ceux du cluster 1 mais qui reste tout de même faible .
#'Leur disponibilité  en terme de protéines est assez équilibré avec 47% de protéines d’origine animale 
#’ce qui les démarques des autres cluster c’est leur croissance démographique qui est la plus forte.


#'Ensuite nous avons pour les clusters 4 et 5 respectivement le Cambodge et l’Indonésie .
#'Qui en termes de disponibilité en protéines d’origine animale sont assez proches et sont tout deux mals équilibrés. 
#’ avec respectivement 28% et 32% de disponibilité de protéines d’origine animale.
#’ Là où le Cambodge à tendance à s’ouvrir de manière considérable au niveau de ses importations de poulets.
#’ L’Indonésie,elle n’en importe pas car elle produit suffisamment de poulets pour répondre aux besoins de sa population.

the_df <- select(new_df, "diff_pop", "ratio_prot", "Disponibilité alimentaire (kcal/habitant)", "Disponibilité de protéines en quantité (g/habitant)", "diff_import", "PIB", "Poulets")

colnames(the_df) <-c("diff_pop", "ratio_prot", "Disp_kcal", "Disp_prot", "diff_import", "PIB", "Poulets")

res.pca <- PCA(the_df, graph = FALSE)


# Valeurs propres
eig.val <- get_eigenvalue(res.pca)
eig.val

#'Éboulies des valeurs propres:
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 60))


var <- get_pca_var(res.pca)

#Coordonnées des variables
head(var$coord, 7)

#'Cercle de corrélation :
fviz_pca_var(res.pca, col.var = "red")

#'Qualité de représentation des variables :
corrplot(var$cos2, is.corr=FALSE)


#'Projection des individus:
fviz_pca_ind (res.pca, col.ind = groupes.cah,
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE # Évite le chevauchement de texte
)


#Merge des groupes avec le df 
the_df$zone <- row.names(the_df)

groupes$zone <- row.names(groupes) 

the_df <- left_join(the_df, groupes,by="zone")

#'Groupes 
groupe_3 <- subset(the_df, sort.groupes.cah. == 3)
groupe_1 <- subset(the_df, sort.groupes.cah. == 1)
groupe_2 <- subset(the_df, sort.groupes.cah. == 2)  

#'Test d'adéquation à une loi normale :

# Calcul de la moyenne pour la variable Disponibilité alimentaire (kcal/habitant) dans le dataframe générale
xbar <- mean(the_df$`Disp_kcal`)
round(xbar,digits=2)

# Calcul de l'ecart type pour la variable Disponibilité alimentaire (kcal/habitant) dans le dataframe générale
sprime <- sd(the_df$`Disp_kcal`)
round(sprime,digits=2)

# Calcul de la variance pour la variables Disponibilité alimentaire (kcal/habitant) dans le dataframe générale
sprime2 <- var(the_df$`Disp_kcal`)
round(sprime2,digits=2)

hist(the_df$`Disp_kcal`, prob=TRUE,main="Histogramme")
curve(dnorm(x,mean=xbar,sd=sprime),col="red",lwd=2,add=TRUE,yaxt="n")

#Test de shapiro-wilk

tw <- shapiro.test(the_df$`Disp_kcal`)

#La p-value du test de shapiro-wilk est de 0.39 ce qui est supérieur à 0.05 on peut donc considerer que cette variable suit une loi normale

######## Test de comparaison de deux populations (dans le cas gaussien) : ########

#'Test égalité des variances

var_test <-var.test(groupe_1$`Disp_kcal`,groupe_2$`Disp_kcal`)
#' p-value de 0.71 qui est supérieur au 5% du niveau de test. il y'a donc égalité des variances

#'Test égalité des moyennes

mean_test <- t.test(groupe_1$`Disp_kcal`,groupe_2$`Disp_kcal)`,var.equal=TRUE)
# p-value de 2.18e-61  ce qui est inférieur à 0.05 on rejète donc l'égalité des moyennes


#'Sélection plus fines des pays :
#'
#'Je réalise une seconde ACP uniquement sur le cluster numéro 2

row.names(groupe_2) <- groupe_2$zone
groupe_2 <- select(groupe_2, "diff_pop", "ratio_prot", "Disp_kcal", "Disp_prot", "diff_import", "PIB", "Poulets")

# Supression des pays non favorables à une implentation
row.names.remove <- c("Arabie saoudite")
groupe_2 <- groupe_2[!(row.names(groupe_3) %in% row.names.remove), ]


#Centrage réduction des données
group_2.cr <- scale(groupe_2, center=T, scale=T)

#matrice des distances entre individus
d.bilan_alim <- dist(group_2.cr)

#Cah - critère de Ward
#method = « ward.D2 » correspond au vrai critère de Ward
#utilisant le carré de la distance
cah.ward.2 <- hclust(d.bilan_alim,method="ward.D2")

# Affichage du dendogramme
plot(cah.ward.2, labels = FALSE, main = "Dendrogramme")

#dendrogramme avec matérialisation des groupes
rect.hclust(cah.ward.2,k=6)

#Découpage en 6 groupes
groupes.cah.2 <- cutree(cah.ward.2,k=6)

#Liste des groupes
groupes.2 <- data.frame(sort(groupes.cah.2))
print(groupes.2)

print(sapply(groupe_2,stat.comp,y=groupes.cah.2))

######## ACP ######
res.pca.2 <- PCA(groupe_2, graph = FALSE)

var.2 <- get_pca_var(res.pca.2)

# Coordonnées des variables
head(var.2$coord, 7)

#' Cercle de corrélation
fviz_pca_var(res.pca.2, col.var = "red")

#' Qualité de représentation:
corrplot(var.2$cos2, is.corr=FALSE)

#'Projection des individus sur le premier plan factoriel :
fviz_pca_ind (res.pca.2, col.ind = groupes.cah.2,
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE # Évite le chevauchement de texte
)
