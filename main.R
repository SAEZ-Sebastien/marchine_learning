#----------------------------------------------#
# DOWNLOADING AND INSTALLING REQUIRED PACKAGES #
#----------------------------------------------#

install.packages("rpart")
install.packages("C50")
install.packages("tree")

# Activate packages
library(rpart)
library(C50)
library(tree)

setwd('..')

#---------------------------------------------------#
# LOADING DATA                                      #
#---------------------------------------------------#

donnees <- read.csv("./datas/Data Projet.csv", header = T, sep = ",", dec = "..")

str(donnees)

donnees_EA <- donnees[1:800,]

donnes_ET <- donnees[801:1200,]

# Supression de "customer" car c'est id et n'a donc pas d'influence
donnees_EA <- subset(donnees_EA, select = -customer)

# Creation des graphes
tree1 <- rpart(default ~ ., donnees_EA)
tree2 <- C5.0(default ~ ., donnees_EA)
tree3 <- tree(default ~ ., donnees_EA)

# Prediction des 3 algos
test_tree1 <- predict(tree1, donnes_ET, type="class")
test_tree2 <- predict(tree2, donnes_ET, type="class")
test_tree3 <- predict(tree3, donnes_ET, type="class")

donnes_ET$Tree1 <- test_tree1
donnes_ET$Tree2 <- test_tree2
donnes_ET$Tree3 <- test_tree3

#---------------------------------------------------#
# CALCUL TAUX SUCCES                                #
#---------------------------------------------------#

# Comparaison des taux de succes des 3 algos
taux_succes_rpart <- length(donnes_ET[donnes_ET$default==donnes_ET$Tree1,"customer"]) / nrow(donnes_ET)
taux_succes_c5 <- length(donnes_ET[donnes_ET$default==donnes_ET$Tree2,"customer"]) / nrow(donnes_ET)
taux_succes_tree <- length(donnes_ET[donnes_ET$default==donnes_ET$Tree3,"customer"]) / nrow(donnes_ET)

print(taux_succes_rpart) # Meilleur taux succes
print(taux_succes_c5) # 3e meilleur taux succes
print(taux_succes_tree) # 2e meilleur taux succes

#----------------------------------#
# CALCUL DES MATRICES DE CONFUSION #
#----------------------------------#

# Matrice de confusion pour 'tree1'
mc_tree1 <- table(donnes_ET$default, test_tree1)
print(mc_tree1)
# Rappel
mc_tree1[1,1]/(mc_tree1[1,1]+mc_tree1[1,2])
# Spécificité
mc_tree1[2,2]/(mc_tree1[2,1]+mc_tree1[2,2])
# Précision
mc_tree1[1,1]/(mc_tree1[1,1]+mc_tree1[2,1])
# Taux de Vrais Négatifs
mc_tree1[2,2]/(mc_tree1[1,1]+mc_tree1[1,2])

# Matrice de confusion pour 'tree2'
mc_tree2 <- table(donnes_ET$default, test_tree2)
print(mc_tree2)
# Rappel
mc_tree2[1,1]/(mc_tree2[1,1]+mc_tree2[1,2])
# Spécificité
mc_tree2[2,2]/(mc_tree2[2,1]+mc_tree2[2,2])
# Précision
mc_tree2[1,1]/(mc_tree2[1,1]+mc_tree2[2,1])
# Taux de Vrais Négatifs
mc_tree2[2,2]/(mc_tree2[1,1]+mc_tree2[1,2])

# Matrice de confusion pour 'tree3'
mc_tree3 <- table(donnes_ET$default, test_tree3)
print(mc_tree3)
# Rappel
mc_tree3[1,1]/(mc_tree3[1,1]+mc_tree3[1,2])
# Spécificité
mc_tree3[2,2]/(mc_tree3[2,1]+mc_tree3[2,2])
# Précision
mc_tree3[1,1]/(mc_tree3[1,1]+mc_tree3[2,1])
# Taux de Vrais Négatifs
mc_tree3[2,2]/(mc_tree3[1,1]+mc_tree3[1,2])
