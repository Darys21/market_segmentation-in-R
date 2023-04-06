
  

# Charger le fichier CSV
store_data <- read.csv("C:/Users/2SH/Documents/analyse de donnée/projet_analyse/store.csv", header=TRUE)

# Afficher les premières lignes pour vérifier que le fichier est bien chargé
head(store_data)

# Vérifier s'il y a des valeurs manquantes
sum(is.na(store_data))

# Supprimer les lignes avec des valeurs manquantes
store_data <- na.omit(store_data)

# Vérifier s'il y a des doublons
sum(duplicated(store_data))






# Installer la librairie "dplyr" pour manipuler les données
install.packages("dplyr")
library(dplyr)



# Vérifier s'il y a des doublons
sum(duplicated(store_data))

# Supprimer les doublons
store_data <- distinct(store_data)




# Installer la librairie "cluster" pour effectuer une segmentation de marché
install.packages("cluster")
library(cluster)

# Sélectionner les variables à utiliser pour la segmentation
segmentation_data <- store_data[, c("qty", "revenue")]

# Standardiser les données pour les rendre comparables
segmentation_data <- scale(segmentation_data)

# Effectuer une analyse de clustering k-means pour créer 4 segments de marché
segmentation_results <- kmeans(segmentation_data, centers=4)

# Ajouter les résultats de la segmentation au tableau de données
store_data$segment <- as.factor(segmentation_results$cluster)

# Afficher le nombre d'observations dans chaque segment
table(store_data$segment)




# Analyse univariée des variables numériques
summary(store_data[, c("qty", "revenue")])

# Analyse univariée des variables qualitatives
summary(store_data[, c("region", "segment")])


# Analyse bivariée entre les variables qualitatives et numériques
table(store_data$region, store_data$segment)
table(store_data$qty,store_data$revenue)




#calculer le tableau de revenus totaux pour chaque segment 
revenue_by_segment <- store_data %>%
  group_by(segment) %>%
  summarise(total_revenue = sum(revenue))

# Afficher le tableau de revenus par segment
revenue_by_segment





# Calculer les revenus totaux pour chaque région
revenue_by_region <- store_data %>%
  group_by(region) %>%
  summarise(total_revenue = sum(revenue))

# Afficher le tableau de revenus par région
revenue_by_region




# Comparaison des résultats des segments avec les régions
sales_summary %>%
  group_by(region) %>%
  mutate(segment_share = total_revenue / sum(total_revenue)) %>%
  arrange(region, segment) %>%
  select(region, segment, segment_share, total_revenue, total_qty)




# Installer la librairie "ggplot2" pour visualiser les données
install.packages("ggplot2")
library(ggplot2)

# Créer un graphique en barres pour les revenus par région
ggplot(revenue_by_region, aes(x=region, y=total_revenue)) +
  geom_bar(stat="identity", fill="blue") +
  labs(title="Revenus par région", x="Région", y="Revenue")



# Installer la librairie "factoextra" pour visualiser les résultats de la segmentation
install.packages("factoextra")
library(factoextra)

# Créer un graphique en nuage de points pour les quantités vendues et les revenus, en coloriant par segment
fviz_cluster(segmentation_results, data=segmentation_data, stand=FALSE,
             geom="point", palette="jco", main="Segmentation de marché")




# Visualiser les résultats
ggplot(store_revenue, aes(x = region, y = revenue, color = as.factor(kmeans_model$cluster))) + 
  geom_point() + 
  labs(title = "Segmentation de marché en fonction des revenus générés dans différentes régions", 
       x = "Région", y = "Revenue") +
  theme_bw()




#la structure des revenus
str(store_revenue)
#nouveau data frame   qui ne contient que les colonnes numériques des revenus
store_revenue_numeric <- store_revenue[, sapply(store_revenue, is.numeric)]
#standarisation 
store_revenue_std <- scale(store_revenue_numeric)

#Statistiques descriptives pour chaque variable
summary(store_data)



#Statistiques descriptives pour chaque variable, par segment de marché
store_data %>%
  group_by(segment) %>%
  summarise_at(vars(qty, revenue), list(mean=mean, sd=sd, min=min, max=max))



#Visualisation des ventes par produit, par région
ggplot(store_data, aes(x=product, y=qty, fill=region)) +
  geom_boxplot() +
  labs(title="Ventes par produit et par région", x="Produits", y="Quantité vendue")

#Visualisation des revenus par région et par segment de marché
ggplot(store_data, aes(x=region, y=revenue, fill=segment)) +
  geom_bar(stat="identity", position="dodge") +
  labs(title="Revenus par région et par segment de marché", x="Région", y="Revenus")
#Ces analyses  permettent de mieux comprendre les comportements d'achat des clients dans chaque région et dans chaque segment de marché




#segments obtenus à partir de la segmentation de marché
table(store_data$segment)

#les différents numéros de cluster présents dans la colonne "segment"
unique(store_data$segment)




# Grouper les données par région et segment
grouped_data <- store_data %>%
  group_by(region, segment)

# Afficher les résultats
print(summary_data)

