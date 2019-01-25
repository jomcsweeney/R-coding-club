setwd("~/Health_R_coding_club/week2")

install.packages('spotifyr')
install.packages('kableExtra')
library(spotifyr)
library(tidyverse)
library(kableExtra)

#load in the data
top50 <- read_csv("top50playlists.csv")

#look at the data
knitr::kable(top50) %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), font_size = 12) %>%
  scroll_box(width = "720px", height = "500px")

#some quick cleaning to remove variables we dont need.
top50$playlist_name <- as.factor(top50$playlist_name)
top50$artist_name <- as.factor(top50$artist_name)
top50$track_name <- as.factor(top50$track_name)
top50 <- top50[,-c(2,7)]

#Now the playlist_name variable is a factor we can easily see the different playlists in the dataset using levels
levels(top50$playlist_name)

pop_tracks <- top50 %>% dplyr::count(track_name, sort=TRUE)
pop_tracks$artist_name <- top50$artist_name[match(pop_tracks$track_name, top50$track_name)]
pop_tracks

top50 %>% dplyr::count(artist_name, sort=TRUE)

#before we start the clustering lets clean the playlist_name column so we have a nicer shorter name for identifying a country
top50$country <- gsub("\\-.*","", top50$playlist_name)
levels(as.factor(top50$country))
top50$country <- gsub("TOP HITS ","",top50$country)
levels(as.factor(top50$country))
top50$country <- gsub(" ","",top50$country)
levels(as.factor(top50$country))
top50$country <- as.factor(top50$country)

#To keep things tidy lets also make the global and paraguay names match and drop the extra Japan playlist
top50$country <- gsub("GLOBALTOP50","GLOBAL",top50$country)
top50$country <- gsub("Top50SongsinParaguay","PARAGUAY",top50$country)
top50 <- subset(top50, country!="TOPSONGSINJAPAN2019")
top50$country <- as.factor(top50$country)


#Lets firstly compare the country based on their averages for the different features.
top50_agg <- aggregate(top50[, c(6:17)], list(top50$country), mean)
colnames(top50_agg)[1] <- "country"

#look at the data
knitr::kable(top50_agg) %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), font_size = 12) %>%
  scroll_box(width = "720px", height = "500px")

#Another good way to spot this is by using the summary function we looked at last week.
summary(top50_agg)

#We can see that key and mode are all NA's, lets use this function on the raw data to find out why.
summary(top50)
#We can see that key and mode are character variables so the numeric operation mean doesn't work. 
#*This is why its always worth looking at your data before doing a transformation*

#We could transform these to be numberic but for now we drop them.
top50$key <- as.factor(top50$key)
top50 %>% dplyr::count(key, sort=TRUE)
top50$mode <- as.factor(top50$mode)
top50 %>% dplyr::count(mode, sort=TRUE)

top50_agg <- aggregate(top50[, c(6,7,9,11:17)], list(top50$country), mean)
colnames(top50_agg)[1] <- "country"
str(top50_agg)
#A quick check shows all the remaining variables are numeric and have values.

#look at the data
knitr::kable(top50_agg) %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), font_size = 12) %>%
  scroll_box(width = "720px", height = "500px")

#Now like last week lets normalise the data
top50_agg_norm <- cbind(top50_agg[1],apply(top50_agg[-1],2,
                                           function(x){(x-min(x))/diff(range(x))}))

#Again lets plot a radar chart to have a quick look at the data (this is just small tweaks to last weeks code)
install.packages("radarchart")
library(radarchart)
top50_agg_10 <- top50_agg[top50_agg$country %in% c("GLOBAL", "ARGENTINA", "BRAZIL", "CANADA", "FRANCE", "GERMANY", "HONGKONG", "SPAIN", "TAIWAN", "UNITEDKINGDOM", "UNITEDSTATES"),]

top50_agg_norm_10 <- cbind(top50_agg_10[1], 
                           apply(top50_agg_10[-1],2,
                                 function(x){(x-min(x)) / diff(range(x))})) 

radarDF <- gather(top50_agg_norm_10, key=Attribute, value=Score, -country) %>%
  spread(key=country, value=Score)

chartJSRadar(scores = radarDF,
             scaleStartValue = -1, 
             maxScale =1, 
             showToolTipLabel = TRUE)

#Now on to the clustering!

#Before we get started we first want to rescale the numeric variables
scaledFeatures <- scale(top50_agg[-1])
rownames(scaledFeatures) <- top50_agg$country


knitr::kable(scaledFeatures) %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), font_size = 12) %>%
  scroll_box(width = "720px", height = "500px")


install.packages("ggfortify")
library(ggfortify)
set.seed(345)
k_means <- kmeans(scaledFeatures, 3)
kmeans_plot <- autoplot(k_means,
                        main= "K-means clustering",
                        data = scaledFeatures,
                        loadings = TRUE, loadings.colour = "#CC0000",loadings.label.colour = "#CC0000",
                        loadings.label = TRUE, loadings.label.size = 3,  loadings.label.repel=T,
                        label.size = 3, label.repel = T) + scale_fill_manual(values = c("#000066","#9999CC", "#66CC99"))+ scale_color_manual(values = c("#000066", "#9999CC", "#66CC99")) + theme(plot.title=element_text(size=18, face="bold"))


kmeans_plot
#This looks a bit messy, as its so topical lets restrict it to EU countries.
scaledFeaturesEU <- scaledFeatures[c("AUSTRIA", "BELGIUM", "BULGARIA", "CYPRUS", "CZECHREPUBLIC", "DENMARK", "ESTONIA", "FINLAND", "FRANCE", "GERMANY", "GREECE", "HUNGARY", "IRELAND", "ITALY", "LATVIA", "LITHUANIA", "LUXEMBOURG", "MALTA", "NETHERLANDS", "POLAND", "PORTUGAL", "ROMANIA", "SLOVAKIA", "SPAIN", "SWEDEN", "UNITEDKINGDOM"),]

#run the clustering again
set.seed(345)
k_meansEU <- kmeans(scaledFeaturesEU, 3)
kmeans_plotEU <- autoplot(k_meansEU,
                          main= "K-means clustering",
                          data = scaledFeaturesEU,
                          loadings = TRUE, loadings.colour = "#CC0000",loadings.label.colour = "#CC0000",
                          loadings.label = TRUE, loadings.label.size = 3,  loadings.label.repel=T,
                          label.size = 3, label.repel = T) + scale_fill_manual(values = c("#000066","#9999CC", "#66CC99"))+ scale_color_manual(values = c("#000066", "#9999CC", "#66CC99")) + theme(plot.title=element_text(size=18, face="bold"))


#We can tweak this to form 4 clusters, by changing the centers argument for kmeans. 
#*note you will need to remember to add an extra colour to scale_fill_manual and scale_colour_manual to see these.*
set.seed(345)
k_meansEU4 <- kmeans(scaledFeaturesEU, 4)
kmeans_plotEU4 <- autoplot(k_meansEU4,
                           main= "K-means clustering",
                           data = scaledFeaturesEU,
                           loadings = TRUE, loadings.colour = "#CC0000",loadings.label.colour = "#CC0000",
                           loadings.label = TRUE, loadings.label.size = 3,  loadings.label.repel=T,
                           label.size = 3, label.repel = T) + scale_fill_manual(values = c("#000066","#9999CC", "#66CC99", "#e7e247"))+ scale_color_manual(values = c("#000066", "#9999CC", "#66CC99", "#e7e247")) + theme(plot.title=element_text(size=18, face="bold"))


k_means4 <- kmeans(scaledFeatures, 4)
kmeans_plot4 <- autoplot(k_means4,
                         main= "K-means clustering",
                         data = scaledFeatures,
                         loadings = TRUE, loadings.colour = "#CC0000",loadings.label.colour = "#CC0000",
                         loadings.label = TRUE, loadings.label.size = 3,  loadings.label.repel=T,
                         label.size = 3, label.repel = T) + scale_fill_manual(values = c("#000066","#9999CC", "#66CC99"))+ scale_color_manual(values = c("#000066", "#9999CC", "#66CC99")) + theme(plot.title=element_text(size=18, face="bold"))




#Lets put these clusters on a map.
op <- options(gvis.plot.tag = "chart")
install.packages("googleVis")
library(googleVis)
countries_clusters <- as_tibble(rownames_to_column(as_data_frame(k_means$cluster)))
colnames(countries_clusters) <- c("Country", "Cluster")
WorldMap=gvisGeoChart(data = countries_clusters,locationvar="Country", colorvar="Cluster",
                      options=list(projection="kavrayskiy-vii",
                                   colorAxis="{colors:['#000066', '#9999CC', '#66CC99']}",
                                   width = 400, height = 200))
plot(WorldMap)

countries_clusters <- as_tibble(rownames_to_column(as_data_frame(k_meansEU$cluster)))
colnames(countries_clusters) <- c("Country", "Cluster")
WorldMap=gvisGeoChart(data = countries_clusters,locationvar="Country", colorvar="Cluster",
                      options=list(projection="kavrayskiy-vii",
                                   colorAxis="{colors:['#000066', '#9999CC', '#66CC99']}",
                                   width = 400, height = 200))
plot(WorldMap)

countries_clusters <- as_tibble(rownames_to_column(as_data_frame(k_meansEU4$cluster)))
colnames(countries_clusters) <- c("Country", "Cluster")
WorldMap=gvisGeoChart(data = countries_clusters,locationvar="Country", colorvar="Cluster",
                      options=list(projection="kavrayskiy-vii",
                                   colorAxis="{colors:['#000066', '#9999CC', '#66CC99']}"
                      ))
plot(WorldMap)

countries_clusters <- as_tibble(rownames_to_column(as_data_frame(k_means4$cluster)))
colnames(countries_clusters) <- c("Country", "Cluster")
WorldMap=gvisGeoChart(data = countries_clusters,locationvar="Country", colorvar="Cluster",
                      options=list(projection="kavrayskiy-vii",
                                   colorAxis="{colors:['#000066', '#9999CC', '#66CC99']}"
                      ))
plot(WorldMap)

#radarchart
top50_agg_norm_83 <- cbind(top50_agg[1], 
                           apply(top50_agg[-1],2,
                                 scale)) 


# Converting cluster to vector


library(radarchart)
library(tidyr)
cluster_centers <- as.data.frame(k_means$centers)
cluster <- c("Cluster 1", "Cluster 2", "Cluster 3")
cluster_centers <- cbind(cluster, cluster_centers)

radarDF2 <- gather(cluster_centers, key=Attribute, value=Score, -cluster) %>%
  spread(key=cluster, value=Score)
#we change the colours according to clusters
colMatrix = matrix(c(c(4,24,102), c(135,133,193), c(87,196,135)), nrow = 3)
#chart
chartJSRadar(scores = radarDF2,
             scaleStartValue = -4, 
             maxScale =1.5, 
             showToolTipLabel = TRUE,
             colMatrix = colMatrix)

#We can further explore individual features, lets look at danceability
library(stringr)
clusters <- as.vector(k_means$cluster)
clusters <- str_replace_all(clusters, "1", "Cluster 1")
clusters <- str_replace_all(clusters, "2", "Cluster 2")
clusters <- str_replace_all(clusters, "3", "Cluster 3")
clusters[28] <- "Global"

# Showing only Danceability
top50_agg_norm_83 <- cbind(top50_agg_norm_83, cluster = clusters)
danceability_subset <- top50_agg_norm_83[,c("country","danceability", "cluster")]

library(ggplot2)
# Showing only Danceability
danceability_subset <- danceability_subset[order(danceability_subset$danceability, decreasing = TRUE), ]

danceability_plot <- ggplot(danceability_subset, aes(x = reorder(country, danceability), y = danceability, label=danceability)) + xlab("Country") + ylab("Danceability") + geom_bar(stat='identity', aes(fill=cluster), width = .5) + scale_fill_manual(name="Cluster",
                                                                                                                                                                                                                                                        labels = c("Cluster 1", "Cluster 2", "Cluster 3", "Global"), 
                                                                                                                                                                                                                                                        values = c("Cluster 1"="#000066", "Cluster 2"="#9999CC", "Cluster 3" = "#66CC99", "Global" = "indianred2")) + labs(title="Danceability feature", subtitle= "Diverging Bars") + coord_flip()
danceability_plot

#Lets see if any of the other features look like they could be more interesting
summary(top50_agg_norm_83)

# Showing only Loudness
loudness_subset <- top50_agg_norm_83[,c("country","loudness", "cluster")]

# Showing only Loudness
loudness_subset <- loudness_subset[order(loudness_subset$loudness, decreasing = TRUE), ]

loudness_plot <- ggplot(loudness_subset, aes(x = reorder(country, loudness), 
                                             y = loudness, label=loudness)) + 
  xlab("Country") + ylab("Loudness") + geom_bar(stat='identity', aes(fill=cluster), width = .5) + scale_fill_manual(name="Cluster", 
                                                                                                                    labels = c("Cluster 1", "Cluster 2", "Cluster 3", "Global"), 
                                                                                                                    values = c("Cluster 1"="#000066", "Cluster 2"="#9999CC", "Cluster 3" = "#66CC99", "Global" = "indianred2")) 
+ labs(title="Loudness feature", subtitle= "Diverging Bars") + coord_flip()

loudness_plot


