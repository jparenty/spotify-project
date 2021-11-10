songs <- read.csv("./R/R dataset/songs.csv")

liked_songs <- read.csv("./R/R dataset/LikedSongs.csv")
disliked_songs <- read.csv("./R/R dataset/DisLikedSongs.csv")


library(ggplot2)
library(plyr)
library(grid)
install.packages("gridBase")
library(gridBase)
library(ggfortify)

summary(songs)

#count the numner of different values for various variable
type = count(songs, "type")
#one observation, the same for all rows, droping this column
time_sign = count(songs, "time_signature")
#what is time signature exactly? we don't expect this to be significant pred, we'll see when testing models



#####TOP ARTISTS DISTRIBUTION#####
par(mfrow=c(2,1))
artists_liked = count(liked_songs, "artist")
#top 20 liked artists
artists_liked = artists_liked[order(artists_liked$freq, decreasing = TRUE),][1:20,]
xarg = artists_liked$artist

top_artists <- barplot(artists_liked$freq, names.arg = xarg, las=2, col = "skyblue", border = F, main = "Top 20 artists")
legend("topright", c("Liked Songs", "Disliked Songs"), fill=c("skyblue", "red"))


artists_disliked = count(disliked_songs, "artist")
#top20 disliked artists
artists_disliked = artists_disliked[order(artists_disliked$freq, decreasing = TRUE),][1:20,]
xarg_dis = artists_disliked$artist
top_dis_artists <- barplot(artists_disliked$freq, las=2, col = "red", border = F, cex.names=.8)
vps1 <- baseViewports()
pushViewport(vps1$inner, vps1$figure, vps1$plot)
grid.text(xarg_dis,
          x = unit(top_dis_artists, "native"), y=unit(-1, "lines"),
          just="right", rot=50)

genres_liked = count(liked_songs, "time_signature")


###### TOP GENRES DISTRIBUTION #######
#genres in liked songs
genres_liked = count(liked_songs, "genres")
genres_liked = genres_liked[2:196,]
genres_liked = genres_liked[order(genres_liked$freq, decreasing = TRUE),][1:20,]
sum(genres_liked[order(genres_liked$freq, decreasing = TRUE),][1:20,]$freq)
#top 20 genres group 563 of the 1110 liked tracks (50%), the rest is scatter in 175 other genres
barplot(genres_liked$freq, names.arg = genres_liked$genres, las=2, col = "skyblue", border = F, main = "Top genres")
legend("topright", c("Liked Songs", "Disliked Songs"), fill=c("skyblue", "red"))

#genres in disliked songs
genres_disliked = count(disliked_songs, "genres")
genres_disliked = genres_disliked[2:152,]
genres_disliked = genres_disliked[order(genres_disliked$freq, decreasing = TRUE),][1:20,]
genres_dis <- barplot(genres_disliked$freq, las=2, col = "red", border = F)
vps1 <- baseViewports()
pushViewport(vps1$inner, vps1$figure, vps1$plot)
grid.text(genres_disliked$genres,
          x = unit(genres_dis, "native"), y=unit(-1, "lines"),
          just="right", rot=50)

#we see some common genres in top dis and top like, but with very different proportions, this mean that in the same genre there's
#some content I like and don't like, expected


#what is adult_standards ? my 13th most popular genre, lets find out:
adult = liked_songs[which(liked_songs$genres == "adult standards"),]

loud_track = liked_songs[which(liked_songs$loudness < -15),]
#adult genres: Nina Simone, Micheal Franks, George Benson mainly --> probably spotify classifying old music
#Adult standards (also sometimes known as the nostalgia format) is aimed at "mature" adults, meaning mainly those persons over 50 years of age, but it is mostly targeted for senior citizens.
#maybe spotify is trying to tell me that I should be 40 years older regarding my taste, but at least, I would have great music taste.
#more seriously, most of these songs shoud be classified as Soul/Blues.
#an adult standard playlist: https://open.spotify.com/playlist/24g2jG93hDW5Fsn8JInwRR



##### QUANTITATIVE VARIABLES DISTRIBUTION ######

par(mfrow=c(2,2))
par(mfrow=c(1,1))
op <- par(mar = c(4,4,4,4) + 0.1)

#plotting track's dancebility
#shifted
dance <- hist(liked_songs$danceability,xlim=c(0,1),col='skyblue',border=F, xlab="Danceability", main="Distribution of track's danceability")
dance_dis <- hist(disliked_songs$danceability,add=T,col=scales::alpha('red',.5),border=F)
legend("topleft", c("Liked Songs", "Disliked Songs"), fill=c("skyblue", "red"))

#flag
energy <- hist(liked_songs$energy,col='skyblue',border=F, xlab="Energy", main="Distribution of track's energy")
energy_dis <- hist(disliked_songs$energy,add=T,col=scales::alpha('red',.5),border=F)
legend("topleft", c("Liked Songs", "Disliked Songs"), fill=c("skyblue", "red"))

#flag
loudness <- hist(liked_songs$loudness,col='skyblue',border=F, xlab="Loudness", main="Distribution of track's loudness")
loudness_dis <- hist(disliked_songs$loudness,add=T,col=scales::alpha('red',.5),border=F)
legend("topleft", c("Liked Songs", "Disliked Songs"), fill=c("skyblue", "red"))

loudness_test <- hist(test_songs$loudness,col='grey',border=F, xlab="Valence", main="Distribution of track's Valence")

#DROP
speechiness <- hist(liked_songs$speechiness,col='skyblue',border=F, xlab="Speechiness", main="Distribution of track's speachiness")
speechiness_dis <- hist(disliked_songs$speechiness,add=T,col=scales::alpha('red',.5),border=F)
legend("topright", c("Liked Songs", "Disliked Songs"), fill=c("skyblue", "red"))
#all observations are music and none are just speech
#we can drop this variable as it's just the same for both dataset and doesn't provide much more information than 'the observation is a song'

#flag
valence <- hist(liked_songs$valence,col='skyblue',border=F, xlab="Valence", main="Distribution of track's Valence")
valence_dist <- hist(disliked_songs$valence,add=T,col=scales::alpha('red',.5),border=F)
legend("topleft", c("Liked Songs", "Disliked Songs"), fill=c("skyblue", "red"))
#liked songs are very present in high valence but also in low valence --> distribution with a high deviation from its mean compare to the disliked music wich has a lower deviation from its mean

valence_test <- hist(test_songs$valence,col='grey',border=F, xlab="Valence", main="Distribution of track's Valence")

#pretty similar
acoustic <- hist(liked_songs$acousticness,col='skyblue',border=F, xlab="acousticness", main="Distribution of track's acousticness")
acoustic_dis <- hist(disliked_songs$acousticness,add=T,col=scales::alpha('red',.5),border=F)
legend("topright", c("Liked Songs", "Disliked Songs"), fill=c("skyblue", "red"))
#we observe a slight increase in frequences as the acousticness increase compare to disliked songs which has a more constant decreasing trend


#istrumentalness -> probability that the track contains no vocal
instrumentalness <- hist(liked_songs$instrumentalness,col='skyblue',border=F, xlab="Instrumentalness", main="Distribution of track's instrumentalness") 
instrumentalness_dis <- hist(disliked_songs$instrumentalness,add=T,col=scales::alpha('red',.5),border=F)
legend("topright", c("Liked Songs", "Disliked Songs"), fill=c("skyblue", "red"))
#among songs I enjoy, instrumental tracks have a bigger portions compare to songs I dislike and test songs dataset, which doesn't have any instrumental
#track with a confidence above 0.8
trackt_instrumental = liked_songs[which(liked_songs$instrumentalness > 0.8),]
#what make a track more instrumental: let look at their genres
genres_liked_instrumental = count(trackt_instrumental, "genres")
#I compare this to the instrumentalness of test_song to get an idea of the average distribution of this variable
#among the tracks with high instrumentalness, we have classical music, electro and soundtrack

#similar
tempo <- hist(liked_songs$tempo,col='skyblue',border=F, xlab="tempo", main="Distribution of track's tempo")
tempo_dis <- hist(disliked_songs$tempo,add=T,col=scales::alpha('red',.5),border=F)
#same distribution, we can expect tempo not to play a big role in the model
tempo_test <- hist(test_songs$tempo,col='skyblue',border=F, xlab="tempo", main="Distribution of track's tempo")

time_sign <- hist(liked_songs$time_signature,col='skyblue',border=F, xlab="time signature", main="Distribution of track's time signature")
time_sign_dis <- hist(disliked_songs$time_signature,add=T,col=scales::alpha('red',.5),border=F)
#pretty similar among liked and disliked songs, we can probably drop this variable from our dataset
#majority of 4, unlikely that other values have impact
#-> probably drop this variable

par(mfrow=c(2,3))
mode_liked = count(liked_songs, "mode")
mode_disliked = count(disliked_songs, "mode")
mode <- barplot(mode_liked$freq, col = "skyblue", names.arg = mode_liked$mode, border = F, space = 0.4, main = "Distribution of liked track's mode")
mode_dis <- barplot(mode_disliked$freq, col = "red", names.arg = mode_disliked$mode, border = F, space = 0.4, main = "Distribution of disliked track's mode")
#pretty similar distribution, better equilibrium for liked songs, major tracks have a higher weight for disliked tracks compare to liked tracks

explicit_liked = count(liked_songs, "explicit")
explicit_disliked = count(disliked_songs, "explicit")
op <- par(mar = c(2,2,8,2) + 0.1)
explicit <-  barplot(explicit_liked$freq, col = "skyblue", names.arg = explicit_liked$explicit, ylim = c(0,800),  border = F, space = 0.4, main = "Distribution of liked track's explicity")
explicit_dis <-  barplot(explicit_disliked$freq, col = "red",  names.arg = explicit_disliked$explicit, ylim = c(0,800), border = F, space = 0.4, main = "Distribution of disliked track's explicity")
#the proportion of explicit content in my disliked songs is higher than in my liked songs, my mother would probably be very proud.
#I tend prefer songs that are have less explicit content on average
#we'll see how true this is when building our model

#popularity distribution
popularity <- hist(liked_songs$popularity,col='skyblue',border=F, xlab="Popularity", main="Distribution of track's popularity") 
popularity_dis <- hist(disliked_songs$popularity,add=T,col=scales::alpha('red',.5),border=F)
#this is an intersting finding and it shows the limit of my disliked songs dataset, I failed to include in there songs from different
#periods of time (as its evident that I dislike songs released before 2020), it is certain that year variable will be affected and biased in predicive model and souldn't be included

#year distribution
year <- hist(liked_songs$year,col='skyblue',border=F, xlab="Year", main="Distribution of track's year") 
year_dis <- hist(disliked_songs$year,add=T,col=scales::alpha('red',.5),border=F)
#this is an intersting finding and it shows the limit of my disliked songs dataset, I failed to include in there songs from different
#periods of time (as its evident that I dislike songs released before 2020),
#it is highly probable that year variable will be affected and biased in for predicion and souldn't be included in our model
#however, it shows that my music taste are dominated by music released in the last years
recent_liked_songs = liked_songs[which(liked_songs$year > 2009),]
old_liked_songs = liked_songs[which(liked_songs$year < 2010),]
#we have 535 songs from 2010 and after
#we have 575 songs from 2009 and earlier

#duration distribution
duration <- hist(liked_songs$duration_ms ,col='skyblue',border=F, xlab="Duration", main="Distribution of track's duration") 
duration_dis <- hist(disliked_songs$duration_ms,add=T,col=scales::alpha('red',.5),border=F)
#liked_songs duration is shifted toward longer songs compare to disliked_songs duration
#I like longer songs better, I totally relate with this information as I'm conviced that, at least for my taste, longer songs 
#have much more potential to be better as they simply allow for more content to listen to (longer intro, more transitions and parts in the song)


####### lets take the mean for: danceability, energy, loudness, valence, acoustic, instrumentalness, tempo

#liked songs mean:

like_dance_avg = summary(liked_songs$danceability)[["Mean"]]
like_energy_avg = summary(liked_songs$energy)[["Mean"]]
like_valence_avg = summary(liked_songs$valence)[["Mean"]]
like_acoustic_avg = summary(liked_songs$acoustic)[["Mean"]]
like_instrumentalness_avg = summary(liked_songs$instrumentalness)[["Mean"]]

avg_liked_comp = c(like_dance_avg, like_energy_avg, like_valence_avg, like_acoustic_avg, like_instrumentalness_avg)
#xarg_liked_comp = c("Danceability", "Energy", "Valence", "Acoustic", "Instrumentalness")

par(mfrow=c(2,1))
op <- par(mar = c(2,4,2,2) + 0.1)
barplot(avg_liked_comp, las=2, col = "skyblue", border = F, main = "Track important features average", space=0.4)
legend("topright", c("Liked Songs", "Disliked Songs"), fill=c("skyblue", "red"))
par(op)

#disliked songs mean:

disliked_dance_avg = summary(disliked_songs$danceability)[["Mean"]]
disliked_energy_avg = summary(disliked_songs$energy)[["Mean"]]
#like_loudness_avg = summary(liked_songs$loudness)[["Mean"]]
disliked_valence_avg = summary(disliked_songs$valence)[["Mean"]]
disliked_acoustic_avg = summary(disliked_songs$acoustic)[["Mean"]]
disliked_instrumentalness_avg = summary(disliked_songs$instrumentalness)[["Mean"]]
#like_tempo_avg = summary(liked_songs$tempo)[["Mean"]]

avg_disliked_comp = c(disliked_dance_avg, disliked_energy_avg, disliked_valence_avg, disliked_acoustic_avg, disliked_instrumentalness_avg)
xarg_disliked_comp = c("Danceability", "Energy", "Valence", "Acoustic", "Instrumentalness")

op <- par(mar = c(3,4,0,2) + 0.1)
dis_feat_avg <- barplot(avg_disliked_comp, names.arg = xarg_disliked_comp, col = "red", border = F, space=0.4)
vps1 <- baseViewports()
pushViewport(vps1$inner, vps1$figure, vps1$plot)
grid.text(xarg_disliked_comp,
          x = unit(dis_feat_avg, "native"), y=unit(-1, "lines"),
          just="right", rot=50)

par(op)

avg_diff = avg_liked_comp - avg_disliked_comp

par(op)

avg_result <- barplot(avg_diff, col = ifelse(avg_diff < 0, "red", "skyblue"), names.arg = xarg_disliked_comp, border = F, space = 0.4, main = "Track important features average difference")
legend("topleft", c("Liked Songs", "Disliked Songs"), fill=c("skyblue", "red"))

#individual mean variable comparaison for: loudness, temp, popularity (they have different scale compare to others variables), popularity

par(mfrow=c(1,3))

#tempo:

like_tempo_avg = summary(liked_songs$tempo)[["Mean"]]
disliked_tempo_avg = summary(disliked_songs$tempo)[["Mean"]]
tempo_avg_diff = like_tempo_avg - dislied_tempo_avg

barplot(tempo_avg_diff, col = ifelse(tempo_avg_diff < 0, "red", "skyblue"), ylim = c(-5,5), names.arg = "Tempo", border = F, space = 0.4, main = "Track tempo average difference")
legend("topleft", c("Liked Songs", "Disliked Songs"), fill=c("skyblue", "red"))

#loudness:

like_loudness_avg = abs(summary(liked_songs$loudness)[["Mean"]])
disliked_loudness_avg = abs(summary(disliked_songs$loudness)[["Mean"]])
loudness_avg_diff = like_loudness_avg - disliked_loudness_avg

barplot(loudness_avg_diff, col = ifelse(loudness_avg_diff < 0, "red", "skyblue"), ylim = c(-5,5), names.arg = "Loudness", border = F, space = 0.4, main = "Track loudness average difference")

#popularity:

like_popularity_avg = summary(liked_songs$popularity)[["Mean"]]
dislike_popularity_avg = summary(disliked_songs$popularity)[["Mean"]]
popularity_avg_diff = like_popularity_avg - dislike_popularity_avg

barplot(popularity_avg_diff, col = ifelse(popularity_avg_diff < 0, "red", "skyblue"), names.arg = "Popularity",ylim=c(-10,10), border = F, space = 0.4, main = "Track popularity average difference")



###### MODEL CONSTRUCTION #####

attach(songs)
library(randomForest)
library(tree)
library(rpart)
library(tidyverse)
library(caret) 

excl_columns= names(songs) %in% c("type", "id", "uri", "track_href", "analysis_url")
excl_columns_liked= names(liked_songs) %in% c("type", "id", "uri", "track_href", "analysis_url")
excl_columns_disliked= names(disliked_songs) %in% c("type", "id", "uri", "track_href", "analysis_url")

songs = songs[!excl_columns]
liked_songs = liked_songs[!excl_columns_liked]
disliked_songs = disliked_songs[!excl_columns_disliked]

songs$key = as.factor(songs$key)
songs$mode = as.factor(songs$mode)
songs$explicit = as.factor(songs$explicit)
songs$time_signature = as.factor(songs$time_signature)
songs$artist = as.factor(songs$artist)


liked_songs$key = as.factor(liked_songs$key)
liked_songs$mode = as.factor(liked_songs$mode)
liked_songs$explicit = as.factor(liked_songs$explicit)
liked_songs$time_signature = as.factor(liked_songs$time_signature)
liked_songs$artist = as.factor(liked_songs$artist)
#liked_songs$outcome = as.factor(liked_songs$outcome)

disliked_songs$key = as.factor(disliked_songs$key)
disliked_songs$mode = as.factor(disliked_songs$mode)
disliked_songs$explicit = as.factor(disliked_songs$explicit)
disliked_songs$time_signature = as.factor(disliked_songs$time_signature)
disliked_songs$artist = as.factor(disliked_songs$artist)
disliked_songs$outcome = as.factor(disliked_songs$outcome)


par(mfrow=c(1,2))
par(mfrow=c(1,1))
#### PCA

excl_labels_songs_pca = names(songs2) %in% c("type", "id", "uri", "track_href","outcome", "analysis_url", "track_name", "artist", "genres", "explicit", "mode", "key", "time_signature")
excl_labels_liked_pca = names(liked_songs) %in% c("type", "id", "uri", "track_href", "analysis_url", "track_name", "artist", "genres", "outcome", "explicit", "mode", "key", "time_signature")
excl_labels_disliked_pca = names(disliked_songs) %in% c("type", "id", "uri", "track_href", "analysis_url", "track_name", "artist", "genres", "outcome", "explicit", "mode", "key", "time_signature")

#songs2 = songs2[!excl_labels_songs_pca]

songs_pca = songs[!excl_labels_songs_pca]
liked_songs_pca = liked_songs[!excl_labels_liked_pca]
disliked_songs_pca = disliked_songs[!excl_labels_disliked_pca]

#pca with liked songs only
pca_liked = prcomp(liked_songs_pca, scale = TRUE )
autoplot(pca_liked, data=liked_songs_pca, col = "lightgreen", loadings = TRUE, loadings.label = TRUE, main = "PCA 1 and 2 from liked songs")
pca_liked
#1st pca: focus on: danceability, energy, loudness, acousticness and valence --> emotions/feelings translated by the track
#if the song has more a party vibe or a relax vibe for example

#2nd pca still include danceability but also focus on more factual aspect of the songs, such as its duration, tempo, popularity and year realease

#3rd pca on the other hand focus on instrumentalness, valence, duration and year
#makes a lot of sense if we think about it, it focus on longer songs, which likely are more instrumental (classical music, alternative rock
#jazz, electronic) and belong to specific period (year). They also are describing a certain mood that tend to be on the lower side of valance.
#ps: most classical music year is misclassified)

#4th pca focus on danceability, speechiness, instrumentalness, valence and liveness
#this pca probably focus on distinguishing speechiness and instrumentalness from track to track, as valance, liveness and
# danceability are directly affected

#pca with disliked songs
pca_disliked = prcomp(disliked_songs_pca, scale = TRUE)
autoplot(pca_disliked, data=disliked_songs_pca, col = "purple", loadings = TRUE, loadings.label = TRUE, main = "PCA 1 and 2 from disliked songs")
#1st PCA focus on: energy, loudness and acousticness --> energy and loudness are surely affected by each other and positevely correlated
#while acousicness is for sure negatively correlated with those 2 variables. This first PCA tells us that those variable focus
# ,among my disliked songs, are the most significant to make a first distinction between the tracks.

#2nd PCA focus on danceability, instrumentalness and duration: as danceability is surely negatively correlated with the 2 other
#factors

#3rd PCA focus on liveness, valence, popularity and year to describe a song

pca_disliked

#one pca with both, skyblue = liked, red = disliked
pca_songs = prcomp(songs_pca, scale = TRUE)
autoplot(pca_songs, data=songs_pca, loadings = TRUE, col = ifelse(songs$outcome == 1, "green", "purple"), loadings.label = TRUE,  main = "PCA 1 and 2 from both liked and disliked songs")
pca_songs

#1st PCA describing both disliked and liked songs focus on energy, loudness and acousticness
#as they seeem to be variable that significantly difer among liked and disliked songs it's not surprising to see that the main
#PCA rely on those, also acousticness is negatively correlated with energy and loudness, therefore including it make sense to classify a song on one side or the other

#2nd PCA focus specially on danceability and duration_ms
#again we can expect those 2 variables to be negatively correlated as longer tracks are usually calmer, and short track (pop) more aimed for dancing
#also they both discribe different trakcs, as the disliked songs are more toward danceability while my liked songs tend to be longer

### MODEL SELECTION

#ranom forest to evaluate predictors importance
pred_importance = randomForest(as.factor(songs$outcome)~danceability+energy+key+loudness+mode+speechiness+ acousticness + instrumentalness + liveness
                               +valence+tempo+duration_ms+time_signature+popularity+explicit, ntree=2000, data=songs, importance=TRUE)
pred_importance

importance(pred_importance)
pred_importance = randomForest(outcome~danceability+energy+loudness+speechiness+ acousticness + instrumentalness 
                               +valence+tempo+duration_ms+popularity+explicit, ntree=2000, data=songs, importance=TRUE)

#forest classifier
set.seed(123) 

songs_split <- createDataPartition( songs$outcome, p = 0.7, list = FALSE)
training_dataset  <- songs[songs_split, ] 
testing_dataset <- songs[-songs_split, ] 

classification_tree = rpart(outcome~danceability+energy+loudness+speechiness+ acousticness + instrumentalness 
                            +valence+tempo+duration_ms+popularity+explicit, control=rpart.control(cp=0.01), data=training_dataset, method = 'class')
prediction = predict(classification_tree, testing_dataset, type = "class")
testing_dataset$prediction = prediction
testing_dataset$pred_right = ifelse(testing_dataset$prediction == testing_dataset$outcome, 1, 0)
c = count(testing_dataset$pred_right, vars=1)
accuracy_classifier = c[2,2] / nrow(testing_dataset)
#accuracy 72.6%
summary(classification_tree)

#logistic regression



logit = glm(outcome ~ danceability+energy+loudness+speechiness+ acousticness + instrumentalness 
            +valence+tempo+duration_ms+popularity+explicit, family = "binomial", data=training_dataset)
summary(logit)

logic_pred = predict(logit, testing_dataset, type="response")
testing_dataset$prediction = ifelse(logic_pred >0.5, 1,0)
testing_dataset$pred_right = ifelse(testing_dataset$prediction == testing_dataset$outcome, 1, 0)
c_logic = count(testing_dataset$pred_right, vars=1)
accuracy_logit = c_logic[2,2] / nrow(testing_dataset)
#accuracy 0.76.2%


#random forest classifier

songs2=songs
songs2$outcome = as.factor(songs2$outcome)
randForest = randomForest(outcome~danceability+energy+loudness+speechiness+ acousticness + instrumentalness 
                               +valence+tempo+duration_ms+popularity+explicit, ntree=1000, data=songs2, importance=TRUE)

randForest
accuracy_random = 1-randForest$err.rate[997,1][["OOB"]]

#gradient boosting classifier

library(gbm)

songs_split <- createDataPartition( songs$outcome, p = 0.7, list = FALSE)
training_dataset  <- songs[songs_split, ] 
testing_dataset <- songs[-songs_split, ] 

gradientboosting = gbm(outcome~danceability+key+loudness+energy+speechiness+ acousticness + instrumentalness 
                       +valence+tempo+duration_ms+popularity+explicit, distribution="bernoulli", n.trees=1000, interaction.depth=3,
                       data=training_dataset)
summary(gradientboosting)

boosting_pred = predict(gradientboosting, testing_dataset, type="response")
testing_dataset$prediction = ifelse(boosting_pred > 0.5, 1, 0)
testing_dataset$pred_right = ifelse(testing_dataset$prediction == testing_dataset$outcome, 1, 0)
c_boosting = count(testing_dataset$pred_right, vars=1)
accuracy_boosting = c_boosting[2,2] / nrow(testing_dataset)
#acuracy 0.79


##plot mse for each model

models_accuracy = c(accuracy_boosting, accuracy_classifier, accuracy_logit, accuracy_random)
models_labels = c("Gradient Boosting", "Classification Trees", "Logistic Regression", "Random Forest")


barplot(models_accuracy, names.arg = models_labels, col= c("green", "orange", "yellow", "purple"), ylim=c(0,1), border =T, main = "Models' training performance")

genres_liked$freq
models_accuracy

#### tuning random forest and gradient

#trying to add genre
#top 25 genres:
genres_liked = count(liked_songs, "genres")
genres_liked = genres_liked[2:196,]
genres_liked = genres_liked[order(genres_liked$freq, decreasing = TRUE),][1:25,]
genres_liked[,1]
#if songs genre is in top 25, genre = 1, otherwise genre = 0
songs$genres = ifelse(genres %in% genres_liked[,1], 1, 0)
songs$genres = as.factor(songs$genres)

#tunning random forest

songs2 = songs
songs2$outcome = as.factor(songs2$outcome)
randForest = randomForest(outcome~danceability+energy+key+loudness+mode+speechiness+ acousticness + instrumentalness + liveness
                          +valence+tempo+duration_ms+time_signature+popularity+explicit+genres, ntree=1000, data=songs2, importance=TRUE)
randForest
accuracy_random = 1-randForest$err.rate[997,1][["OOB"]]

#tunning gradient boosting

songs_split <- createDataPartition( songs$outcome, p = 0.7, list = FALSE)
training_dataset  <- songs[songs_split, ] 
testing_dataset <- songs[-songs_split, ] 

gradientboosting = gbm(outcome~danceability+loudness+mode+ energy+ speechiness+ acousticness + key+ instrumentalness + liveness
                       +valence+tempo+duration_ms+time_signature+popularity+explicit+genres, distribution="bernoulli", n.trees=1000, interaction.depth=3,
                       data=training_dataset)
summary(gradientboosting)

boosting_pred = predict(gradientboosting, testing_dataset, type="response")
testing_dataset$prediction = ifelse(boosting_pred > 0.5, 1, 0)
testing_dataset$pred_right = ifelse(testing_dataset$prediction == testing_dataset$outcome, 1, 0)
c_boosting = count(testing_dataset$pred_right, vars=1)
accuracy_boosting = c_boosting[2,2] / nrow(testing_dataset)

tuned_models_accuracy = c(accuracy_boosting, accuracy_random)
tuned_models_labels = c("Gradient Boosting", "Random Forest")
barplot(tuned_models_accuracy, names.arg = tuned_models_labels, col= c("green","purple"), ylim=c(0,1), border =T, main = "Tuned Models' Training Performance")

#adding genre predictors significantly improve gradient boosting model's performance

## FINAL MODEL ##
final_model = gbm(outcome~danceability+energy+loudness+mode+speechiness+ acousticness + instrumentalness + liveness
                  +valence+tempo+duration_ms+time_signature+popularity+explicit+genres, distribution="bernoulli", n.trees=1000, interaction.depth=3,
                  data=songs)


#### vALIDATION TEST ####

#full_test_true_outcome = c(0, 0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 0, 0, 1, 1, 1, 0, 1, 1, 0, 0, 1, 0, 1, 1)
full_test_true_outcome = c(1, 1, 0, 0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 0, 0, 1, 1, 1, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 0, 1, 1, 1)


full_test_playlist <- read.csv("C:/Users/jeanp/OneDrive - McGill University/Fall 2020/MGSC 401/Final Project/dataset/full_test_playlist.csv")

full_test_playlist$key = as.factor(full_test_playlist$key)
full_test_playlist$mode = as.factor(full_test_playlist$mode)
full_test_playlist$explicit = as.factor(full_test_playlist$explicit)
full_test_playlist$time_signature = as.factor(full_test_playlist$time_signature)
#full_test_playlist$artist = as.factor(full_test_playlist$artist)
full_test_playlist$genres = ifelse(full_test_playlist$genres %in% genres_liked[,1], 1, 0)
full_test_playlist$genres = as.factor(full_test_playlist$genres)

excl_columns_test= names(full_test_playlist) %in% c("type", "id", "uri", "track_href", "analysis_url")
full_test_playlist = full_test_playlist[!excl_columns_test]

#prediction
validation_prediction = predict(final_model, full_test_playlist, type="response")
full_test_playlist$prediction = ifelse(validation_prediction > 0.5, 1, 0)

full_test_playlist$outcome = full_test_true_outcome
full_test_playlist$pred_right = ifelse(full_test_playlist$prediction == full_test_playlist$outcome, 1, 0)
c_full_test = count(full_test_playlist$pred_right, vars=1)
accuracy_validation_test = c_full_test[2,2] / nrow(full_test_playlist)

full_test_playlist$prediction = as.factor(full_test_playlist$prediction)
full_test_playlist$outcome = as.factor(full_test_playlist$outcome)

matrix = confusionMatrix(full_test_playlist$prediction, full_test_playlist$outcome)
matrix
