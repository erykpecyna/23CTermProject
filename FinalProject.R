#Kodi Obika
#Eryk Pecyna
#Additional Point 22 - Team consists of two members

#This code is for getting Spotify's data Billboard Top 100s from 1960-2015 and writing the relevant
#data to a csv, run it once then never again

#library(billboard)
#dirty <- spotify_track_data
#tracks <- data.frame(year = dirty$year, explicit = dirty$explicit, danceability = dirty$danceability,
#                     energy = dirty$energy, key = dirty$key, loudness = dirty$loudness,
#                     mode = dirty$mode, speechiness = dirty$speechiness, acousticness = dirty$acousticness,
#                     instrumentalness = dirty$instrumentalness, liveness = dirty$liveness,
#                     valence = dirty$valence, tempo = dirty$tempo, duration_ms = dirty$duration_ms,
#                     time_signature = dirty$time_signature)
#
#write.csv(tracks, "TrackData.csv")
#
#data <- read.csv("TrackData.csv")
#
#N <- length(data$danceability)
#Position <- numeric(N)
#for(i in 1:N) {
#  diff <- which(data$year == data$year[i])[1] - 1
#  Position[i] <- data$X[i]-diff
#}
#
#tracks <- cbind(Position, tracks)
#
#write.csv(tracks, "TrackData.csv")


#Additional Point 1 - lots of columns baby
#Additional Point 2 - The data set is large enough that we can use it as a population to
#                     draw samples from
data <- read.csv("TrackData.csv")

par(mar=c(3,3,1,1))

hist(data$danceability,
     breaks="FD", 
     freq=FALSE,
     col="darkmagenta",
     xlab = "Danceability",
     main = "Danceability Probabilty Density")

plot(data$energy, data$Position)


linreg <- function(xCol, yCol, xLabel = "X", yLable = "Y") {
  #Basis Vectors for the vector space single degree polynomial functions
  m1 <- rep(1, length(xCol))
  m2 <- xCol
  
  #Projection Matrix approach
  A <- cbind(m1, m2)
  B <- t(A)%*%A
  BInv <- solve(B)
  P <- A%*%BInv%*%t(A)
  Projection <- P%*%yCol
  
  plot(xCol, yCol, pch = ".", cex = 2)
  points(xCol, Projection, col = "darkmagenta", pch =".", cex = 3)
}

linreg(data$danceability, data$Position)

