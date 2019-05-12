#Kodi Obika
#Eryk Pecyna
#Additional Point 22 - Team consists of two members

#This code is for getting Spotify's data Billboard Top 100s from 1960-2015 and writing the relevant
#data to a csv, run it once then never again

# library(billboard)
# dirty <- spotify_track_data
# tracks <- data.frame(year = dirty$year, explicit = dirty$explicit, danceability = dirty$danceability,
#                     energy = dirty$energy, key = dirty$key, loudness = dirty$loudness,
#                     mode = dirty$mode, speechiness = dirty$speechiness, acousticness = dirty$acousticness,
#                     instrumentalness = dirty$instrumentalness, liveness = dirty$liveness,
#                     valence = dirty$valence, tempo = dirty$tempo, duration_ms = dirty$duration_ms,

#                     time_signature = dirty$time_signature, artist_name = dirty$artist_name,
#                     track_name = dirty$track_name)

#write.csv(tracks, "TrackData.csv")

#data <- read.csv("TrackData.csv")

#N <- length(data$danceability)
#Position <- numeric(N)
#for(i in 1:N) {
#  diff <- which(data$year == data$year[i])[1] - 1
#  Position[i] <- data$X[i]-diff
#}

#tracks <- cbind(Position, tracks)
#write.csv(tracks, "TrackData.csv")


#Additional Point 1 - lots of columns baby
#Additional Point 2 - The data set is large enough that we can use it as a population to
#                     draw samples from
library(tidyverse)


data <- read.csv("TrackData.csv")

mode_lab <- c("Major", "Minor")
mode_count <- c(sum(data$mode == 1), sum(data$mode == 0))
barplot(mode_count, xlab = "Mode", ylab = "Counts", col = "darkmagenta", names.arg = mode_lab, main = "Mode Barplot")

par(mar=c(3,3,1,1))

hist(data$danceability,
     breaks="FD", 
     freq=FALSE,
     col="darkmagenta",
     xlab = "Danceability",
     main = "Danceability Probabilty Density")

curve(dnorm(x, mean(data$danceability), sqrt(var(data$danceability))), add = TRUE, lwd = 3, lty = 4)

perm.test <- function(x, y, z, n) {
  mu.z = mean(x[y == z])
  mu.nz = mean(x[y != z])
  permutations = numeric(n)
  for (i in 1:n) {
    resample = sample(x)
    zs = resample[1:(length(x)/2)]
    nzs = resample[(length(x)/2+1):length(x)]
    permutations[i] = mean(zs) - mean(nzs)
  }
  hist(permutations, freq = FALSE)
  test.statistic = mu.z - mu.nz
  abline(v = test.statistic, lwd = 3)
  print("p-value:")
  print(2*mean(permutations < test.statistic))
  var.p = var(x) * 2 / (length(x)/2)
  curve(dnorm(x, mean = 0, sd = sqrt(var.p)), add = TRUE, lwd = 3)
}
perm.test(data$danceability, data$explicit, FALSE, 10000)

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
  return((BInv%*%t(A)%*%yCol)[2])
}

linreg(data$danceability[data$year == 2014], data$Position[data$year == 2014])
linreg(data$energy, data$Position)
linreg(data$loudness, data$Position)
linreg(data$speechiness, data$Position)
linreg(data$energy, data$Position)


#Principal Components Analysis
Attributes <- cbind(data$danceability, data$energy, data$loudness, data$speechiness,
                    data$acousticness, data$instrumentalness, data$liveness, data$valence,
                    data$tempo)
m <- nrow(data)

A.phi <- scale(Attributes, center=TRUE, scale = c(rep(sqrt(m-1), 9)))
S <- t(A.phi)%*%A.phi

Eig <- eigen(S)
Eig$values #First two eigenvalues are by far the largest

P <- Eig$vectors
PInv <- solve(P)

A.eig <- A.phi%*%P

TRACK.eig <- data.frame(data$artist_name, v1=A.eig[,1])
eigstuffs <- ((data[order(TRACK.eig$v1),]))
eigs2015on <- eigstuffs[eigstuffs$year > 2014,]










linreg(data$danceability, data$Position)

tbl <- table(data$explicit, data$mode); tbl
expected <- outer(rowSums(tbl), colSums(tbl))/sum(tbl); expected
chisq.test(data$explicit, data$mode)

# Additional point 15 - calculation and display of logistic regression
logreg <- function(x, y, z) {
  tf <- (as.numeric(y == z))
  plot(x, wins)
  MLL <- function(alpha, beta) {
    -sum(log(exp(alpha+beta*x)/(1+exp(alpha+beta*x)))*tf
          + log(1/(1+exp(alpha+beta*x)))*(1-tf))
  }
  results <- mle(MLL, start = list(alpha = 0, beta = 0))
  results@coef
  curve(exp(results@coef[1]+results@coef[2]*x)/ (1+exp(results@coef[1]+results@coef[2]*x)),col = "blue", add=TRUE)
}

logreg(data$danceability, data$explicit, FALSE)



