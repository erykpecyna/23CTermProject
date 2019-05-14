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

mode_count <- c(sum(data$mode == 1), sum(data$mode == 0))
barplot(mode_count, 
        xlab = "Mode", 
        ylab = "Song Counts", 
        col = "darkmagenta", 
        names.arg = c("Major", "Minor"), 
        main = "Representation of Each Mode in Billboard Hot 100",
        horiz = T)

par(mar=c(3,3,1,1))

hist(data$danceability,
     breaks="FD", 
     freq=FALSE,
     col="darkmagenta",
     xlab = "Danceability",
     main = "Danceability Probabilty Density")

<<<<<<< HEAD
=======
curve(dnorm(x, mean(data$danceability), sqrt(var(data$danceability))), add = TRUE, lwd = 3, lty = 4)

>>>>>>> b48eab012a67f1ddffde33288f1385cc04095a4a
# Additional point 11 - Graphics using ggplot
ggplot(data, aes(x = factor(mode))) + 
  geom_bar(stat = "count", width = 0.5, fill = "darkmagenta") + 
  theme_minimal() +
  xlab("Mode") +
  ylab("Song Count") +
  ggtitle("Representation of Each Mode in Billboard Hot 100") +
  scale_x_discrete(labels = c("Minor", "Major")) +
  coord_flip()

ggplot(data, aes(x = danceability)) + 
  geom_histogram(binwidth = 0.025, fill = "darkmagenta") +
  theme_minimal() +
  xlab("Danceability") +
  ggtitle("Danceability Probabilty Density")

<<<<<<< HEAD
curve(dnorm(x, mean = mean(data$danceability), sd = sqrt(var(data$danceability))), add = TRUE, lwd = 3, lty = 4)
=======


>>>>>>> b48eab012a67f1ddffde33288f1385cc04095a4a

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
# Additional point 8 - Wouldn't necessarily expect danceabilty to
# have a statistically significant relationship with songs being
# explicit, but it turns out it does
perm.test(data$danceability, data$explicit, FALSE, 10000)

clt <- function(y, n = 10000) {
  avg <- numeric(n)
  for (i in 1:n) {
    avg[i] <- mean(sample(y, 10))
  }
  hist(avg, probability = TRUE)
  curve(dnorm(x, mean(y), sd(y)/sqrt(10)), add = TRUE, col = "darkmagenta")
}
clt(data$danceability)

plot(data$energy, data$Position)

#Uses a projection matrix to do linear regression, plots the results, and returns the corellation
linreg <- function(xCol, yCol) {
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
  
  R2 <- var(Projection)/var(yCol)
  print("Correlation:")
  print(R2[1])
  R2[1]
}

#Reconstruct data using only first nV eigenvectors with biggest eigenvalues
PCA <- function(RawData, nV = 1) {
  #Numrow and numCol are required for later calcultaions
  numRow <- nrow(RawData)
  numCol <- length(RawData[1,])
  
  #This function returns a matric of numCols zero columns
  #It will be useful when reconstructing the data using 
  #less than all of the eigenvectors
  fillZeroes <- function(numCols) {
    matrix(0, nrow = numRow, ncol = numCols)
  }
  
  #Principal Components Analysis requires normalized data 
  Adjusted <- scale(RawData, center=TRUE, scale = c(rep(sqrt(numRow-1), numCol)))
  
  #Additional point 16: Appropriate use of covariance matrix
  S <- var(RawData)
  Eig <- eigen(S)
  
  #The eigenvectors of S provide a new basis of independent random variables
  #We can then use the most significant eigenvectors (largest eigenvalues)
  #to try and reconstruct the data with less information
  P <- Eig$vectors
  PInv <- solve(P)
  
  #This is the centered data represented in terms of the new basis of eigenvectors
  Data.eig <- Adjusted%*%P
  
  #Stripping out some of the data along some of the eigenvectors
  AStripped <- cbind(Data.eig[,1:nV], fillZeroes(numCol-nV))
  #Convert back to the old basis
  A.reconstruct <- AStripped%*%PInv
  
  #Undoing the scaling from before
  Reconstructed <- scale(A.reconstruct, scale = c(rep(1/sqrt(numRow-1), numCol)))
  Reconstructed <- scale(Reconstructed, center = -colMeans(RawData), scale = FALSE)
  
  #The data reconstructed without all of its eigenvectors
  Reconstructed
}


Attributes <- cbind(data$danceability, data$energy, data$loudness,
                    data$speechiness, data$acousticness, data$instrumentalness,
                    data$liveness, data$valence, data$tempo)
numRow <- nrow(data)
numCol <- length(Attributes[1,])
Adjusted <- scale(Attributes, center=TRUE, scale = c(rep(sqrt(numRow-1), numCol)))
S <- t(Adjusted)%*%Adjusted

Eig <- eigen(S)
Eig$values #The first two values are much bigger than the others

P <- Eig$vectors
PInv <- solve(P)

A.eig <- Adjusted%*%P

eigOrder <- data.frame(data$artist_name, v1=A.eig[,1], v2=A.eig[,2])
#This eigenvector seems to represent how fast a song is, as when we sort by
#this first eigenvector, it is almost the same as sorting by tempo
eigenOrderedv1 <- ((data[order(eigOrder$v1),]))
head(eigenOrderedv1$track_name); head(((data[order(data$tempo),]$track_name)))
#This eigenvector seems to represent how loud a song is, as loudness increases
#steadily when going down the matrix sorted by this eigenvector
eigenOrderedv2 <- ((data[order(eigOrder$v2),]))
head(eigenOrderedv2$loudness); tail(eigenOrderedv2$loudness)

#Converting back to the old basis yields the original adjusted data
A.reconstruct <- A.eig%*%PInv
head(A.reconstruct); head(Adjusted)

#undoing scaling
Reconstructed <- scale(A.reconstruct, scale = c(rep(1/sqrt(numRow-1), numCol)))
Reconstructed <- scale(Reconstructed, center = -colMeans(Attributes), scale = FALSE)
head(Reconstructed); head(Attributes)

#Reconstructing without using all of the eigenvectors
AStripped <- cbind(A.eig[,1], A.eig[,2], 0, 0, 0, 0, 0, 0, 0)
A.reconstruct <- AStripped%*%PInv
Reconstructed <- scale(A.reconstruct, scale = c(rep(1/sqrt(numRow-1), numCol)))
Reconstructed <- scale(Reconstructed, center = -colMeans(Attributes), scale = FALSE)
head(Reconstructed); head(Attributes)

#This is the same reconstruction yielded by the PCA function
head(PCA(Attributes, 2)); head(Reconstructed)

#As we can see, reconstructing from only the first eigenvalue yields
#a good reconstruction of tempo (column 9) but not much else
head(PCA(Attributes)); head(Attributes)

#Lets see which columns are most corellated with the others
meancorrelation <- function(dt, testcol) {
  total <- 0
  for(i in 1:ncol(dt)) {
    if(i != testcol) {
      total <- total + linreg(dt[,testcol], dt[,i])
    }
  }
  print("Mean Correlation:")
  print(total / (ncol(dt) - 1))
  total / (ncol(dt) - 1)
}

numericalData <- (cbind(data$Position, Attributes))  

N <- ncol(numericalData)
means <- numeric(N)
for(i in 1:N) {
  means[i] <- meancorrelation(numericalData, i)
}

#It looks like the third column (energy in this case) has the highest average correlation with the
#other columns
means

linreg(data$energy, data$danceability)
linreg(data$energy, data$loudness)
linreg(data$energy, data$speechiness)
linreg(data$energy, data$acousticness)
linreg(data$energy, data$instrumentalness)
linreg(data$energy, data$liveness)
linreg(data$energy, data$valence)
linreg(data$energy, data$tempo)


tbl <- table(data$explicit, data$mode); tbl
expected <- outer(rowSums(tbl), colSums(tbl))/sum(tbl); expected
chisq.test(data$explicit, data$mode)


# Additional point 15 - calculation and display of logistic regression
library(stats4)
logreg <- function(x, y, z) {
  tf <- (as.numeric(y == z))
  plot(x, tf)
  MLL <- function(alpha, beta) {
    -sum(log(exp(alpha+beta*x)/(1+exp(alpha+beta*x)))*tf
          + log(1/(1+exp(alpha+beta*x)))*(1-tf))
  }
  results <- mle(MLL, start = list(alpha = 0, beta = 0))
  results@coef
  curve(exp(results@coef[1]+results@coef[2]*x)/ (1+exp(results@coef[1]+results@coef[2]*x)),col = "blue", add=TRUE)
}

logreg(data$danceability, data$explicit, FALSE)
  

logreg(data$danceability, data$explicit, TRUE)


# Additional point 20 - Calculation of a confidence interval
me <- qt(0.95, 9) * sd(data$danceability) / sqrt(10)
mean(data$danceability) - me
mean(data$danceability) + me
