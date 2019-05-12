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
#                     time_signature = dirty$time_signature)
# 
# write.csv(tracks, "TrackData.csv")
# 
# data <- read.csv("TrackData.csv")
# 
# N <- length(data$danceability)
# Position <- numeric(N)
# for(i in 1:N) {
#  diff <- which(data$year == data$year[i])[1] - 1
#  Position[i] <- data$X[i]-diff
# }
# 
# tracks <- cbind(Position, tracks)
# 
# write.csv(tracks, "TrackData.csv")


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

for(i in 1960:2015) print(sum(data$year== i))

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
  

# Additional point 20 - Calculation of a confidence interval
me <- qt(0.95, 9) * sd(data$danceability) / sqrt(10)
mean(data$danceability) - me
mean(data$danceability) + me
