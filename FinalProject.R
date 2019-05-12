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

for(i in 1960:2015) print(sum(data$year== i))

curve(dnorm(x, mean(data$danceability), sqrt(var(data$danceability))), add = TRUE, lwd = 3, lty = 4)

perm.test <- function(x, y, z, n) {
  mu.z <- mean(x[y == z])
  mu.nz <- mean(x[y != z])
  permutations <- numeric(n)
  for (i in 1:n) {
    resample = sample(x)
    zs = resample[1:(length(x)/2)]
    nzs = resample[(length(x)/2+1):length(x)]
    permutations[i] = mean(zs) - mean(nzs)
  }
  hist(permutations, freq = FALSE)
  test.statistic = mu.z - mu.nz
  print(2*mean(permutations < test.statistic))
  sigma.2 = var(x)
  var.p = sigma.2 * 2 / (length(x)/2)
  curve(dnorm(x, mean = 0, sd = sqrt(var.p)), add = TRUE, lwd = 3)
}
perm.test(data$danceability, data$mode, 1, 10000)

perm.test <- function(x, y, z, n) {
  actualdiff <- mean(x[y == z]) - mean(x[y != z])
  diffs <- numeric(n)
  for (i in 1:n) {
    hi <- sample(x)
    hey <- mean(x[y == z])
    hello <- mean(x[y != z])
    diffs[i] <- hey - hello
  }
  diffs
  hist(diffs, freq = F)
  abline(v = actualdiff, col = "Blue")
  print(mean(diffs < actualdiff) * 2)
  dated <- sd(diffs)
  curve(dnorm(x, mean = 0, sd = dated), add = TRUE, lwd = 3, col = "blue")
}

perm.test(data$danceability, data$mode, 1, 10000)


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



mean(data$danceability)
var(data$danceability)
# Find mean and variance for men alone
mu.m = mean(data$danceability[data$explicit == T]); mu.m
var(data$danceability[data$explicit == T])
# Find mean and variance for women alone
mu.w = mean(data$danceability[data$explicit != T]); mu.w
var(data$danceability[data$explicit != T])

# B
# Find body temperatures
temps = data$danceability
# Conduct permutation test
n = 10000
permutations = numeric(n)
for (i in 1:n) {
  resample = sample(temps)
  men = resample[1:65]
  women = resample[66:130]
  permutations[i] = mean(men) - mean(women)
}
# Generate histogram
hist(permutations, freq = FALSE)

# C
# Overlay normal density function plot on histgram
test.statistic = mu.m - mu.w
abline(v = test.statistic, lwd = 3)
2 * mean(permutations < test.statistic)
sigma.2 = var(data$BodyTemp)
var.p = sigma.2 * 2 / 65
curve(dnorm(x, mean = 0, sd = sqrt(var.p)), add = TRUE, lwd = 3)


