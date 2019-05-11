#Kodi Obika
#Eryk Pecyna
#Additional Point 22 - Team consists of two members

#This code is for getting Spotify's data Billboard Top 100s from 1960-2015 and writing the relevant
#data to a csv, run it once then never again

library(billboard)
dirty <- spotify_track_data
tracks <- data.frame(year = dirty$year, explicit = dirty$explicit, danceability = dirty$danceability,
                     energy = dirty$energy, key = dirty$key, loudness = dirty$loudness,
                     mode = dirty$mode, speechiness = dirty$speechiness, acousticness = dirty$acousticness,
                     instrumentalness = dirty$instrumentalness, liveness = dirty$liveness,
                     valence = dirty$valence, tempo = dirty$tempo, duration_ms = dirty$duration_ms,
                     time_signature = dirty$time_signature)

write.csv(tracks, "TrackData.csv")

#Additional Point 1 - lots of columns baby
#Additional Point 2 - The data set is large enough that we can use it as a population to
#                     draw samples from
data <- read.csv("TrackData.csv")

min.temp <- c("Major", "Minor")
max.temp <- c(sum(data$mode == 1), sum(data$mode == 0))
barplot(max.temp, xlab = "Mode", ylab = "Counts", col = "darkmagenta", names.arg = c("Major", "Minor"), main = "Mode Barplot")

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
  mean(x)
  var(x)
  mu.z = mean(x[y == z]); mu.z
  var(x[y == z])
  mu.nz = mean(x[y != z]); mu.nz
  var(x[y != z])
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
  2*mean(permutations < test.statistic)
  sigma.2 = var(x)
  var.p = sigma.2 * 2 / (length(x)/2)
  curve(dnorm(x, mean = 0, sd = sqrt(var.p)), add = TRUE, lwd = 3)
}
perm.test(data$danceability, data$explicit, TRUE, 10000)


