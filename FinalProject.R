# Kodi Obika
# Eryk Pecyna
# Additional point 22 - Team consists of two members

# This code is for getting Spotify's data Billboard Top 100s from 1960-2015 and writing the relevant
# data to a csv file; run it once then never again

install.packages(c("billboard", "tidyverse", "stats4"))
library(billboard)
library(tidyverse)
library(stats4)
dirty <- spotify_track_data
tracks <- data.frame(year = dirty$year, explicit = dirty$explicit, danceability = dirty$danceability,
                     energy = dirty$energy, key = dirty$key, loudness = dirty$loudness,
                     mode = dirty$mode, speechiness = dirty$speechiness, acousticness = dirty$acousticness,
                     instrumentalness = dirty$instrumentalness, liveness = dirty$liveness,
                     valence = dirty$valence, tempo = dirty$tempo, duration_ms = dirty$duration_ms,
                     time_signature = dirty$time_signature, artist_name = dirty$artist_name,
                     track_name = dirty$track_name)

N <- length(tracks$danceability)
Position <- numeric(N)
for(i in 1:N) {
  diff <- which(data$year == data$year[i])[1] - 1
  Position[i] <- data$X[i]-diff
}

tracks <- cbind(Position, tracks)
write.csv(tracks, "TrackData.csv")
data <- read.csv("TrackData.csv")

par(mar=c(3,3,1,1))

# Function to conduct permutation tests
perm.test <- function(x, y, z) {
  n <- dim(data)[1]
  match <- y == z
  n.m <- sum(match)
  x.m <- x[match]
  x.nm <- x[!match]
  mu.m <- mean(x.m)
  mu.nm <- mean(x.nm)
  diff.x <- mu.m - mu.nm
  N <- 10^4
  perms <- numeric(N)
  for (i in 1:N) {
    m.index = sample(1:n, size = n.m)
    m.mean = mean(x[m.index])
    nm.mean = mean(x[-m.index])
    perms[i] = m.mean - nm.mean
  }
  
  hist(perms,
       col = "darkmagenta",
       main = "Histogram of Permuted Statistics",
       xlab = "Permuted Statistics",
       ylab = "Relative Frequency",
       prob = TRUE)
  abline(v = diff.x, lwd = 3)
  
  p.val <- mean(perms > diff.x) * 2
  print("p-value:")
  p.val
}

# Uses a projection matrix to do linear regression, 
# plots the results, and returns the corellation
linreg <- function(xCol, yCol, xLabel = "X", yLabel = "Y") {
  # Basis Vectors for the vector space single degree polynomial functions
  m1 <- rep(1, length(xCol))
  m2 <- xCol
  # Projection Matrix approach
  A <- cbind(m1, m2)
  B <- t(A)%*%A
  BInv <- solve(B)
  P <- A%*%BInv%*%t(A)
  Projection <- P%*%yCol
  plot(xCol, yCol, pch = ".", cex = 2)
  points(xCol, Projection, col = "darkmagenta", pch = ".", cex = 3)
  R2 <- var(Projection)/var(yCol)
  print("Correlation:")
  print(R2[1])
}

# Reconstruct data using only first n eigenvectors with biggest eigenvalues
PCA <- function(RawData, nV = 1) {
  # Numrow and numCol are required for later calcultaions
  numRow <- nrow(RawData)
  numCol <- length(RawData[1,])
  # This function returns a matric of numCols zero columns
  # It will be useful when reconstructing the data using 
  # less than all of the eigenvectors
  fillZeroes <- function(numCols) {
    matrix(0, nrow = numRow, ncol = numCols)
  }
  # Principal Components Analysis requires normalized data 
  Adjusted <- scale(RawData, center=TRUE, scale = c(rep(sqrt(numRow-1), numCol)))
  # Additional point 16: Appropriate use of covariance matrix
  S <- var(RawData)
  Eig <- eigen(S)
  # The eigenvectors of S provide a new basis of independent random variables
  # We can then use the most significant eigenvectors (largest eigenvalues)
  # to try and reconstruct the data with less information
  P <- Eig$vectors
  PInv <- solve(P)
  # This is the centered data represented in terms of the new basis of eigenvectors
  # This is the centered data represented in terms of basis of eigenvectors
  Data.eig <- Adjusted%*%P
  # Stripping out some of the data along some of the eigenvectors
  A.reconstruct <- Data.eig%*%PInv
  Reconstructed <- scale(A.reconstruct, scale = c(rep(1/sqrt(numRow-1), numCol)))
  Reconstructed <- scale(Reconstructed, center = -colMeans(RawData), scale = FALSE)
  AStripped <- cbind(Data.eig[,1:nV], fillZeroes(numCol-nV))
  # Convert back to the old basis
  A.reconstruct <- AStripped%*%PInv
  # Undoing the scaling from before
  Reconstructed <- scale(A.reconstruct, scale = c(rep(1/sqrt(numRow-1), numCol)))
  Reconstructed <- scale(Reconstructed, center = -colMeans(RawData), scale = FALSE)
  # The data reconstructed without all of its eigenvectors
  Reconstructed
}

# Let's see which columns are most corellated with the others
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

ci <- function(x, p) {
  err <- qt(p, 9) * sd(x) / sqrt(10)
  print("Lower bound:")
  print(mean(x) - err)
  print("Upper bound:")
  print(mean(x) + err)
}
# Additional point 10 - Defining/using helper functions

data
# Required dataset standards 1 - Dataframe
# Required dataset standards 2 - At least two categorical/logical columns (explicit, 
#                                mode, key, etc.)
# Required dataset standards 3 - At least two numeric columns (danceability, energy,
#                                etc.)
# Required dataset standards 4 - Over 5000 rows 
# Additional point 1 - Data set with a lot of columns
# Additional point 2 - Data set is large enough that we can use it as a population to
#                      draw samples from

# Barplot displaying how much each key (irrespective of mode) was represented in data
key_count <- c(sum(data$key == 0),
               sum(data$key == 1),
               sum(data$key == 2),
               sum(data$key == 3),
               sum(data$key == 4),
               sum(data$key == 5), 
               sum(data$key == 6), 
               sum(data$key == 7),
               sum(data$key == 8), 
               sum(data$key == 9), 
               sum(data$key == 10),
               sum(data$key == 11))
key_lab <- c("C", "C#/Db", "D", "D#/Eb", "E", "F", "F#/Gb", "G", "G#/Ab", "A", "A#/Bb", "B")
barplot(key_count,
        xlab = "Key", 
        ylab = "Song counts", 
        col = "darkmagenta", 
        names.arg = key_lab,
        ylim = c(0, 700),
        main = "Representation of Each Key in Billboard Hot 100")
# Required graphical displays 1 - Barplot

# Histogram displaying probability density of danceability
hist(data$danceability,
     breaks="FD", 
     freq=FALSE,
     col="darkmagenta",
     xlab = "Danceability",
     main = "Danceability Probabilty Density")
# Required graphical displays 2 - Histogram

# 95% confidence interval of mean danceability
ci(data$danceability, 0.95)
# Additional point 20 - Calculation of confidence interval

# Can be roughly modeled by normal distribution
curve(dnorm(x, mean = mean(data$danceability), sd = sqrt(var(data$danceability))), add = TRUE, lwd = 3, lty = 4)
# Required graphical displays 3 - Normal distribution overlaid on histogram

# Ggplot barplot displaying how much each mode was represented in data
data %>% 
 ggplot(aes(x = factor(mode))) + 
   geom_bar(stat = "count", width = 0.5, fill = "darkmagenta") + 
   theme_minimal() +
   xlab("Mode") +
   ylab("Song Count") +
   ggtitle("Representation of Each Mode in Billboard Hot 100") +
   scale_x_discrete(labels = c("Minor", "Major")) +
   coord_flip()

# Ggplot barplot displaying how much each key (with mode) was represented in data
keymode_lab <- c("C", "Cm", "C#/Db", "C#m/Dbm", "D", "Dm", "D#/Eb", 
                 "D#m/Ebm", "E", "Em", "F", "Fm", "F#/Gb", "F#m/Gbm", 
                 "G", "Gm", "G#/Ab", "G#m/Abm", "A", "Am", "A#/Bb", 
                 "A#m/Bbm", "B", "Bm")
data %>% 
  select(key, mode) %>% 
  group_by(key, mode) %>% 
  mutate(n = n()) %>% 
  unique() %>% 
  arrange(desc(mode)) %>%
  arrange(key) %>% 
  ungroup() %>% 
  mutate(keymode = keymode_lab) %>% 
  arrange(n) %>% 
  select(keymode, n) %>% 
  ggplot(aes(x = reorder(keymode, n), y = n)) + 
  geom_bar(stat = "identity", fill = "darkmagenta") +
  ggtitle("Representation of Each Key (with Modality) in Billboard Hot 100") +
  xlab("Key (with Modality)") +
  ylab("Song Count") + 
  coord_flip() + 
  theme_bw()

# Ggplot line graph showing change in mean tempo over time
data %>%
  select(year, tempo) %>% 
  group_by(year) %>% 
  mutate(mean_tempo = mean(tempo)) %>% 
  select(year, mean_tempo) %>% 
  unique() %>% 
  ggplot(aes(x = year, y = mean_tempo)) +
  geom_line(color = "darkmagenta", linetype = "solid", size = 1.25) + 
  ggtitle("Change in Mean Tempo of Billboard Hot 100 Songs Over Time")
  xlab("Year") +
  ylab("Mean Tempo") +
  theme_bw()
# Additional point 5 - Different graphical display (line graph)
# Additional point 11 - Ggplot graphics

# Contingency table relating explicitness to mode
tbl <- table(data$explicit, data$mode); tbl
# Required graphical display 4 - Contingency table

# Conduct chi square test on tbl
chisq.test(tbl)
# P-value of 0 implies statistically significant evidence
# that explicitness and mode are not independent
# Required analysis 3 - Analysis of contingency table

# Conduct permutation test on valence for explictness and mode
perm.test(data$valence, data$explicit, FALSE)
# P-value of 0.06 shows no statistically significant difference in
# valence for explicit vs non-explicit songs at 5% significance level
perm.test(data$valence, data$mode, 0)
# P-value of 0 shows statistically significant difference
# in valence for songs written in major vs minor mode
# Required analysis 1 - Permutation tests
# Required analysis 2 - P-values

# Required analysis 4 - The classical chi-square analysis gave us a less
#                       precise result for p-value than the simulation
#                       method (permutation test)

# Find logistic regression for loudness
# and explictness
logreg(data$loudness, data$explicit, TRUE)
# Additional point 15 - calculation and display of logistic regression

# Finding correlation between position and 
# energy, loudness, and speechiness
linreg(data$energy, data$Position)
linreg(data$loudness, data$Position)
linreg(data$speechiness, data$Position)
# All seem to have 0 correlation with chart position
# Additional point 14 - Use of linear regression

# Reconstructing the data through principal component analysis
Attributes <- cbind(data$danceability, data$energy, data$loudness,
                    data$speechiness, data$acousticness, data$instrumentalness,
                    data$liveness, data$valence, data$tempo)
numRow <- nrow(data)
numCol <- length(Attributes[1,])
Adjusted <- scale(Attributes, center=TRUE, scale = c(rep(sqrt(numRow-1), numCol)))
S <- t(Adjusted)%*%Adjusted
Eig <- eigen(S)
Eig$values # The first two values are much bigger than the others
P <- Eig$vectors
PInv <- solve(P)
A.eig <- Adjusted%*%P
eigOrder <- data.frame(data$artist_name, v1=A.eig[,1], v2=A.eig[,2])
# This eigenvector seems to represent how fast a song is, as when we sort by
# this first eigenvector, it is almost the same as sorting by tempo
eigenOrderedv1 <- ((data[order(eigOrder$v1),]))
head(eigenOrderedv1$track_name); head(((data[order(data$tempo),]$track_name)))
# This eigenvector seems to represent how loud a song is, as loudness increases
# steadily when going down the matrix sorted by this eigenvector
eigenOrderedv2 <- ((data[order(eigOrder$v2),]))
head(eigenOrderedv2$loudness); tail(eigenOrderedv2$loudness)
A.reconstruct <- A.eig%*%PInv
head(A.reconstruct); head(Adjusted)
Reconstructed <- scale(A.reconstruct, scale = c(rep(1/sqrt(numRow-1), numCol)))
Reconstructed <- scale(Reconstructed, center = -colMeans(Attributes), scale = FALSE)
head(Reconstructed); head(Attributes)
AStripped <- cbind(A.eig[,1], A.eig[,2], 0, 0, 0, 0, 0, 0, 0)
A.reconstruct <- AStripped%*%PInv
head(A.reconstruct); head(Adjusted)
Reconstructed <- scale(A.reconstruct, scale = c(rep(1/sqrt(numRow-1), numCol)))
Reconstructed <- scale(Reconstructed, center = -colMeans(Attributes), scale = FALSE)
head(Reconstructed); head(Attributes)
# This is the same reconstruction yielded by the PCA function
head(PCA(Attributes, 2)); head(Reconstructed)
# As we can see, reconstructing from only the first eigenvalue yields
# a good reconstruction of tempo (column 9) but not much else
head(PCA(Attributes)); head(Attributes)

# Finding average correlation between each factor
numericalData <- (cbind(data$Position, Attributes))  
N <- ncol(numericalData)
means <- numeric(N)
for(i in 1:N) {
  means[i] <- meancorrelation(numericalData, i)
}
means
# It looks like the third column (energy in this case) has
# the highest average correlation with the other columns

# Finding correlation between energy and other factors
linreg(data$energy, data$danceability)
linreg(data$energy, data$loudness)
linreg(data$energy, data$speechiness)
linreg(data$energy, data$acousticness)
linreg(data$energy, data$instrumentalness)
linreg(data$energy, data$liveness)
linreg(data$energy, data$valence)
linreg(data$energy, data$tempo)
# Loudness seems to have highest correlation
# with energy, while instrumentalness has lowest