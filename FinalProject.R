#library(billboard)
#dirty <- spotify_track_data
#tracks <- data.frame(year = dirty$year, explicit = dirty$explicit, danceability = dirty$danceability,
#                     energy = dirty$energy, key = dirty$key, loudness = dirty$loudness,
#                     mode = dirty$mode, speechiness = dirty$speechiness, acousticness = dirty$acousticness,
#                     instrumentalness = dirty$instrumentalness, liveness = dirty$liveness,
#                     valence = dirty$valence, tempo = dirty$tempo, duration_ms = dirty$duration_ms,
#                     time_signature = dirty$time_signature)
#write.csv(tracks, "TrackData.csv")

data <- read.csv("TrackData.csv")
