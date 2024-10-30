library(tidyverse)
library(ggcorrplot)

#load data
spotify = read.csv("top_10000_1950-now.csv")

#column names and classes
colnames(spotify)
lapply(spotify, class)

#missing data in each column
lapply(spotify, function(x) sum(is.na(x)))

# find indexes of NAs in Danceability column
danceabilityNA = spotify[is.na(spotify$Danceability), ]

# since all the NAs are between danceability and time.signature columns,
# we find the column index of both so we can simply output the results 
# and see if all the NAs exist in one area
first_index = which(colnames(danceabilityNA) == "Danceability")
second_index = which(colnames(danceabilityNA) == "Time.Signature")

head(danceabilityNA[, first_index:second_index])

# delete album genres column
spotify = spotify[, -c(which(colnames(spotify) == "Album.Genres"))] 
spotify = drop_na(spotify)

#clean up year column
spotify$Release.Year = lapply(spotify$Album.Release.Date, 
                              function(x) substr(x, 1, 4))

#histrogram of years in popularity
hist(as.numeric(spotify$Release.Year),
     xlab = "Years From 1950 to 2030",
     ylab = "Frequency",
     main = "Distribution of Years in Popularity Variable",
     breaks = "FD",
     xlim = c(1950, 2030))

#same plot with nonzero popularity
hist(spotify$Popularity[!spotify$Popularity == 0 ],
     xlab = "Popularity Score",
     ylab = "Frequency",
     main = "Distribution of Non-Zero Popularity Scores",
     breaks = "FD")

spotify_nonzero_pop <- spotify[!spotify$Popularity == 0, ]


#artists with most songs on the list
spotify %>%
  group_by(Artist.Name.s.) %>%
  summarise(n()) %>%
  top_n(10)

#genres with the most songs on the list
spotify %>%
  group_by(Artist.Genres) %>%
  summarise(n()) %>%
  top_n(10)


#scatterplots for numeric variables

#all popularity scores
for (column in colnames(spotify)) {
  if (is.numeric(spotify[[column]])) {
    
    #generate scatter plots for numerical variables with popularity as the y
    plt <- ggplot(spotify, aes_string(x = column, y = "Popularity")) +
            geom_point(alpha = 0.1) + 
            geom_rug(alpha = 0.01) +
            xlab(column) +
            ylab("Popularity")
    
    #save plots locally; remove if you want
    png(filename = paste0("assets/", column, "_scatter.png"))
    plot(plt)
    dev.off()
  }
}

#nonzero popularity scores
for (column in colnames(spotify)) {
  if (is.numeric(spotify[[column]])) {
    
    #generate scatter plots for numerical variables with popularity as the y
    plt <- ggplot(spotify_nonzero_pop, aes_string(x = column, y = "Popularity")) +
      geom_point(alpha = 0.1) + 
      geom_rug(alpha = 0.01) +
      xlab(column) +
      ylab("Popularity (Nonzero)")
    
    #save plots locally; remove if you want
    png(filename = paste0("assets/", column, "_nonzero_scatter.png"))
    plot(plt)
    dev.off()
  }
}

#boxplots for categorical variables
plt <- ggplot(spotify_nonzero_pop, aes(x = Explicit, y = Popularity)) +
    geom_boxplot() + 
    xlab("Explicit") +
    ylab("Popularity (Nonzero)")

png(filename = "assets/explicit_boxplot.png")
plot(plt)
dev.off()


plt <- ggplot(spotify_nonzero_pop, aes(x = Artist.Genres, y = Popularity)) +
  geom_boxplot() + 
  xlab("Explicit") +
  ylab("Popularity (Nonzero)")

png(filename = "assets/explicit_boxplot.png")
plot(plt)
dev.off()

#store important looking numeric variables
numeric <- c("Acousticness", 
             "Danceability",
             "Energy",
             "Liveness",
             "Loudness",
             "Speechiness",
             "Tempo",
             "Valence")

#correlation matrix
corr <- round(cor(spotify[numeric]), 3)
plt <- ggcorrplot(corr, type="upper", lab = TRUE)

png(filename = "assets/correlation_plot.png")
plot(plt)
dev.off()

#loudness vs energy scatter plot
plt <- ggplot(spotify_nonzero_pop, aes(x = Energy, y = Loudness)) +
  geom_point(alpha = 0.1) + 
  geom_rug(alpha = 0.01) +
  xlab("Energy") +
  ylab("Loudness")

png(filename = "assets/energy_loudness_scatter.png")
plot(plt)
dev.off()

#loudness vs energy scatter plot
plt <- ggplot(spotify_nonzero_pop, aes(x = Acousticness, y = Loudness)) +
  geom_point(alpha = 0.1) + 
  geom_rug(alpha = 0.01) +
  xlab("Acousticnesss") +
  ylab("Loudness")

png(filename = "assets/acousticness_loudness_scatter.png")
plot(plt)
dev.off()

#valence vs danceability
plt <- ggplot(spotify_nonzero_pop, aes(x = Valence, y = Danceability)) +
  geom_point(alpha = 0.1) + 
  geom_rug(alpha = 0.01) +
  xlab("Valence") +
  ylab("Danceability")

png(filename = "assets/valence_danceability_scatter.png")
plot(plt)
dev.off()


#initial model
m <- lm(Popularity ~ Danceability + Energy + Tempo + Explicit + Valence + 
          Loudness + Track.Duration..ms., 
          data = spotify_nonzero_pop)
summary(m)
