library(readr)
library(tidyverse)
library(ggplot2)
library(plyr)

titles <- read_tsv("title.basics.tsv")

# filtering out shorts, TV shows, etc. so we only have movies
movies <- titles %>% filter(grepl("movie", titleType))

# eliminating movies that haven't been released yet (as of 2019
# to simplify)
movies <- movies %>% filter(startYear<2019)

# creating a visulization of movie releases over time
ggplot(data = movies, aes(startYear)) +
  geom_histogram() +
  theme_minimal() +
  labs(x = "Release Year", y = "Movies", title = "Number of Movies Released")

# loading ratings data
ratings <- read_tsv("title.ratings.tsv")
ratings <- ratings %>% filter(numVotes>1000)

# plotting average ratings by number of votes
ggplot(data = ratings, aes(x = numVotes, y = averageRating)) +
  geom_jitter() +
  theme_bw() +
  labs(x = "number of votes", y = "average rating", title = "IMDB movie ratings")

# merging the movies and ratings data set
newdata <- join(movies, ratings, by="tconst", type = "inner")

# plotting
ggplot(data = newdata, aes(x = runtimeMinutes, y = averageRating)) +
  geom_jitter() +
  theme(axis.text.x = element_blank(), axis.ticks = element_blank()) +
  labs(x = "runtime", y = "average rating", title = "Rating by Runtime")
