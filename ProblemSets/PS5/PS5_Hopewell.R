library(rvest)
# Question 3
# scraping the data from director Richard Linklater's wikipedia page
linklater <- read_html("https://en.wikipedia.org/wiki/Richard_Linklater")

# creating an object that isolates just the table of his feature films
feature.films <- linklater %>% html_nodes("#mw-content-text > div > table:nth-child(45)") %>%
  html_table(fill=TRUE)
feature.films

# checking to make sure this is a data frame
str(feature.films)

# it's a list of 1, so converting it to a df by selecting its only element
feature.films <- feature.films[[1]]
str(feature.films)

# now it's a data frame

# Question 4

# setting up my access to Twitter's API
library(twitteR)
requestURL = "https://api.twitter.com/oauth/request_token"
accessURL = "https://api.twitter.com/oauth/access_token"
authURL = "https://api.twitter.com/oauth/authorize"


setup_twitter_oauth(consumerKey,
                    consumerSecret,
                    accessToken,
                    accessSecret)

# searching Twitter for tweets using the hashtag #Oscars2020 within 200 miles
# of Norman
tweets <- searchTwitter('#Oscars2020', 
                        geocode='35.2225685120,-97.4394760132,200mi',  
                        n = 2000, retryOnRateLimit=1)

# converting to dataframe
tweets.df <- twListToDF(tweets) 
tweets.df

# I'm curious to see how many were retweets vs. original tweets
length(which(tweets.df$isRetweet == TRUE))

# How many were talking about the best picture winner, Parasite?
sum(length(grep("Parasite", tweets.df$text)),
length(grep("parasite", tweets.df$text)),
length(grep("Best Picture", tweets.df$text)),
length(grep("best picture", tweets.df$text)))

summary(tweets.df)


