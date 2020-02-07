# importing nfl player stats
system("wget -O nfl.json 'https://api.fantasy.nfl.com/v1/players
       /stats?statType=seasonStats&season=2010&week=1&format=json'")

# printing the file
system("cat nfl.json")

# converting from JSON to dataframe
library(jsonlite)
mydf <- fromJSON('nfl.json')

# examining the dataframe
class(mydf)
class(mydf$players)
head(mydf$players)
