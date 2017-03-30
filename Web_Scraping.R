# Tutorial provided by Analytics Vidhya
# Load the Library for Web scraping
library(rvest)
library(ggplot2)
# Load the URL
url <- 'http://www.imdb.com/search/title?count=100&release_date=2016,2016&title_type=feature'
# Reading the HTML code from Website
webpage <- read_html(url)
#Now, we'll be scraping the following data from this website.

# Rank: The rank of the film from 1 to 100 on the list of 100 most popular feature films released in 2016.
# Title: The title of the feature film.
# Description: The description of the feature film.
# Runtime: The duration of the feature film.
# Genre: The genre of the feature film,
# Rating: The IMDb rating of the feature film.
# Metascore: The metascore on IMDb website for the feature film.
# Votes: Votes cast in favor of the feature film.
# Gross_Earning_in_Mil: The gross earnings of the feature film in millions.
# Director: The main director of the feature film. Note, in case of multiple directors, I'll take only the first.
# Actor: The main actor of the feature film. Note, in case of multiple actors, I'll take only the first.

#Using CSS selectors to scrap the rankings section
rank_data_html <- html_nodes(webpage,'.text-primary')
#Converting ranking data to text
rank_data <- html_text(rank_data_html)
# Let's check ranking data
head(rank_data)
# Converting data to numeric
rank_data <- as.numeric(rank_data)
#Test again
head(rank_data)
#Using CSS selectors to scrap the title section
title_data_html <- html_nodes(webpage, '.lister-item-header a')
# Converint title data to text
title_data <- html_text(title_data_html)
# Let's check title
head(title_data)
#Using CSS selectors to scrap the description section
description_data_html <- html_nodes(webpage,'.ratings-bar+ .text-muted')
# Convert description data to text
description_data <- html_text(description_data_html)
head(description_data)

# Removing "\n"
description_data <- gsub("\n","", description_data)
head(description_data)
#Using CSS selectors to scrap the Movie runtime section
runtime_data_html <- html_nodes(webpage,'.text-muted .runtime')
# Converting data to text
runtime_data <- html_text(runtime_data_html)

#Let's have a look at the runtime
head(runtime_data)

#Removing min from data and then converting it to numeric
runtime_data <- gsub("min","",runtime_data)
runtime_data <- as.numeric(runtime_data)
#Test data
head(runtime_data)

#Using CSS selectors to scrap the Movie genre section
genre_data_html <- html_nodes(webpage,'.genre')

#Converting the genre data to text
genre_data <- html_text(genre_data_html)

#Let's have a look at the runtime
head(genre_data)

# Removing excess spacefrom data
genre_data <- gsub(" ", "", genre_data)
#Let's check'
head(genre_data)
# Let's select only one genre from each movie
genre_data <- gsub(",.*","",genre_data)
# Converting text to factor
genre_data <- as.factor(genre_data)
#Let's Check
head(genre_data)
#Using CSS selectors to scrap the IMDB rating section
rating_data_html <- html_nodes(webpage,'.ratings-imdb-rating strong')

#Converting the ratings data to text
rating_data <- html_text(rating_data_html)

#Let's have a look at the ratings
head(rating_data)
#Data-Preprocessing: converting ratings to numerical
rating_data<-as.numeric(rating_data)

#Let's have another look at the ratings data
head(rating_data)
#Using CSS selectors to scrap the votes section
votes_data_html <- html_nodes(webpage,'.sort-num_votes-visible span:nth-child(2)')
# Converting votes to text
votes_data <- html_text(votes_data_html)
#Let's check
head(votes_data)
# Removing "," from data
votes_data <- gsub(",","",votes_data)
#Converting data to numeric
votes_data <- as.numeric(votes_data)
#Test data
head(votes_data)
#Using CSS selectors to scrap the directors section
directors_data_html <- html_nodes(webpage,'.text-muted+ p a:nth-child(1)')
# Converting it to text
directors_data <- html_text(directors_data_html)
# Converting directors to factors
directors_data <- as.factor(directors_data)
# Let's test
head(directors_data)

#Using CSS selectors to scrap the actors section
actors_data_html <- html_nodes(webpage,'.lister-item-content .ghost+ a')
# Converting it to text
actors_data <- html_text(actors_data_html)
# Converting it to factor
actors_data <- as.factor(actors_data)
#Let's check
head(actors_data)
#Using CSS selectors to scrap the metascore section
metascore_data_html <- html_nodes(webpage,'.metascore')

#Converting the runtime data to text
metascore_data <- html_text(metascore_data_html)

#Let's have a look at the metascore 
head(metascore_data)

#Convert to numeric
metascore_data <- gsub(" " ,"" , metascore_data)
#Let's test
head(metascore_data)
#Lets check the length of metascore data
length(metascore_data)
for (i in c(34,46,48,70,84,85,100)) {
  a <- metascore_data[1:(i-1)]
  b <- metascore_data[i:length(metascore_data)]
  metascore_data<-append(a,list("NA"))
  metascore_data<-append(metascore_data,b)
}
#Data-Preprocessing: converting metascore to numerical
metascore_data <- as.numeric(unlist(metascore_data[1:100]))
#Length of Metascore
length(metascore_data)
#Let's take a look at Summary 
summary(metascore_data)
#Using CSS selectors to scrap the gross revenue section
gross_data_html <- html_nodes(webpage,'.ghost~ .text-muted+ span')
#Converting data to text
gross_data <- html_text(gross_data_html)
#Let's check data
head(gross_data)
#Data-Preprocessing: removing '$' and 'M' signs
gross_data<-gsub("M","",gross_data)
gross_data<-substring(gross_data,2,6)
#Let's check the length of gross data
length(gross_data)
for (i in c(28,34,35,46,48,56,61,68,70,74,76,78,84,85,93,100)) {
  a <- gross_data[1:(i-1)]
  b <- gross_data[i: length(gross_data)]
  gross_data <- append(a, list("NA"))
  gross_data <- append(gross_data , b)
}
#Data-Preprocessing: converting gross to numerical
gross_data<-as.numeric(unlist(gross_data))
#Let's have another look at the length of gross data
length(gross_data)
#Let's check summary
summary(gross_data)


#Comnbining all lists to form a dataframe
movies_df<-data.frame(Rank = rank_data, Title = title_data,
                      
                      Description = description_data, Runtime = runtime_data,
                      
                      Genre = genre_data, Rating = rating_data,
                      
                      Metascore = metascore_data, Votes = votes_data,                                                             Gross_Earning_in_Mil = gross_data[1:100],
                      
                      Director = directors_data, Actor = actors_data)
#Structure of DataFrame
str(movies_df)
qplot(data = movies_df, Runtime, fill = Genre, bins = 30, main  = "Visualization of Movies and Runtime")

ggplot(movies_df, aes(x= Runtime, y = Rating)) + geom_point(aes(size = Votes, col = Genre)) + labs(title =  "Movie Runtime vs Rating")

ggplot(movies_df, aes(x= Runtime, y = Gross_Earning_in_Mil)) + geom_point(aes(size = Rating, col = Genre)) + labs(title = "Runtime vs Gross Earning")
