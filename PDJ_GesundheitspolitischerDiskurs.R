###############################
# Stephan Pauli UZH Data Journalism Seminar 
# 10th June 2023 
# This file includes preprocessing steps to filter the dataframe 
#
###############################

setwd("/Users/stephanpauli/Downloads")

library(ggplot2)
library(tidyverse)
library(tm)
library(tidytext)
library(scales)
library(lubridate)

df <- readRDS("20190910.rds")

#Group to check counts per language 
grouped_df <- df %>%
  group_by(s_language_cld2) %>%
  summarise(count = n())

#filtered based on german and french 
filtered_df <- df %>%
  filter(s_language_cld2 %in% c("de", "fr"))

#filtered based on time frame 
filtered_df <- df %>%
  filter(d_date >= as.Date("2003-01-01"))

#lowercase all words 
filtered_df <- filtered_df %>%
  mutate(s_transcript = tolower(s_transcript))

#keywords to filter speeches
keywords <- c("spitalwesen", "spital", "spitäler", "spitalfinanzierung", "krankenkassenprämie", "krankenkassenprämien",
"système hospitalier", "hôpital", "hôpitaux", "financement des hôpitaux", "primes d'assurance maladie")

#filter based on hospital related words
filtered_df <- filtered_df %>%
  filter(grepl(paste(keywords, collapse = "|"), s_transcript, ignore.case = TRUE))

saveRDS(filtered_df, file = "hospitalspeeches.rds")


#read in  dictionary and df 
dic <- read.csv("wordsdictionary.csv", sep=";")
df <- readRDS("hospital_speeches.rds")

#create year variable 
df <- df %>%
  mutate(year = year(as.Date(d_date)))

#lowercase dic and include only word and sentiment 
dic <- dic %>%
  mutate(word = tolower(word))

dic <- dic %>% 
  select(c("word","sentiment"))

#create sentiment score 
result <- df %>%
  mutate(linenumber = row_number()) %>% #line number for later sentence grouping 
  unnest_tokens(word, s_transcript) %>% #tokenisation - sentence to words
  inner_join(dic) %>% # inner join with our lexicon to get the polarity score
  group_by(linenumber) %>% #group by for sentence polarity
  summarise(sentiment = sum(sentiment)) %>% # final sentence polarity from words
  left_join(
    df %>%
      mutate(linenumber = row_number()) #get the actual text next to the sentiment value
  ) #%>% write.csv("sentiment_output.csv",row.names = FALSE)


#divide sentiment score by length of speech and squish it into -1 to 1 
result$test <- (result$sentiment / result$s_length_words) *100
result$test2 <- squish(result$test, range=c(-1,1))
summary(result$test2)


#Average sentiment per faction 
sentiment_per_faction <- result %>%
  group_by(s_speaker_faction) %>%
  summarise(mean_val = mean(test2))
sentiment_per_faction <- sentiment_per_faction[-c(1, nrow(df)), ]
sentiment_per_faction <- sentiment_per_faction[-12, ]
#create year column
result <- result %>%
  mutate(year = year(as.Date(d_date)))

#average sentiment per year 
sentiment_per_year <- result %>%
  group_by(year) %>%
  summarise(mean_val = mean(test2))

#number of speeches
number_of_speeches <- df %>%
  group_by(year) %>%
  summarise(count = n())

# Create a vector of colors based on the sentiment values
colors <- ifelse(sentiment_per_year$mean_val > 0, "blue", "red")
# Create the bar plot and set the colors based on the sentiment values
barplot(sentiment_per_year$mean_val,
        names.arg = sentiment_per_year$year,
        xlab = "Year",
        ylab = "Sentiment",
        ylim = c(-0.15, 0.15),
        col = colors)

#plot sentiment per faction 
party_colors <- c("yellow", "orange", "orange", "orange", "olivedrab", "green", "blue", "blue", "blue", "red", "darkgreen")
party_names <- c("BDP", "CVP", "CVP/EVP", "CVP/EVP/GLP", "Grüne", "GLP", "Liberale Fraktion (vor 2005)", "FDP (vor 2005)", "FDP.DieLiberalen", "SP", "SVP")
barplot(sentiment_per_faction$mean_val, names.arg = party_names, 
        xlab = "Faction", ylab = "Sentiment", ylim=c(-0.4,0.2), col = party_colors)


#plot number of speeches per year 
barplot(number_of_speeches$count, names.arg = number_of_speeches$year, xlab="Year",ylab="Number of Speeches", col = "goldenrod")




