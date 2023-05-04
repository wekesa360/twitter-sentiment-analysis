# Load necessary library
library(tidytext)   # for text mining
library(SnowballC)  # for text stemming
library(wordcloud2) # for wordcloud
library(magrittr)   # for pipe operators
library(ggplot2)    # for data visualization
library(dplyr)      # for data manipulation
library(forcats)    # for working with categorical variables
library(igraph)     # for network analysis
library(tidyr)      # provide functions to help tidy and reshape data
library(stringr)    # provides functions to manipulate strings

# load tweet data from .rds files
tweets <- readRDS("./data/bis1m.rds")

# Filter harrypotter hashtags
tweets_harrypotter <- tweets %>%
  filter(str_detect(hashtags, "harrypotter"))

# Extract only the "text" column from the tweets dataset and store in a new variable
tweets_text <- tweets_harrypotter$msg

# Convert tweets_text into a dataframe with one column called "text"
tweets_df <- data.frame(text = tweets_text, stringsAsFactors = FALSE)

# Remove mentions and URLs from the tweet text using regular expressions, tokenize the text,
# and remove stop words using the tidytext package
tweets_clean <- tweets_df %>%
  filter(!is.na(text)) %>%  # remove tweets with no text
  mutate(text = gsub("http\\S+\\s*", "", text)) %>%  # remove urls
  tidytext::unnest_tokens(word, text,) %>%   # tokenize text
  anti_join(tidytext::stop_words) %>%      # remove stop words
  filter(!word %in% c("amp", "rt")) %>%   # remove amp and rt that are not captured by stop words
  filter(!grepl("[0-9]", word)) %>%  # remove words that contain numeric characters
  count(word, sort = TRUE) %>%
  filter(!word %in% c("   ", "    ", "      ", "   bnb", "   ive"))   # filter out non-meaningful words

# Remove the vulgar words from the data
tweets_clean <- tweets_clean %>%
  mutate(word = str_remove_all(word, "fuck")) %>%
  mutate(word = str_remove_all(word, "slut"))

# Create a bar plot of the top 10 meaningful words in the tweet text
top_words <- tweets_clean %>% 
  slice_max(n, n = 10) %>%   # select top 10 words by count
  mutate(word = fct_reorder(word, n))   # reorder the words by count
ggplot(top_words, aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "dodgerblue3") +
  coord_flip() +
  scale_y_continuous(labels = scales::comma_format()) + # set axis labels to integer format
  labs(x = NULL, y = "Count", title = "Top 10 Meaningful Words") +
  theme_classic()

wordcloud2(tweets_clean, size = 2.5, minSize = 5, color = "random-dark", backgroundColor = "white")


# Create a bar plot of the top 10 users with the most tweets
top_users <- tweets_harrypotter %>% 
  count(displayname, sort = TRUE) %>% 
  slice_max(n = 10, order_by = n) %>%
  slice(1:10)
ggplot(top_users, aes(x = reorder(displayname, n), y = n)) + 
  geom_col(fill = "dodgerblue3") + 
  coord_flip() + 
  labs(x = NULL, y = "Count", title = "Top 10 Users") +
  theme_classic()

# Select the "displayname" and "msg" columns from the "tweets_harrypotter" data frame
# and rename the columns to "name" and "text", respectively.
tweets_user_df <- tweets_harrypotter %>%
  select(name = displayname, text = msg) %>%
  # Filter out rows where the "text" column is NA.
  filter(!is.na(text))

# Group the tweets by user and calculate summary statistics.
tweets_summary <- tweets_user_df %>% 
  group_by(name) %>% 
  summarize(
    # Number of tweets per user
    n_tweets = n(),
    # Number of words per user (excluding NAs)
    n_words = sum(!is.na(text)),
    # Percentage of tweets that are retweets (assumed to be 100% here)
    retweet_percentage = 1
  ) %>% 
  # Filter out users with fewer than 10 tweets
  filter(n_tweets >= 10) %>% 
  # Sort users by number of tweets (descending)
  arrange(desc(n_tweets))

# Calculate the percentage of tweets that are retweets
retweet_percentage <- mean(stringr::str_detect(tweets_harrypotter$msg, "^RT")) * 100

# Load the NRC sentiment lexicon
nrc <- get_sentiments("nrc")

# Join the sentiment lexicon to the cleaned tweets
tweets_sentiments <- tweets_clean %>% 
  inner_join(nrc)


# Create a grouped bar chart to display the top three words with the highest count for each NRC category
top_tweets_nrc <- tweets_sentiments %>%
  group_by(sentiment) %>%
  top_n(3, n) %>% # keep only the top 3 words for each sentiment
  ungroup() %>%
  arrange(sentiment, word) %>% # arrange by sentiment and word
  group_by(sentiment) %>%
  slice(1:3) %>% # keep only the top 3 words in alphabetical order
  ungroup()

ggplot(top_tweets_nrc, aes(x = word, y = n, fill = sentiment)) + # specify x and y axis and fill color
  geom_col(position = "dodge") + # create a bar chart with position set to dodge
  labs(x = "Sentiment", y = "Count", fill = "NRC Category") + # add axis and legend labels
  scale_fill_discrete(guide = guide_legend(reverse = TRUE)) + # reverse the legend order
  facet_wrap(~ sentiment, ncol = 2, scales = "free") + # add facet wrap to separate charts by sentiment
  theme_bw() # apply a black and white theme

# Load the stop words data
data(stop_words)


# Separate the bigrams into individual words
tweets_bigrams <- tweets_harrypotter %>%
  unnest_tokens(bigram, msg, token = "ngrams", n = 2, drop = FALSE) %>%
  separate(bigram, c("word1", "word2"), sep = " ", remove = FALSE)
  
tweets_bigrams <- tweets_bigrams %>%
  select(bigram, word1, word2)

# Filter out bigrams with stop words
tweets_bigrams <- tweets_bigrams %>%
  filter(!word1 %in% stop_words$word) %>% # remove rows where word1 is in the stop words list
  filter(!word2 %in% stop_words$word)    # remove rows where word2 is in the stop words list

# Get the dimensions of the filtered bigrams data frame
dim(tweets_bigrams)

# Count the occurrences of each bigram and sort in descending order
tweets_bigrams_counts <- tweets_bigrams %>%
  count(word1, word2, sort = T)

# Unite the word1 and word2 columns into a single column called bigram
tweets_bigrams_united <- tweets_bigrams %>%
  unite(bigram, word1, word2, sep = " ")

# Create a graph from the bigram data frame
bigram_graph <- graph_from_data_frame(tweets_bigrams_counts[, c("word1", "word2")], directed = FALSE)

# Select a seed node
set.seed(100)
seed_node <- sample(V(bigram_graph), 1)

# Plot the graph with the seed node marked
plot(bigram_graph, vertex.size = 2, edge.width = 0.5, edge.arrow.size = 0.5, 
     vertex.label.color = "black", vertex.label.dist = 0.5, mark.vertex = seed_node)

# Display the network graph
bigram_graph

# write dataframes to a CSV file
write.csv(tweets_sentiments, "~/Desktop/sentiment_analysis/output/tweets_sentiments.csv", row.names = FALSE)
write.csv(top_tweets_nrc, "~/Desktop/sentiment_analysis/output/tweets_clean", row.names = FALSE)
write.csv(top_tweets_nrc, "~/Desktop/sentiment_analysis/output/tweets_summary.csv", row.names = FALSE)
write.csv(top_tweets_nrc, "~/Desktop/sentiment_analysis/output/tweets_user_df.csv", row.names = FALSE)