#############################################################################################################.
########### Created by: Matthew Naylor 13/06/26 
########### Objectives: Produce word clouds from computed CCI_df 
#############################################################################################################.


library(rvest)
library(stringr)
library(xlsx)
library(readtext)
library(tidyverse)
library(dplyr)
library(quanteda)
library(tm)
library(readxl)
library(xtable)
library(ggplot2)
library(ggpattern)
# library(magick)
library(stargazer)
library(dplyr)
library(xlsx)
library(tidyverse)
# library(hrbrthemes)
library(viridis)
library(forcats)
library(chemometrics)
library(zoo)
# library(wordcloud)
# library(wordcloud2)
library(ggwordcloud)

# import saved CCI_df
CCI_df


# filter to period of interest
CCI_df = CCI_df %>%
  dplyr::mutate(type = "Document Name")


setwd("/Users/matthewnaylor/Getting-through")
# load("jargon_dictionary_India.Rda")
load("jargon_dictionary_Mar26.Rda")

### Tokenise and compound the tokens ----------------------------------------

tokenise = function(text){
  # Get rid of punctuation for consistency with the dictionary structure
  text = text %>%
    str_replace_all("(('|’) )", " ") %>%
    str_replace_all("(-)", " ") %>%
    str_replace_all("(('|’)s)", "") %>%
    str_replace_all("(('|’)re)", "re") %>%
    str_replace_all("(('|’)ve)", "ve")
  # Convert the 'raw' text into tokens, convert to lower case, and 'stem' the terms. This is needed to then identify jargon words/phrases.
  as.character(lapply(text, tolower))%>% #convert to lower case
    quanteda::tokens(remove_numbers = TRUE, remove_punct = TRUE) %>% #convert to tokens
    tokens_wordstem(language = quanteda_options("language_stemmer")) #now we can stem the words using Porter Stemmer method -> so that 'policy' and 'policies' are not counted as separate word for e.g.
}

docs_tokens = tokenise(CCI_df$text)

# We start by identifying the jargon PHRASES, using the dict_phrases_raw_stem dictionary. We identify the consecutive words within the text that match dict_phrases_raw_stem (indicating they are jargon when used together). We then 'compound' those phrases that match (turning 'monetary policy' into 'monetary_policy') 
docs_tokens_compounded = tokens_compound(docs_tokens, pattern = phrase(dictionary_jargon_phrases))

# collapse tokens per document into one character string
text_compounded <- vapply(
  as.list(docs_tokens_compounded),
  function(x) paste(x, collapse = " "),
  character(1)
)

# keep doc names (optional)
names(text_compounded) <- quanteda::docnames(docs_tokens_compounded)

# add to your data if you like
CCI_df$text_compounded <- text_compounded


# --- word frequencies from your corpus ---
corpus <- Corpus(VectorSource(CCI_df$text_compounded)) %>%
  tm_map(content_transformer(tolower)) %>%      # make sure it's lower case
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords("english"))

dtm    <- TermDocumentMatrix(corpus)
mat    <- as.matrix(dtm)
words  <- sort(rowSums(mat), decreasing = TRUE)

df <- data.frame(
  word = names(words),
  freq = as.integer(words),
  stringsAsFactors = FALSE
)

# --- build a corpus-wide jargon set (tokens) ---
# assumes `jargon` is text with compounded tokens like "monetary_policy ..."
jargon_tokens <- CCI_df$jargon %>%
  { .[!is.na(.)] } %>%
  tolower() %>%
  paste(collapse = " ") %>%
  str_split("\\s+") %>%
  unlist() %>%
  discard(~ .x == "") %>%
  unique()

# --- tag words by whether they appear in the jargon set ---
set.seed(42)
df <- df %>%
  mutate(
    word_lc = tolower(word),
    type    = as.integer(word_lc %in% jargon_tokens),
    angle   = 90 * sample(c(0, 1), n(), replace = TRUE, prob = c(0.6, 0.4))
  ) %>%
  filter(freq > 2) %>%
  mutate(
    # do prettifying *after* the membership test
    word = str_replace_all(word, "_", " ")
  ) %>%
  select(word, freq, type, angle)

offenders <- df$word %>%
  unique() %>%
  set_names() %>%
  map_chr(~{
    err <- tryCatch({
      gridtext::richtext_grob(.x)  # your gridtext parses like ggwordcloud does
      NA_character_
    }, error = function(e) conditionMessage(e))
    err
  }) %>%
  discard(is.na)

length(offenders)                # how many problematic unique tokens
head(offenders, 5)               # see error messages
bad_words <- names(offenders)    # the tokens themselves
bad_words[1:10]


df_plot_safe <- df %>%
  filter(!str_detect(word, "(?i)\\b(https?://|www\\.)\\S+"))  # remove URLs

set.seed(42)

ggplot(df_plot_safe, aes(label = word, color = as.factor(type), size = freq)) +
  geom_text_wordcloud_area(
    shape        = "circle",
    eccentricity = 1,
    grid_size    = 1,
    grid_margin  = 1,
    rm_outside   = TRUE
  ) +
  scale_size_area(max_size = 30) +
  scale_color_manual(values = c("black", "blue")) +
  theme_void()






