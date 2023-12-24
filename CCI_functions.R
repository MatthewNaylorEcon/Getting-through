#############################################################################################################.
########### Created by: Matthew Naylor 23/12/23 
########### Objectives: Contains functions needed to produce CCI measure.  
#############################################################################################################.

library(rvest)
library(stringr)
library(xlsx)
library(readtext)
library(tidyverse)
library(dplyr)
library(quanteda)
library(nsyllable)
library(tm)
library(corpus) #maybe this is easier than quanteda
library(readxl)
library(xtable)
library(DT)
library(tm)
library(tidyverse)
library(tidyr)
library(zoo)


##################################################################################.
########################## Step 1: load the text data ############################
##################################################################################.

# [Instructions:
# To edit for your text data and your working directory.
# Text data has to be in a dataframe with 2 columns: "Quarter" and "text".
# The "Quarter" column must be class 'yearqtr'. The "text" column must be 'character' class.]
# setwd("/Users/matthewnaylor/Ling_Comp_CB_Comms/CCI Function")
# load("MPS_text_15Q3_23Q2.Rda")
# docs = MPS_text_15Q3_23Q2

##################################################################################.
########################## Step 2: load the dictionary dfs #######################
##################################################################################.

# load dictionary of jargon terms. This will 13 dataframes and vectors:
# 'dictionary_jargon_phrases.Rda' and 'dictionary_jargon_all_tokens' are used in Step 4 for identifying jargon phrases and tokens.
# 'dictionary_with_topics.Rda' is a 'readable' version of the full dictionary and the topic categorisations.
# 'topic_X' contains the 'tokenised' jargon terms for each topic, respectively, used in Step 5. 
setwd("/Users/matthewnaylor/Ling_Comp_CB_Comms/CCI Function")
load("jargon_dictionary.Rda")

##################################################################################.
########################## Step 3: clean the text data ###########################
##################################################################################.

#### First we need to a some cleaning for consistency with the dictionary, and then conversion into tokens to identify the jargon words + phrases

#### (a) Start with some basic cleaning of the 'raw' text  

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
    tokens(remove_numbers = TRUE, remove_punct = TRUE) %>% #convert to tokens
    tokens_wordstem(language = quanteda_options("language_stemmer")) #now we can stem the words using Porter Stemmer method -> so that 'policy' and 'policies' are not counted as separate word for e.g.
}

docs_tokens = tokenise(docs$text)

# docs$text = docs$text %>%
#   str_replace_all("(('|’) )", " ") %>%
#   str_replace_all("(-)", " ") %>%
#   str_replace_all("(('|’)s)", "") %>%
#   str_replace_all("(('|’)re)", "re") %>%
#   str_replace_all("(('|’)ve)", "ve")
# 
# #### (b) Convert the 'raw' text into tokens, convert to lower case, and 'stem' the terms. This is needed to then identify jargon words/phrases.
# docs_tokens = as.character(lapply(docs$text, tolower))%>% #convert to lower case
#   tokens(remove_numbers = TRUE, remove_punct = TRUE) %>% #convert to tokens
#   tokens_wordstem(language = quanteda_options("language_stemmer")) #now we can stem the words using Porter Stemmer method -> so that 'policy' and 'policies' are not counted as separate word for e.g.
# 
# tokenise = function(x){
#   as.character(lapply(x, tolower))%>% #convert to lower case
#     tokens(remove_numbers = TRUE, remove_punct = TRUE) %>% #convert to tokens
    # tokens_wordstem(language = quanteda_options("language_stemmer"))} #now we can stem the words using Porter Stemmer method -> so that 'policy' and 'policies' are not counted as separate word for e.g.


##################################################################################.
##################### Step 4: identify jargon terms in the text ##################
##################################################################################.

#### (a) We identify jargon terms in the text by looking for matches with terms in the jargon dictionary

identify_jargon = function(docs_tokens){
  # We start by identifying the jargon PHRASES, using the dict_phrases_raw_stem dictionary. We identify the consecutive words within the text that match dict_phrases_raw_stem (indicating they are jargon when used together). We then 'compound' those phrases that match (turning 'monetary policy' into 'monetary_policy') 
  docs_tokens_compounded = tokens_compound(docs_tokens, pattern = phrase(dictionary_jargon_phrases))
  # Having 'compounded' the jargon phrases, we can now select all jargon terms/phrases in our texts. We do this by finding matches (phrases and individual terms) within the text that match with 'dictionary_tokens'.
  docs_jargon <- tokens_select(docs_tokens_compounded, dictionary_jargon_all_tokens, selection = "keep", valuetype = "fixed", case_insensitive = TRUE, window = 0)
}

docs_jargon = identify_jargon(docs_tokens)

#### (b) Add these to the original df 'docs'

add_jargon_to_df = function(docs_jargon){
  docs_jargon_df = as.data.frame(t(as.data.frame(lapply(docs_jargon, paste, collapse=" ")))) %>% # first we de-tokenise
    cbind(docs) %>% #bind with columns in original 'docs' df
    dplyr::mutate(jargon = V1) %>% #rename the jargon column which comes out as 'V1'
    dplyr::select(Quarter, text, jargon) #re-order
}

docs_jargon_df = add_jargon_to_df(docs_jargon)

###############################################################################################.
##################### Step 5: Assign each jargon term used to a topic. ########################
###############################################################################################.

# here we identify the number of different topics discussed in each document + total number of jargon terms in each topic for each doc
Topics = vector()
for(d in 1:length(docs_jargon_df$Quarter)){
  topics = vector()
  for(j in 1:length(str_split(docs_jargon_df$jargon[d], " ")[[1]])){
    if(str_split(docs_jargon_df$jargon[d], " ")[[1]][j] %in% topic_mp$dictionary_stemmed){
      topics[j] = "A"
    }
    if(str_split(docs_jargon_df$jargon[d], " ")[[1]][j] %in% topic_infl$dictionary_stemmed){
      topics[j] = "B"
    }
    if(str_split(docs_jargon_df$jargon[d], " ")[[1]][j] %in% topic_gdp$dictionary_stemmed){
      topics[j] = "C"
    }
    if(str_split(docs_jargon_df$jargon[d], " ")[[1]][j] %in% topic_demand$dictionary_stemmed){
      topics[j] = "D"
    }
    if(str_split(docs_jargon_df$jargon[d], " ")[[1]][j] %in% topic_fiscal$dictionary_stemmed){
      topics[j] = "E"
    }
    if(str_split(docs_jargon_df$jargon[d], " ")[[1]][j] %in% topic_intl$dictionary_stemmed){
      topics[j] = "F"
    }
    if(str_split(docs_jargon_df$jargon[d], " ")[[1]][j] %in% topic_labour$dictionary_stemmed){
      topics[j] = "G"
    }
    if(str_split(docs_jargon_df$jargon[d], " ")[[1]][j] %in% topic_finmarkets$dictionary_stemmed){
      topics[j] = "H"
    }
    if(str_split(docs_jargon_df$jargon[d], " ")[[1]][j] %in% topic_finstab$dictionary_stemmed){
      topics[j] = "I"
    }
    if(str_split(docs_jargon_df$jargon[d], " ")[[1]][j] %in% topic_other$dictionary_stemmed){
      topics[j] = "J"
    }
  }
  Topics[d] = str_c(topics, collapse=" ")
} 

#construct augmented df with this additional column
docs_jargon_topics_df = docs_jargon_df %>%
  cbind(Topics) %>%
  dplyr::select(c(Quarter:jargon, Topics))

###############################################################################################.
############### Step 6: Create loop to construct the variables that underpin the CCI ##########
###############################################################################################.

### (a) Loop to construct inputs to the CC Index for each topic for each document

jargon_terms_by_topic = vector()
# number_of_jargon_terms_by_topic = vector()
number_of_jargon_terms_by_topic_mod = vector()
unique_jargon_terms_by_topic = vector()
number_of_unique_jargon_terms_by_topic = vector()
count_of_each_unique_jargon_term_by_topic = vector()
for(d in 1:length(docs_jargon_topics_df$Quarter)){
  topics = list()
  #identify the location of the words that are in each of these topics
  topics[[1]] = grep("A", str_split(docs_jargon_topics_df$Topics[d], " ")[[1]])
  topics[[2]] = grep("B", str_split(docs_jargon_topics_df$Topics[d], " ")[[1]])
  topics[[3]] = grep("C", str_split(docs_jargon_topics_df$Topics[d], " ")[[1]])
  topics[[4]] = grep("D", str_split(docs_jargon_topics_df$Topics[d], " ")[[1]])
  topics[[5]] = grep("E", str_split(docs_jargon_topics_df$Topics[d], " ")[[1]])
  topics[[6]] = grep("F", str_split(docs_jargon_topics_df$Topics[d], " ")[[1]])
  topics[[7]] = grep("G", str_split(docs_jargon_topics_df$Topics[d], " ")[[1]])
  topics[[8]] = grep("H", str_split(docs_jargon_topics_df$Topics[d], " ")[[1]])
  topics[[9]] = grep("I", str_split(docs_jargon_topics_df$Topics[d], " ")[[1]])
  topics[[10]] = grep("J", str_split(docs_jargon_topics_df$Topics[d], " ")[[1]])
  #create loop to construct inputs to the CC Index for each topic
  jargon = list()
  jargon_vector = vector()
  jargon_vector_mod = vector()
  length_jargon = list()
  length_jargon_mod = list()
  unique_jargon = list()
  unique_jargon_vector = vector()
  length_unique_jargon = list()
  unique_jargon_count_collapsed = vector()
  for(t in 1:length(topics)){
    #get the jargon words in these places, for each topic
    jargon[[t]] = str_split(docs_jargon_topics_df$jargon[d], " ")[[1]][topics[[t]]] #first as list
    jargon_vector[t] = str_c(jargon[[t]], collapse=" ") #then collapse each topic into single vector string
    
    #how many jargon terms are in each topic? 
    # length_jargon[[t]] = length(jargon[[t]]) #here we treat 'interest rate' as a single jargon term. (consistent with what we do below in saying this is a single 'term' in a topic)
    length_jargon_mod[[t]] = length(unlist(str_split(str_replace_all(jargon[[t]], "_", " "), " "))) #here we construct an alterantive vector, which treats 'interest rate' as two separate words, for purposes of calculating PoJ. We will then later still treat 'interest rate' as a single term for purposes of distinguishing the topics.
    
    #then identify the unique terms/phrases in each topic. Here we use the non-mod version (i.e. jargon) such that 'interest rates' counts as one unique term rather than 'interest' and 'rates' counting as two. 
    unique_jargon[[t]] = unique(jargon[[t]]) #first as list
    unique_jargon_vector[t] = str_c(unique_jargon[[t]], collapse=" ") #then collapsed into unique single vector string for each topic
    
    #how many unique words are there? (again for unique words, we only use the non-mod version)
    length_unique_jargon[[t]] = length(unique_jargon[[t]])
    
    #how many of each unique jargon term is there in each topic? i.e. what's the count for each (unique) jargon term in each topic?
    unique_jargon_count = list()
    if(length(unique_jargon[[t]])>0){  #i.e. if there is at least one jargon term in that topic
      for(j in 1:length(unique_jargon[[t]])){
        locate_unique_jargon = grep(unique_jargon[[t]][j], jargon[[t]])
        unique_jargon_count[j] = length(locate_unique_jargon)
        unique_jargon_count_collapsed[t] = str_c(as.character(unique_jargon_count), collapse=" ")
      }
    }else{
      unique_jargon_count_collapsed[t] = "0" 
    }
  }
  # now add the variables i am interested in: unlist(length_jargon), toString(jargon_vector), unlist(length_unique_jargon), toString(unique_jargon_vector), toString(unique_jargon_count_collapsed)
  jargon_terms_by_topic[d] = toString(jargon_vector)
  # number_of_jargon_terms_by_topic[d] = toString(unlist(length_jargon))
  number_of_jargon_terms_by_topic_mod[d] = toString(unlist(length_jargon_mod))
  unique_jargon_terms_by_topic[d] = toString(unique_jargon_vector)
  number_of_unique_jargon_terms_by_topic[d] = toString(unlist(length_unique_jargon))
  count_of_each_unique_jargon_term_by_topic[d] = toString(unique_jargon_count_collapsed)
} 

## (b) update the df with these variables 

CCI_df = docs_jargon_topics_df %>%
  # First, add the variables constructed in the loop above to the main dataframe
  cbind(jargon_terms_by_topic,
        # number_of_jargon_terms_by_topic,
        number_of_jargon_terms_by_topic_mod,
        unique_jargon_terms_by_topic,
        number_of_unique_jargon_terms_by_topic,
        count_of_each_unique_jargon_term_by_topic) %>%
  # Now we can start to compute variables that feed into the CCI Index 
  dplyr::group_by(Quarter) %>%
  dplyr::mutate(# Wj = sum(as.numeric(unlist(str_split(number_of_jargon_terms_by_topic, ",")))),
    Wj_mod = sum(as.numeric(unlist(str_split(number_of_jargon_terms_by_topic_mod, ",")))),
    Wi = ntoken(tokens(text)),
    # PoJ = Wj/Wi,
    PoJ = Wj_mod/Wi, #consistent with PJ 
    wjt = count_of_each_unique_jargon_term_by_topic,
    # Wjt = number_of_jargon_terms_by_topic,
    Wjt = number_of_jargon_terms_by_topic_mod)

###############################################################################################.
############# Step 7: Create loop to construct the input variables and calculate CC Index #####
###############################################################################################.

#Now construct the headline sjt, omega, Wjt_star_t, Wjt_star, Phi_d, and, finally, CCI variables in a loop. 
omega = vector()
Omega = vector()
Wjt_star = vector()
sum_Wjt_star = vector()
Phi_d = vector()
CCI = vector()
for(d in 1:length(CCI_df$Quarter)){
  #concentration per topic 
  omega_t = vector()
  Omega_t = vector()
  Wjt_star_t = vector() 
  for(t in 1:length(topics)){
    within_topic_count_char = str_replace_all(unlist(str_split(CCI_df$wjt[d], ","))[t], "^ ", "") #separate the topic counts, currently in a list, by commas, then unlist (and get rid of additional spaces that emerge)
    within_topic_count_numeric = as.numeric(unlist(str_split(within_topic_count_char, " "))) #convert to numeric
    sjt = ifelse(sum(within_topic_count_numeric)>0, within_topic_count_numeric/sum(within_topic_count_numeric), 0) #essentially wjt/Wjt
    sjt2 = sjt^2
    sum_sjt2 = sum(sjt^2)
    omega_t[t] = sqrt(sum_sjt2) #concentration for that topic
    Omega_t[t] = ifelse(omega_t[t]>0, 2^(log(omega_t[t], base=10)),0) #this transformation ensures that the complexity doubles for every x10 reduction in concentration. i.e. (omega = 1, Omega = 1), (omega = 0.1, Omega =0.5), (omega = 0.01, Omega = 0.25) 
    Wjt_star_t[t] = ifelse(Omega_t[t]>0, as.numeric(unlist(str_split(CCI_df$Wjt[d], ","))[t])/Omega_t[t], 0) #essentially Wjt_mod/omega_t
  }
  omega[d] = toString(omega_t)
  Omega[d] = toString(Omega_t)
  Wjt_star[d] = toString(Wjt_star_t) #turn back into single string list to fit into a cell
  
  ## adjustment for the number of topics covered
  #want to look for how many values are > 0, that gives us the N of topics
  Wjt_star_char = str_replace_all(unlist(str_split(Wjt_star[d], ",")), "^ ", "") #convert from list to char vector
  Wjt_star_numeric = as.numeric(unlist(str_split(Wjt_star_char, " "))) #convert to numeric
  #now check which are > 0: signifying this topic is covered
  n_topics_d = length(Wjt_star_numeric[which(Wjt_star_numeric>0)])
  Phi_d[d] = log(10 + 90, base=10)/(log(10 + 90, base=10) - log(n_topics_d, base=10))
  
  #Now we can compute the CC index
  Wjt_star_char = str_replace_all(unlist(str_split(Wjt_star[d], ",")), "^ ", "") #convert from list to char vector
  Wjt_star_numeric = as.numeric(unlist(str_split(Wjt_star_char, " "))) #into numeric vector
  sum_Wjt_star[d] = sum(Wjt_star_numeric)
  CCI[d] = (sum(Wjt_star_numeric)*Phi_d[d])/CCI_df$Wi[d]
}

CCI_df = CCI_df %>%
  cbind(data.frame(omega), data.frame(Omega), data.frame(Wjt_star), data.frame(sum_Wjt_star), data.frame(Phi_d), data.frame(CCI))





