#############################################################################################################.
########### Created by: Matthew Naylor 23/12/23 
########### Objectives: Compute the CC Index.
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

setwd("/Users/matthewnaylor/Ling_Comp_CB_Comms/CCI Function")
load("MPS_text_15Q3_23Q2.Rda")
docs = MPS_text_15Q3_23Q2

##################################################################################.
########################## Step 2: Run the source code ###########################
##################################################################################.

# Run source code
source("CCI_functions.R")

# Remove all vectors/dataframes/functions but the ones below
rm(list=setdiff(ls(), c("docs", "MPS_text_15Q3_23Q2", "CCI_df")))

##################################################################################.
########################## Step 3: Enjoy the CCI :) ## ###########################
##################################################################################.

CCI_df$CCI

