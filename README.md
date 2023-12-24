This folder computes the CCI index developed in 'Getting through: communicating complex information' Bank of England Staff Working Paper 1,047 and CEPR Discussion Paper 18,357. 

Instructions: 

Save all files / scripts / dataframes in this repository in your own local folder. 

Use CCI_compute.R to compute the CCI. 

This will require installation of the packages specified in that script. 

In that script, Step 1 downloads the text data. In the master file, the text dataframe is "MPS_text_15Q3_23Q2.Rda", which is the text from the Bank of England Monetary Policy Summary from 2015 Q3 to 2023 Q2, as used in the Working Paper. The user will need to replace this text dataframe with their own, following the instructions in the script.

Then the file will run the source code from CCI_functions, and spit out a dataframe with the jargon, some other helpful information values, and, crucially, the CCI. 
