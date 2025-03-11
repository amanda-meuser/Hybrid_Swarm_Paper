# 1 - Install Packages ----

install.packages("easyPubMed")
library(easyPubMed)

install.packages("remotes")
library(remotes)

remotes::install_github("elizagrames/litsearchr", ref="main")
library(litsearchr)

install.packages("plyr")
library(plyr)

# 2 - Search ----

#Setting search terms as a variable called "search1". "AND", "OR", and "NOT" statements can be used.
search1 <- '((hybrid swarm) AND (biology) NOT(particle))'

#Searching the PubMed database for our search terms.
search1_id <- get_pubmed_ids(search1)

#Shows us how many results our search got.
search1_id$Count

#This line will download the results of the search into text files. By changing the "batch_size", you change the amount of records that will be saved to one text file.
abstracts_txt <- batch_pubmed_download(pubmed_query_string = search1, format = "medline", batch_size = 74)

# 3 - Importing Data ----

#This will import the results of our search into R as a data frame.
PubMed_import <- import_results(file = "easyPubMed_data_01.txt", verbose = TRUE)

#By viewing the data frame we can easily see important information for each paper such as authors, titles, publishing journal, etc.
View(PubMed_import)

# 4 - Key Information ----

#Taking a subset of the data frame to pull out key information. This will become a new data frame. We are interested in selectd the title, journal, authors, publication date, and abstract.
df_keyinfo <- subset(PubMed_import, select = c(title, journal, author, date_published, abstract))

#View this new data frame. 
View(df_keyinfo)

#Write this new data frame to a csv file.
write.csv(df_keyinfo, file = "hybridswarm_papers.csv")

#Counting the frequency of journals gives us information on what journals produce the most work related to our key search terms.
journal_freq <- count(df_keyinfo$journal)

View(journal_freq)

#Helpful documentation 
https://elizagrames.github.io/litsearchr/#
