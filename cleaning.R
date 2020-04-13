#Loading the rvest package
#library(rvest)
#library(stringi)
library(tm)
library(RSpectra) # svds
#library(rtika)
library(lsa) # cosine
#library(readr)
#library(stringr)

covid_df <- readRDS(file = "covid_df.rds")

# Create a corpus.
vectorsource <- VectorSource(covid_df$raw_text)
corpus <- VCorpus(vectorsource)

# Do some cleaning.
cleaner <- content_transformer(function (x , pattern ) iconv(x,
                                                             to = "ASCII//TRANSLIT",
                                                             sub = ""))
corpus <- tm_map(corpus, cleaner)
corpus <- tm_map(corpus, stemDocument)
stopword_remover <- content_transformer(function (x , pattern ) removeWords(x, stopwords("english")))
corpus <- tm_map(corpus, stopword_remover)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)

# Make that corpus into a term document matrix.
# All of these options remove punctuation,
# stopwords, whitespace, and stem the words.
tdm <- TermDocumentMatrix(corpus, 
                                control = 
                                  list(tolower = TRUE,
                                       bounds = list(global = c(1, Inf))))

tdm <- weightTfIdf(tdm, normalize = TRUE)

# Make the term document matrix into a sparse matrix for SVD.
tdm_matrix <- as.matrix(tdm)

############################################## K
# Find the largest k singular values.
svd <- svds(tdm_matrix, k = 5)
############################################## K
# The svd object contains several matrices.
# The matrix v contains the values that we want
# to use for a search engine.
# Specifically, v contains a k by kv matrix whose columns
# contain the right singular vectors.
lsa <- svd$v

# Transpose the u matrix
u_transpose <- t(svd$u)

# Take the inverse of the d matrix / sigma values
sigma_inverse <- 1 / svd$d


### Creating user query

# This is the user input.
query <- "burke"
query <- tolower(query)
#query <- stemDocument(query)
query <- removeWords(query, stopwords("english"))

query_split <- unlist(strsplit(query, " "))
query_split <- stripWhitespace(query_split)

# Query Matrix experiment: just count the number of times that
# the word appears - put the counts in the tdm$dimnames$Terms
# vector.
query_matrix <- rep(0, length(tdm$dimnames$Terms))
for(i in 1:length(query_split)){
  query_matrix[which(tdm$dimnames$Terms == query_split[i])] <- 1
}
query_matrix <- as.matrix(query_matrix)



# Put the query into the topic model space.
query_lsa <- sigma_inverse * u_transpose %*% query_matrix

# Calculate the cosine similarity between the query and each document.
full_lsa <- cbind(t(lsa), query_lsa)
query_scores <- cosine(full_lsa)

# Create a results dataframe
# The score pulls the cosine similarities from the query_score
# matrix. The last column represents the similarities with the
# user query. The last row last column == 1 because it is the
# user query's similarith with itself. The indexing done here
# in the score column pull the user similarity scores and
# removes the (1) value.
query_match <- data.frame(document = covid_df$raw_text,
                          score = query_scores[1:dim(lsa)[1], # all rows - 1
                                               dim(full_lsa)[2]])   # last col.
colnames(query_match) <- c("document","score")

# Save the things needed for the shiny app:
#saveRDS(files, file = "filenames.rds")
#saveRDS(tdm$dimnames$Terms, file = "tdm_dictionary.rds")
saveRDS(tdm, file = "tdm.rds")
saveRDS(lsa, file = "lsa.rds")
saveRDS(sigma_inverse, file = "sigma_inverse.rds")
saveRDS(u_transpose, file = "u_transpose.rds")

top5 <- head(query_match[order(query_match$score, decreasing=TRUE),], 3)
View(top5)
#tdm$dimnames$Terms[1:3]
#length(tdm$dimnames$Terms)

query




