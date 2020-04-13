tdm <- readRDS("tdm.rds")
lsa <- readRDS("lsa.rds")
sigma_inverse <- readRDS("sigma_inverse.rds")
u_transpose <- readRDS("u_transpose.rds")
covid_search <- function(userinput){
  # This is the user input.
  query <- userinput
  query <- tolower(query)
  query <- stemDocument(query)
  query <- removeWords(query, stopwords("english"))
  
  query_split <- unlist(strsplit(query, " "))
  query_split <- stripWhitespace(query_split)
  
  # Query Matrix: count the number of times that
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
  #query_match <- data.frame(document = covid_df$raw_text,
  #                          score = query_scores[1:dim(lsa)[1], # all rows - 1
  #                                               dim(full_lsa)[2]])   # last col.
  #colnames(query_match) <- c("document","score")
  #covid_df$scores <- query_scores[1:dim(lsa)[1]]
  query_scores[1:dim(lsa)[1],dim(full_lsa)[2]]
}



