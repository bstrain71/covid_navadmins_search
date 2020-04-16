highlighter_function <- function(somedataframe, sometext){
  # The tagged_query object is a list of the different
  # words in the query which have been stemmed, stopwords
  # removed, and split into a list.
  tagged_query <- sometext
  #tagged_query <- stemDocument(tagged_query)
  tagged_query <- removeWords(tagged_query, stopwords("english"))
  tagged_query <- unlist(strsplit(tagged_query, " "))
  
  # The query_regex object is the taged_query list
  # but with the regex on either side to grab
  # the entire sentence which contains the tagged_query
  # word.
  # Regex used: "[A-Za-z,\" <>]+(?i)sometext[A-Za-z,\" <>]+"
  query_regex <- rep(NA, length(tagged_query))
  for(i in c(1:length(tagged_query))){
    query_regex[i] <- paste0(
      "[A-Za-z,\" <>]+(?i)",
      tagged_query[i],
      "[A-Za-z,\" <>]+"
    )
  }
  
  # The query_sentence object is the list of matched sentences.
  # It is a LIST OF LISTS so make sure to unlist when you use it.
  query_sentence <- rep(NA, length(tagged_query))
  for(i in c(1:length(query_regex))){
    query_sentence[i] <- str_extract_all(fixed(as.character(somedataframe),
                                               ignore_case = TRUE),
                                         query_regex[i])
  }
  
  query_sentence <- unlist(query_sentence)
  
  # The query_sentence_marked object is the matched sentences
  # with the html <mark> tag for highlighting.
  #query_sentence_marked <- rep(NA, length(query_sentence))
  #for(i in c(1:length(query_sentence))){
  #  somedataframe <- str_replace_all(somedataframe,
  #                                   )
  #}
  for(i in c(1:length(query_sentence))){
    for(j in c(1:length(query_sentence[i]))){
      somedataframe <- ifelse(length(query_sentence) == 0,
                              somedataframe,
                              str_replace_all(somedataframe,
                                              query_sentence[[i]][j],
                                              paste0(
                                                "<mark>",
                                                query_sentence[[i]][j],
                                                "</mark>"
                                              )))
    }
  }
  somedataframe
}
