# Brute Search Function
covid_search <- function(userinput){
  
  # Variable
  number_of_documents <- dim(covid_df)[1]
  
  # temporary dataframe to hold and use results
  temp_df <- data.frame(matrix(NA,
                               nrow = number_of_documents,
                               ncol = 4))
  colnames(temp_df) <- c("navadmin_no","url","hit_lines","full_text")
  
  # for loop to check for results in each line
  # of each document in the corpus covid_df
  for (i in c(1:number_of_documents)){
    temp_df$navadmin_no[i] <- covid_df$navadmin_no[i]
    temp_df$url[i] <- covid_df$url[i]
    temp_df$full_text[i] <- covid_df$text[i]
    
    # if grep doesn't get anything it returns
    # a list of length 0 so use an ifelse to write
    # NA for the documents that don't contain the user
    # input
    
    # temporary list of hits (if any)
    temporary_grep <- grep(userinput,
                           covid_df$lines[[i]],
                           ignore.case = TRUE)
    
    # if else statement to get results if they exist
    # or return NA if there are no hits
    temp_df$hit_lines[i] <- ifelse(
      length(temporary_grep) == 0,
      NA,
      list(temporary_grep)
    )
  }
  
  # This grabs only results which have a match
  # with the user input.
  temp_df <- temp_df[
    which(is.na(temp_df$hit_lines) == FALSE),]
  
  #rownames(temp_df)
  
  # Make an empty vector to hold the actual text of lines with hit
  hit_text <- c(rep(NA, times = dim(temp_df)[1]))
  
  # Loop over the documents with hits and write a list of lines
  # with hits to hit_text
  
  for(i in c(1:dim(temp_df)[1])){
    hit_text[i] <- 
      list(covid_df$lines[[
        as.numeric(rownames(temp_df)[i]) # index of docs w/ hits
        ]][
          #unlist(temp_df$hit_lines)[i]
          as.numeric(unlist(temp_df$hit_lines[i]))
          ]
      )
    # index of lines within that doc that are hits
  }
  
  # color the lines that have hits
  
  #hit_text
  temp_df$hit_text <- hit_text
  temp_df
}