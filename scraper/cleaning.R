#Loading the rvest package
library('rvest')
library('stringi')

# Proof of concept section

#Specifying the url for desired website to be scraped
url <- 'https://www.public.navy.mil/bupers-npc/reference/messages/Documents/NAVADMINS/NAV2020/NAV20089.txt'

#Reading the HTML code from the website
webpage <- read_html(url)

# Turn the HTML code into text (it is .txt. file so just
# get everything - no menus, etc to worry about)
rank_data <- html_text(webpage)

# Make a list of all NAVADMINs that are related to coronavirus
covid_navadmins <- c("089","088","083","082","080","075","074","073",
                    "072","071","070","069","068","065","064")

# Make an empty dataframe to put stuff into
covid_df <- data.frame(matrix(NA,
                              nrow = length(covid_navadmins),
                              ncol = 3))
colnames(covid_df) <- c("url","text","navadmin_no")

covid_df$navadmin_no <- paste("NAVADMIN ",
                              covid_navadmins,
                              "/20",
                              sep = "")

# Paste the numbers into the bupers URL
covid_df$url = paste(url <- 'https://www.public.navy.mil/bupers-npc/reference/messages/Documents/NAVADMINS/NAV2020/NAV20',
                     covid_navadmins,
                     '.txt',
                     sep = "")

# For loop to get the raw text using the URLs.
for (i in c(1:length(covid_df[,1]))){
  covid_df$text[i] <- html_text(read_html(covid_df$url[i]))
}

# This puts each line of each navadmin into a nested list
# in the adjacent column
# Call individual lines like: covid_df$lines[[1]][1]
covid_df$lines <- stri_split_lines(covid_df$text)


# Save the clean dataframe to be loaded
# into the shiny app.
saveRDS(covid_df, file = "covid_df.rds")

covid_df <- readRDS(file = "covid_df.rds")

#unlist(strsplit(query, " "))


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
  
  #hit_text
  temp_df$hit_text <- hit_text
  
  # Search for matches of hit text in the full text and replace with HTML tags
  temp_df
}

mttest <- covid_search("turkey")
smalltest <- covid_search("bUrke")
test <- covid_search("admin")


#covid_search("wash corona") # TWO WORD SEARCHES!!!!


### Only use the below for demoing and troubleshooting
# The indexing used to run the above function. This
# code won't do anything except for help you to
# understand what the function is doing.
###
# Create a clean list of lines against which users can
# This works to find admin and returns indices in the
# list of list that is $lines[[1]]
#test2 <- grep("admin", covid_df$lines[[1]], ignore.case = TRUE)
#grep(test_input, covid_df$lines[[1]], ignore.case = TRUE)
#grep("burke", covid_df$lines[[1]], ignore.case = TRUE)
#noresults <- grep("penis", covid_df$lines[[1]], ignore.case = TRUE)
#test_input <- "burke"

