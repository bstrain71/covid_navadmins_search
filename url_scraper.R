#Loading the rvest package
library(rvest)
library(stringi)
#library(tm)
#library(RSpectra)
#library(rtika)
#library(lsa)
#library(readr)
library(stringr)
library(lexRankr)

# URLS for all relevant messages - these are are all RAW txt when
# viewed in browser. Don't link to anything that is not raw txt
urls <- c("https://www.public.navy.mil/bupers-npc/reference/messages/Documents/NAVADMINS/NAV2020/NAV20104.txt",
          "https://www.public.navy.mil/bupers-npc/reference/messages/Documents/NAVADMINS/NAV2020/NAV20101.txt",
          "https://www.public.navy.mil/bupers-npc/reference/messages/Documents/NAVADMINS/NAV2020/NAV20102.txt",
          "https://www.public.navy.mil/bupers-npc/reference/messages/Documents/NAVADMINS/NAV2020/NAV20100.txt",
          "https://www.public.navy.mil/bupers-npc/reference/messages/Documents/NAVADMINS/NAV2020/NAV20099.txt",
          "https://www.public.navy.mil/bupers-npc/reference/messages/Documents/NAVADMINS/NAV2020/NAV20098.txt",
          "https://www.public.navy.mil/bupers-npc/reference/messages/Documents/NAVADMINS/NAV2020/NAV20097.txt",
          "https://www.public.navy.mil/bupers-npc/reference/messages/Documents/NAVADMINS/NAV2020/nav20093.txt",
          "https://www.public.navy.mil/bupers-npc/reference/messages/Documents/NAVADMINS/NAV2020/NAV20089.txt",
          "https://www.public.navy.mil/bupers-npc/reference/messages/Documents/NAVADMINS/NAV2020/NAV20088.txt",
          "https://www.public.navy.mil/bupers-npc/reference/messages/Documents/NAVADMINS/NAV2020/NAV20083.txt",
          "https://www.public.navy.mil/bupers-npc/reference/messages/Documents/NAVADMINS/NAV2020/NAV20082.txt",
          "https://www.public.navy.mil/bupers-npc/reference/messages/Documents/NAVADMINS/NAV2020/NAV20080.txt",
          "https://www.public.navy.mil/bupers-npc/reference/messages/Documents/NAVADMINS/NAV2020/NAV20075.txt",
          "https://www.public.navy.mil/bupers-npc/reference/messages/Documents/NAVADMINS/NAV2020/NAV20074.txt",
          "https://www.public.navy.mil/bupers-npc/reference/messages/Documents/NAVADMINS/NAV2020/NAV20073.txt",
          "https://www.public.navy.mil/bupers-npc/reference/messages/Documents/NAVADMINS/NAV2020/NAV20072.txt",
          "https://www.public.navy.mil/bupers-npc/reference/messages/Documents/NAVADMINS/NAV2020/NAV20071.txt",
          "https://www.public.navy.mil/bupers-npc/reference/messages/Documents/NAVADMINS/NAV2020/NAV20069.txt",
          "https://www.public.navy.mil/bupers-npc/reference/messages/Documents/NAVADMINS/NAV2020/NAV20068.txt",
          "https://www.public.navy.mil/bupers-npc/reference/messages/Documents/NAVADMINS/NAV2020/NAV20065.txt",
          "https://www.public.navy.mil/bupers-npc/reference/messages/Documents/NAVADMINS/NAV2020/NAV20064.txt",
          "https://www.public.navy.mil/bupers-npc/reference/messages/Documents/ALNAVS/ALN2020/ALN20037.txt",
          "https://www.public.navy.mil/bupers-npc/reference/messages/Documents/ALNAVS/ALN2020/ALN20035.txt",
          "https://www.public.navy.mil/bupers-npc/reference/messages/Documents/ALNAVS/ALN2020/ALN20029.txt",
          "https://www.public.navy.mil/bupers-npc/reference/messages/Documents/ALNAVS/ALN2020/ALN20028.txt",
          "https://www.public.navy.mil/bupers-npc/reference/messages/Documents/ALNAVS/ALN2020/ALN20026.txt",
          "https://www.public.navy.mil/bupers-npc/reference/messages/Documents/ALNAVS/ALN2020/ALN20025.txt",
          "https://www.public.navy.mil/nrh/ALNAVRESFOR/ALNAVRESFOR%202020/2020%20ALNAVRESFOR%20CNRF/2020%20ALNAVRESFOR%20009%20CNRF-NAVY%20RESERVE%20ENHANCED%20TELECOMMUTING%20PROCEDURES.txt",
          "https://www.public.navy.mil/nrh/ALNAVRESFOR/ALNAVRESFOR%202020/2020%20ALNAVRESFOR%20CNRF/2020%20ALNAVRESFOR%20008%20CNRF-FY20%20NAVY%20RESERVE%20MITIGATION%20MEASURES%20IN%20RESPONSE%20TO%20CORONAVIRUS.txt")


# Make an empty dataframe to put stuff into
covid_df <- data.frame(matrix(NA,
                              nrow = length(urls),
                              ncol = 5))
colnames(covid_df) <- c("urls","title","lines","cleaned_text","raw_text")

covid_df$urls <- urls

# For loop to get the raw text using the URLs.
for (i in c(1:length(covid_df$urls))){
  covid_df$raw_text[i] <- html_text(read_html(covid_df$urls[i]))
}

# This puts each line of each message into a nested list
# in the adjacent column
# Call individual lines like: covid_df$lines[[1]][1]
covid_df$lines <- stri_split_lines(covid_df$raw_text)

###
test <- stri_split_boundaries(covid_df$cleaned_text[1],
                              type = "sentence")
# rubberducky:
# take the words that are closest to the user query in tdm matrix
# anmd if those words are %in the raw_text then flag them with
# tags$mark. varsity: flag the sentence or several sentences around
# those words with tags$mark!!


###
#test <- str_extract(covid_df$raw_text[22], "(.*?)/20")

# Grab the NAVADMIN / ALNAV / WHATEVER number
for(i in c(1:length(covid_df$urls))){
  covid_df$title[i] <- str_extract(covid_df$raw_text[i], "(.*?)/20")
}


# Grab subject line
#test <- str_extract(covid_df$raw_text[1], "SUBJ.*")
for(i in c(1:length(covid_df$urls))){
  temp <- str_extract(covid_df$raw_text[i], "SUBJ.*")
  temp <- gsub("SUBJ", "", temp)
  temp <- gsub("/", "", temp)
  covid_df$subj[i] <- temp
}


# Strip newline and whitespace markers and replace with one space
covid_df$cleaned_text <- gsub("\\s", " ", covid_df$raw_text)

# Find double spaces and replace them with one space.
covid_df$cleaned_text <- gsub("  ", " ", covid_df$cleaned_text)

# Make a new column that contains summaries of the messges.
#covid_lexranks <- rep(NA, dim(covid_df)[1])
#test <- lexRank(covid_df$raw_text[1],
#                docId = "create",
#                n = 5,
#                sentencesAsDocs = TRUE,
#                returnTies = FALSE,
#                Verbose = FALSE)

covid_df$summaries <- rep(NA, dim(covid_df)[1])

for(i in c(1:length(covid_df$summaries))){
  temp <- lexRank(covid_df$cleaned_text[i],
                               docId = "create",
                               n = 5,
                               sentencesAsDocs = TRUE,
                               returnTies = FALSE,
                               Verbose = FALSE)
  covid_df$summaries[i] <- list(temp$sentence)
}

#test <- lexRank(covid_df$raw_text[27],
#                docId = "create",
#                n = 5, 
#                sentencesAsDocs = TRUE,
#                returnTies = FALSE,
#                Verbose = FALSE)
  

# Save the clean dataframe to be loaded
# into the shiny app.
saveRDS(covid_df, file = "covid_df.rds")


#covid_df <- readRDS(file = "covid_df.rds")
