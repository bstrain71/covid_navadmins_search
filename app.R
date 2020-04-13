library(shiny)
library(tm)
library(RSpectra)
library(lsa)

# Load data
covid_df <- readRDS("covid_df.rds")
#tdm <- readRDS("tdm.rds")
#tdm$dimnames$Terms <- readRDS("tdm_dictionary.rds")
#query_matrix <- readRDS("query_matrix.rds")
#lsa <- readRDS("lsa.rds")
#sigma_inverse <- readRDS("sigma_inverse.rds")
#u_transpose <- readRDS("u_transpose.rds")

# Calls the brute force covid_search(...) function
#source('brute_search_function.R')

# Calls the SVD covid_search(...) function
#source('svd_search_function.R')

# This is the function stored in another R file with
# the required RDS objects that the function needs
# to compute LSA.
source("svd_search_function.R")
  
# Define UI
ui <- fluidPage(
    headerPanel('NAVADMINs Relevant to COVID-19 Search'),
    helpText("This tool searches NAVADMINs, ALNAVs, and ALNAVRESFORs relevant to COVID-19. The order in which results appear is most relevant to least relevant. Click through the tabs to see the messages. If your search does not return any results try a simpler search or check for typos. To see all messages click 'Search' with an empty search box."),
    sidebarPanel(
      textInput("userinput","Custom Search", value = '' ),
      selectInput("number_tabs", "Number of Results", selected = "3",
                  choices = list("1" = 1, "2" = 2, "3" = 3,
                                 "4" = 4, "5" = 5,
                                 "Show All" = dim(covid_df)[1])),
      actionButton("update", "Search")
    ),
    mainPanel(
      uiOutput('mytabs')
    )
  )

# Define server logic
server <- function(input, output) {

  datasetInput <- eventReactive(input$update,{
    covid_df$scores <- covid_search(input$userinput) # runs search function
    
    # Cleans user input for raw finder check
    query <- input$userinput
    query <- tolower(query)
    query <- removeWords(query, stopwords("english"))
    query <- unlist(strsplit(query, " "))
    query <- paste(query, collapse = "|")
    
    # This for loop gives a document a score of "1" if an exact
    # match of the user's query is returned, excluding stopwords.
    # This for loop encompases a brute force search function.
    # I have it set to return brute force match scores as 1
    # so if there is an exact match with a word in the user query
    # minus stopwords then it will be the top result.
    # The user doesn't see the ranking (Just tab order) so I don't
    # think this is a big deal.
    for(i in c(1:dim(covid_df)[1])){
      covid_df$scores[i] <- ifelse(
        grepl(query,
              tolower(covid_df$raw_text[i])),
        1, covid_df$scores[i])
    }
    covid_df <- covid_df[order(-covid_df$scores),] # sorts by score
    covid_df
  }
  )
  
  output$mytabs = renderUI({
    
    # Validates that the user's input is in the corpus.
    validate(
      
      # This checks two conditions:
      # 1) The scores are not all NAN
      # 2) Are there any brute (ctrl+F type) hits?
      # Non-NAN scores and brute hits are returned as the results.
      # IF there are no results the text is shown.
      need(all(is.na(datasetInput()$scores)) == FALSE,
           "No results found. Try a simpler search, check for typos, or use standard Navy phraseology.")
    )
    
    # How many tabs to show? User chooses this.
    nTabs <- input$number_tabs
    
    # Use this area to grab lines(?) sentences(?)
    # that need to be highlighted in the full_text
    # pane.
    # Steps to do that:
    # 1) Break messages into sentences
    # 2) Flag sentences with tag$mark
    # 3) Re-assemble sentences into text
    # 4) renderText(...) to show
    
    do.call(navlistPanel, c(lapply(1:nTabs,function(i) {
      tabPanel(
        # Writes the message number and the subject line
        # on the left hand side as a vertical selectable list.
        title = paste(datasetInput()$title[i], datasetInput()$subj[i]),
        
        # Title and Link to NAVADMIN Library
        tagList(tags$h4("Link to Official Message:")),
        tagList(a(paste(datasetInput()$title[i], datasetInput()$subj[i]),
                  href = datasetInput()$urls[i])
                ),
        
        # Title and auto-generated message summary
        tagList(tags$h4("Auto-Generated Message Summary")),
        tagList(tags$p(datasetInput()$summaries[[i]][1]),
                tags$p(datasetInput()$summaries[[i]][2]),
                tags$p(datasetInput()$summaries[[i]][3]),
                tags$p(datasetInput()$summaries[[i]][4]),
                tags$p(datasetInput()$summaries[[i]][5])),
        
        # Title and full text 
        tagList(tags$h4("Full Text")),
        # Use tags$mark(stuff) to highlight
        # things.
        renderText(datasetInput()$raw_text[i],
                   outputArgs = c(pre))
      )
    })))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

