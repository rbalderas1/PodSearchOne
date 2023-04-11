# load libraries
library(shiny)
library(shinyjs)
library(shinydashboard)
library(fresh)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(DT)
library(rsconnect)
library(stringr)

podsearch_df <- read_csv("podsearch_df_complete_04_08_2023_v1.csv")

word(podsearch_df[1, "categories"], 1)

# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$head(tags$style(HTML('* {
                            font-family: "Space Mono", monospace;
                            color: #291440;
                            background-color: #F2EDF9;
                            }
                            .shiny-input-container {
                            color: #291440;
                            }
                            .js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {
                            background: #A64EFF;
                            }
                            h1 {
                            background-color: #A64EFF;
                            color: #F2EDF9;
                            padding: 15px
                            }
                            h3 {
                            font-weight: bold;
                            text-align: justify;
                            }
                            h5 {
                            text-align: left;
                            font-size: 1.15em;
                            }
                            #pod-link {
                            text-align: center;
                            }
                            .btn, button {
                            display: block;
                            margin: 20px auto;
                            height: 50px;
                            width: 100px;
                            border-radius: 50%;
                            border: 2px solid #A64EFF;
                            font-weight: bold;
                            }
                            img {
                            width: 100%;
                            height: auto;
                            padding-bottom: 15px;
                            padding-top: 15px;
                            border-radius: 10%;
                            }
                            '))),
  titlePanel(h1("PodSearch")),
  fluidRow(
    column(2, 
           # Added slider input alternative
           sliderInput("number_episodes_slider",
                       "Select range of episodses:",
                       min = 1, max = 800,
                       value = 1),
           selectInput("explicit",
                       "Explicit:",
                       c("None",
                         c("explicit", "not explicit"))),
           selectInput("genre",
                       "Genre/Category:",
                       c("None",
                         c("art", "business", "christianity", "comedy", "education", "fiction", "health", "history", "kids", "leisure", "music", "news", "religion", "science", "society", "spirituality", "sports", "technology", "tv"))),
           selectInput("zodiac", #Will probably remove this selectinput
                       "Zodiac:",
                       c("None",
                         c("Aries", "Taurus", "Gemini", "Cancer", "Virgo", "Leo", "Libra", "Scorpio", "Sagittarius", "Capricorn", "Aquarius", "Pisces")))
           ),
    mainPanel(column(12, 
                     (tabsetPanel(type="tabs",
                                  tabPanel("Dating", 
                                           box(
                                             htmlOutput("filtered_podcast"),
                                             
                                           )
                                  ), # end of dating tab
                                  
                                  
                                  tabPanel("Table", 
                                           box(h2(""),
                                               h3("All eligible podcasts!"),
                                               h2(""),
                                               DT::dataTableOutput("table"))
                                           
                                  )
                                  
                     ))))))

# Define server logic required to draw a histogram
server <- function(input, output) {

  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    data <- podsearch_df
    if (input$explicit != "None") {
      data <- data[data$explicit == input$explicit,]
    }
    if (input$zodiac != "None") {
      data <- data[data$zodiac == input$zodiac,]
    }
    if (input$genre != "None") {
      data <- data %>% 
        filter(grepl(input$genre, categories))
    }
    data[c("title", "description", "number_episodes", "categories", "explicit", "zodiac")]
    
  }))
  
  output$filtered_podcast <- renderText({
    
    if(input$explicit == "None" | input$number_episodes_slider == 1 | input$genre == "None") {
      paste(h2("Use filters to get your match!"))
    } else{
      
      filtered_df <- podsearch_df %>% 
        filter(number_episodes <= input$number_episodes_slider & explicit %in% input$explicit & grepl(input$genre, categories))
      
      pod_match <- sample_n(filtered_df, 1)
      observeEvent(input$shuffleButton, {
        
        filtered_df <- podsearch_df %>% 
          filter(number_episodes <= input$number_episodes_slider & explicit %in% input$explicit & grepl(input$genre, categories))
        
        pod_match <- sample_n(filtered_df, 1)
      })
      pod_match %>% 
        mutate(title = paste0(h2(""),
                              h4("Meet your match!"),
                              h3(),
                              h3(title),
                              img(src=paste(zodiac, ".png", sep = "")), # added conditional for zodiac image
                              h5(description),
                              column(4,
                                     h5("Number of Episodes:",number_episodes, "episodes"),
                                     h5("Birthday (air-date):", birthday)
                                     
                              ), # end of baby column 1
                              column(4,
                                     h5("Zodiac Sign:", zodiac),
                                     h5("Explicit:", explicit)
                              ),
                              column(4,
                                     h5("Average Rating:", average_rating),
                                     h5("Categories:", categories)
                                     ),
                              column(12),
                              column(12,
                                     actionButton("shuffleButton", "Shuffle", icon = icon("random")),
                                     tags$button(
                                       id = "shuffle_button",
                                       class = "btn action-button",
                                       tags$img(src = "random2.jpg", height = "5%")
                                     )),
                              column(12,
                                     actionButton("podlinkButton", "Show Link", icon = icon("podcast"),
                                                  onclick = paste0("window.open('", show_link, "', '_blank')") # added link functionality
                                     )))) %>% 
        pull(title)
      
    }
    
    
    
  })
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
