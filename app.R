library(shiny)
library(data.table)
library(tidyverse)
library(tidytext)

data = read.csv("billboard_lyrics_1965-2018.csv", header = TRUE)


ui <- fluidPage(
  
  # App title ----
  titlePanel("Song Lyrics Analysis"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "Years",
                  label = "Years",
                  min = 1965,
                  max = 2018,
                  value = 1965),
      
      selectInput(inputId = "sentiment", 
                  label = "Choose a sentiment",
                  choices = c("joy", 
                              "fear",
                              "anger", 
                              "surprise"),
                  selected = "joy"),
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
      
    )
  )
)

server <- function(input, output) {
  

  output$distPlot <- renderPlot({
    
    text_df <- tibble(lyrics = 1:nrow(data), text = data$Lyrics, song = data$Song, rank = data$Rank, artist = data$Artist, year = data$Year)
    
    tt  = text_df %>% filter(year == input$Years) %>% unnest_tokens(word, text)
    
    nrc_joy = get_sentiments("nrc") %>% 
      filter(sentiment == input$sentiment)
    
    lyrics_joy = tt %>% 
      select(word) %>% 
      inner_join(nrc_joy) %>% 
      count(word,sort=TRUE)
    
    lyrics_joy %>%
      filter(n > 15) %>%
      ggplot(aes(word, n)) + geom_col()
    
  })
  
}

shinyApp(ui = ui, server = server)