library(shiny)
library(tidyverse)
library(DT)
library(tools)
library(shinythemes)
library(spotifyr)
library(ggthemes)

Sys.setenv(SPOTIFY_CLIENT_ID = 'XXXXXXXXXXXXXXXXXXXXXXXXXXX')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'XXXXXXXXXXXXXXXXXXXXXXXX')
access_token <- get_spotify_access_token()

kanye <- get_artist_audio_features(artist = "kanye west")

kanye2 <- kanye %>%
  filter(!(album_name %in% c("808s & Heartbreak (Softpak)", "Late Orchestration", 
                             "The College Dropout (Edited)", 
                             "Graduation (Alternative Business Partners)"))) %>%
  select(-c(artist_uri, album_uri, album_type, is_collaboration, track_uri,
            track_preview_url, album_release_year, artist_name, album_img, 
            album_release_date, track_open_spotify_url, track_number,
            disc_number, key, mode, key_mode, album_popularity, time_signature))


ui <- shinyUI(
  fluidPage(
    theme = shinytheme("flatly"),
    titlePanel("Kanye's Song Attributes"),
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "x",
                    label = "X-axis:",
                    choices = c("Danceability" = "danceability", "Energy" = "energy",
                                "Speechiness" = "speechiness", "Acousticness" =
                                  "acousticness", "Instrumentalness" =
                                  "instrumentalness", "Liveness" = "liveness",
                                "Valence" = "valence", "Tempo" = "tempo", "Loudness" =
                                  "loudness"),
                    selected = "danceability"),
        
        selectInput(inputId = "y",
                    label = "Y-axis:",
                    choices = c("Danceability" = "danceability", "Energy" = "energy",
                                "Speechiness" = "speechiness", "Acousticness" =
                                  "acousticness", "Instrumentalness" =
                                  "instrumentalness", "Liveness" = "liveness",
                                "Valence" = "valence", "Tempo" = "tempo", "Loudness" =
                                  "loudness"),
                    selected = "energy"),
        br(),
        
        checkboxGroupInput(inputId = "albums",
                           label = "Select album(s) to plot:",
                           choices = levels(as.factor(kanye2$album_name)),
                           selected = c("808s & Heartbreak", 
                                        "My Beautiful Dark Twisted Fantasy") )
      ),
      
      mainPanel(
        plotOutput(outputId = "scatter", brush = "plot_brush"),
        br(),
        textOutput("text"),
        br(),
        dataTableOutput(outputId = "table")
      )
    )
  )
)

server <- function(input, output, session) {
  
  albums_checked <- reactive({
    req(input$albums)
    filter(kanye2, album_name %in% input$albums)
  })
  
  output$scatter <- renderPlot({
    ggplot(data = albums_checked(), aes_string(x = input$x, y = input$y)) +
      geom_point(size = 4, alpha = .80, aes(color = album_name)) +
      ggtitle(toTitleCase(paste(input$y, "vs", input$x))) +
      theme_fivethirtyeight() +
      theme(legend.position = "bottom", legend.title = element_blank(),
            plot.title = element_text(hjust = .5), axis.title = element_text())
  })
  
  output$table <- renderDataTable({
    datatable(data = albums_checked() %>%
                brushedPoints(input$plot_brush, xvar = input$x, 
                              yvar = input$y) %>%
                select(album_name, track_name, input$x, input$y))
  })
  
  output$text <- renderText(
    paste0("Highlight a group of points to populate data table!")
  )
}

shinyApp(ui, server)

