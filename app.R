#' Shiny app to display Floating Forests subjects and classifications
#' @author Isaac Rosenthal
#'
#'

#load libraries
library(shiny)
library(readr)
library(dplyr)
library(sf)

#load data
subjects_raw <- read_csv("data/floating-forests-subjects.csv")

#clean up image URLs
subjects_clean <- subjects_raw %>%
  mutate(URL = gsub(pattern = "\\{\"0\":\"", replacement = "", x = locations)) %>%
  mutate(URL = gsub(pattern = "\"\\}", replacement = "", x = URL))
  

#Build the app!

# Define UI 
ui <- fluidPage(
    textInput("subject", label = "Enter a Subject id"),
    textOutput("zooniverse_subject_id"),
    textOutput("zooniverse_subject_URL"),
    htmlOutput("zooniverse_subject_image")
)

server <- function(input, output, session) {
  output$zooniverse_subject_id <- renderText({
    paste0("Zooniverse ID: ",  filter(subjects_clean, subject_id == input$subject)$subject_id)
  })
  output$zooniverse_subject_URL <- renderText({
    paste0("Image URL: ",  filter(subjects_clean, subject_id == input$subject)$URL)
  })
  
  output$zooniverse_subject_image <- renderUI({
    
    src <- filter(subjects_clean, subject_id == input$subject)$URL
    tags$img(src=src, height = "75%", width = "75%")
  })
  
}
shinyApp(ui, server)
