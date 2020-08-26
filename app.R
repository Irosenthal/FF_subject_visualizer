#' Shiny app to display Floating Forests subjects and classifications
#' @author Isaac Rosenthal
#'
#'

#load libraries
library(shiny)
library(readr)
library(dplyr)
library(sf)
library(raster)
library(ggplot2)
library(RStoolbox)

#helper functions - TO DO: put these in a separate file

rasterize_relaunch_image <- function(arow){
  #get URL  
  loc <- arow$locations
  #download tile  
  download.file(loc, basename(loc), mode = 'wb')
  #create the appropriate proj4string  
  zone = arow$`#utm_zone`
  proj = paste0("+proj=utm +zone=", zone , " +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  
  #rasterize  
  img <-brick(basename(loc),              
              crs=proj)
  #add tile corners
  img@extent = extent(c(arow$`#tile_UL_x`, arow$`#tile_LR_x`, 
                        arow$`#tile_LL_y`, arow$`#tile_UR_y`))
  
  return(img)
  
}
brick_relaunch_image <- function(arow){
  #get URL  
  loc <- arow$locations
  #download tile  
  download.file(loc, basename(loc), mode = 'wb')
  #create the appropriate proj4string  
  zone = arow$`#utm_zone`
  proj = paste0("+proj=utm +zone=", zone , " +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  
  #rasterize  
  img <-brick(basename(loc),              
              crs=proj)
  #add tile corners
  img@extent = extent(c(arow$`#tile_UL_x`, arow$`#tile_LR_x`, 
                        arow$`#tile_LL_y`, arow$`#tile_UR_y`))
  
  return(img)
  
}

#load data
subjects <- readRDS("../clean/data/relaunch_data/level_0/ff_relaunch_subjects.RDS")

#roll all this cleanup into readRDS call once I find all issues
subjects$`#tile_UL_x` <- as.numeric(subjects$`#tile_UL_x`)
subjects$`#tile_LR_x` <- as.numeric(subjects$`#tile_LR_x`)
subjects$`#tile_LL_y` <- as.numeric(subjects$`#tile_LL_y`)
subjects$`#tile_UR_y` <- as.numeric(subjects$`#tile_UR_y`)

#TO DO: dynamically load classification data based on subject query
classifications <- readRDS("../clean/data/relaunch_data/level_0/utm_zone_20_sf.RDS")



#Build the app!

# Define UI 
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      textInput("subject", label = "Enter a Subject id"), #user enters a subject of interest
      textOutput("zooniverse_subject_id"), #echo useful information 
      textOutput("zooniverse_subject_URL"),#echo useful information 
      uiOutput("classification",label = "please choose classifications", choices = "please enter a subject")), #do it like this so that I can reactively display only the classification IDs that go with the queried subject
    
  mainPanel(
    plotOutput("user_classifications"), #classification polygons on top of subject, converted from png to raster
    #plotOutput("interactive_tile"), #given a subject, display a clickable list of user classifications. CURRENTLY BUGGED AND WILL CRASH THE APP
    tableOutput("interactive_data_test"))#sanity checker
))

server <- function(input, output, session) {
  
  #make group checkbox input
  output$classification = renderUI({
    #populate with all classification IDs from the specified subject
    checkboxGroupInput(inputId = "classification_ID_select", #name of input
                       label = "Classification ID:", #label displayed in ui
                       choices = unique(one_subject_classifications()$classification_id), 
                       selected = "Please Enter a Subject")
  })
  
  
  #for finding subject metadata 
  one_subject <- reactive({
    dplyr::filter(subjects, subject_id == input$subject)
    })
  
  #for finding all classifications from that subject
  one_subject_classifications <- reactive({
    filter(classifications,
           subject_ids == input$subject)
    })
  
  #for displaying specific classifications 
  #does not currently work with plotting, but produces expected output when rendering as a table
  specific_classifications <- reactive({
    dplyr::filter(one_subject_classifications(),
           classification_id %in% input$classification_ID_select)
  })

  #text output
  output$zooniverse_subject_id <- renderText({
    paste0("Zooniverse ID: ",  one_subject()$subject_id)
  })
  
  #text output
  output$zooniverse_subject_URL <- renderText({
    paste0("Image URL: ",  one_subject()$locations)
  })
  
  
  
  #interactive tile 
  #this crashes R 
  #output$interactive_tile <- renderPlot({
    #rasterize
    #r <- brick_relaunch_image(one_subject())
    #plot with classifications
   # ggplot() +
    #  ggRGB(img = r, ggLayer = TRUE) +
    #  geom_sf(data = dplyr::select(specific_classifications(), classification_id), 
#              aes(color = as.factor(classification_id)),
 #             fill = NA)
  
  #})
  
  
  #use a table to test if checkbox UI input works
  #it does, or at least returns the expected classification IDs.
  #there is some weirdness here with lists - I need to use $ notation for this to render
  output$interactive_data_test <- renderTable({
    
    specific_classifications()$classification_id
    
  })
  
  
  ##plot with tile + classifications
  output$user_classifications <- renderPlot({
  #rasterize FF subject
  r <- brick_relaunch_image(one_subject())
  #plot with classifications
  ggplot() +
   ggRGB(img = r, ggLayer = TRUE) +
   geom_sf(data = one_subject_classifications(), 
  aes(color = as.factor(classification_id)),
   fill = NA) 
  })
  
  
}

shinyApp(ui, server)

#test on subject 15115972 (has kelp, land, classifications)

#todo:
#instead of a static map, create a leaflet map - rasters are problematic. it can't seem to reproject properly
#for now, create a list of classification IDs (user names?) and allow users to click them on and off 1 by 1 or export as an sf