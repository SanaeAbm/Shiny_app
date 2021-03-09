
#Loading libraries:
library(shiny)
library(tidyverse)
library(AER)
library(leaflet)
library(magrittr)



#Get data:
data("HousePrices")

#Map coordinates:
windsor<-data.frame(lat=c(42.317099),
                   lon=c(-83.0353434))

#Create first panel:
intro_panel<-tabPanel("Home",
                      mainPanel(
                      HTML(
                        paste(
                          h3("PREDICTING HOUSE PRICING IN WINDSOR, CANADA"),'<br/>'
                        )
                      )
                    ),
                    
                      fluidRow(
                        column(width = 8,
                               plotOutput("map",
                                          
                               )),
                        column(width = 4,
                               h2("Info")
                               
                        )
                      )
)
  
  
  
  
  






# Define UI for application:
ui <- fluidPage(navbarPage("",
                           intro_panel
)
)

# Define server logic:
server <- function(input, output) { 
  
  
  output$map <- renderPlot(
    leaflet() %>% 
      addProviderTiles("PREDICTING HOUSE PRICES IN WINDSOR, CANADA") %>% 
      addCircleMarkers(data = windsor,
                       lat = ~lat, lng = ~lon,
                       color = "blue")
    
    
  )
  
  
  
  
  
  
  
  }

# Run the application 
shinyApp(ui = ui, server = server)
