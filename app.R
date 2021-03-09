



#Loading libraries:
library(shiny)
library(tidyverse)
library(AER)
library(leaflet)
library(magrittr)
library(Cairo)



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
                      leafletOutput('map', width = '100%', height = '300px'),
                      absolutePanel(top = 10, right = 10, id = 'controls'
                      ),
                      DT::DTOutput("esTable")
)
                      
                      
                          
        
  
  

# Define UI for application:
ui <- fluidPage(navbarPage("",
                           intro_panel
)
)

# Define server logic:
server <- function(input, output) { 
  
  
  output$map <-renderLeaflet({
    leaflet() %>% 
      addTiles() %>% 
      addCircleMarkers(data = windsor,
                       lat = ~lat, lng = ~lon,
                       color = "blue")
    
  })
  

  

    
    }


# Run the application 
shinyApp(ui = ui, server = server)
