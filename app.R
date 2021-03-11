



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

#Create intro panel:
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
                      
#Data tyding:
all_list=as.vector(names(HousePrices))
cont_list=c("price","lotsize" )
categ_list=setdiff(all_list,cont_list)
HousePrices[,categ_list] %<>% lapply(function(x) as.factor(as.numeric(x)))
plot_list=c("Histogram","Scatterplot","Boxplot")

data2<-HousePrices
data2[,categ_list] %<>% lapply(function(x) as.numeric(as.factor(x)))




#Create descriptive panel:
descript_panel<-tabPanel("Descriptive analysis",
                       fluidPage(
                        sidebarLayout(sidebarPanel(
                          selectInput("select_V", label = h3("Choose main variable"), 
                                      choices = cont_list,
                                      selected = 1),
                          selectInput("select_G", label = h3("Choose grouping variable"), 
                                      choices = categ_list,
                                      selected = 1),
                          selectInput("select_P", label = h3("Choose plot type"), 
                                      choices = plot_list,
                                      selected = 1),
                          
                          uiOutput("tab")
                        
                          ),
                          
                          mainPanel(plotOutput( "p_uni"),
                                    h4("Descriptive statistics of the main selected variable"),
                                    div(style="width:400px;",fluidRow(verbatimTextOutput("summary1", placeholder = TRUE))),
                                    h4("Descriptive statistics of the grouping selected variable"),
                                    div(style="width:400px;",fluidRow(verbatimTextOutput("summary2", placeholder = TRUE)))
                          )
                                   
                                   
                                    
                        )))

  
  

# Define UI for application:
ui <- fluidPage(navbarPage("",
                           intro_panel,
                           descript_panel
                           
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
   
    
    output$p_uni<- renderPlot({
          x_name=HousePrices[,input$select_V]
          y_name=HousePrices[,input$select_G]
          
          if (input$select_P== "Boxplot") {
            url <- a("What is a Boxplot?", href="https://en.wikipedia.org/wiki/Box_plot")
            output$tab <- renderUI({
              
              tagList(url)
              
            })
            ggplot(HousePrices, aes(x=x_name, group=y_name, fill=y_name)) +
              geom_boxplot()+scale_fill_brewer(palette="RdBu")
          }
          
          else if (input$select_P== "Histogram") {
            url <- a("What is an histogram?", href="https://en.wikipedia.org/wiki/Histogram")
            
            output$tab <- renderUI({
              
              tagList(url)
              
            })
            ggplot( HousePrices, aes( x=x_name, color=y_name, fill=y_name) ) + geom_histogram(position="identity", alpha=0.5) +
              ggtitle( "Frequency histogram")
          }
    
          else if (input$select_P== "Scatterplot") {
            url <- a("What is a Scatterplot?", href="https://en.wikipedia.org/wiki/Scatter_plot")
            output$tab <- renderUI({
              
              tagList(url)
              
            })
            ggplot(HousePrices, aes(x = 1:nrow(HousePrices), y = x_name, color=y_name)) +
              geom_point() + labs(x = "Index")
            
          }
    })
      
      
    
    output$summary1<- renderPrint({
      x_name=data2[,input$select_V]
      summary(x_name) 
    })
    
    output$summary2<- renderPrint({
      y_name=data2[,input$select_G]
      summary(y_name) 
    })
  

}


# Run the application 
shinyApp(ui = ui, server = server)
