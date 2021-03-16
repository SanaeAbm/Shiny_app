

#Loading libraries:
library(shiny)
library(tidyverse)
library(dplyr)
library(AER)
library(leaflet)
library(magrittr)
library(MASS)
library(plotly)


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
                      DT::DTOutput("esTable"),
                      h4("All models are wrong but some are useful")
)
                      
#Data tyding:
all_list=as.vector(names(HousePrices))
cont_list=c("price","lotsize" )
categ_list=setdiff(all_list,cont_list)
predictors_lis=c(categ_list,"lotsize")
response="price"
HousePrices[,categ_list] %<>% lapply(function(x) as.factor(as.numeric(x)))
plot_list=c("Histogram","Scatterplot","Boxplot")

data2<-HousePrices
data2[,categ_list] %<>% lapply(function(x) as.numeric(as.factor(x)))
data3<-HousePrices[,categ_list]


#Datset:
data_panel <- tabPanel("Dataset",
                       DT::dataTableOutput("data")
)


#Create descriptive panel:
descript_panel<-tabPanel("Descriptive analysis",
                       fluidPage(
                        sidebarLayout(sidebarPanel(
                          selectInput("select_V", label = h4("Choose main variable"), 
                                      choices = cont_list,
                                      selected = 1),
                          selectInput("select_G", label = h4("Choose grouping variable"), 
                                      choices = categ_list,
                                      selected = 1),
                          selectInput("select_P", label = h4("Choose plot type"), 
                                      choices = plot_list,
                                      selected = 1),
                          
                          uiOutput("tab")
                        
                          ),
                          
                          mainPanel(plotOutput( "p_uni",click = "click"),
                                    uiOutput("slider"),
                                    div(style="width:500px;",fluidRow(verbatimTextOutput("plot_info", placeholder = FALSE))),
                                    h4("Descriptive statistics of the main selected variable"),
                                    div(style="width:500px;",fluidRow(verbatimTextOutput("summary1", placeholder = TRUE))),
                                    h4("Descriptive statistics of the grouping selected variable"),
                                    div(style="width:500px;",fluidRow(verbatimTextOutput("summary2", placeholder = TRUE)))
                          )
                                   
                                   
                                    
                        )))

  
#Describe model section:

model<-tabPanel("Model section",
            
                tabsetPanel(
                  tabPanel("Build model", fluidPage(
                    sidebarLayout(sidebarPanel(
                      checkboxGroupInput("checkGroup", label = h4("Select the predictors"), 
                                         choices = predictors_lis,
                                         selected = "lotsize"),
                      h5("Let's use a criterion to validate your selection"),
                      selectInput("select_criterion", label = h4("Choose the criterion"), 
                                  choices =list("AIC"=2,"BIC"=log(nrow(HousePrices)))),
                                              selected = "AIC")
        
                      
                  ,
                      mainPanel( 
                        h4("Summary of the selected model"),
                                 div(style="width:500px;",fluidRow(verbatimTextOutput("model_summary", placeholder = TRUE))),
                        h4("Summary of the final model"),
                        div(style="width:500px;",fluidRow(verbatimTextOutput("model_summary2", placeholder = TRUE))))
                  
                      
                      
                    ))),
                    
                  tabPanel("Diagnosis",fluidPage(
                           mainPanel(plotlyOutput( "qqplot"),
                                     plotlyOutput( "rf")
                           ))),
                  tabPanel("Prediction")
                  
))





# Define UI for application:
ui <- fluidPage(navbarPage("",
                           intro_panel,
                           data_panel,
                           descript_panel,
                           model
                           
)
)



# Define server logic:
server <- function(input, output) { 
  
  output$data <- DT::renderDataTable({
    DT::datatable(HousePrices, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
    
  })
                             
  
  
  
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
            
            output$plot_info <- renderPrint({
              req(input$select_P== "Boxplot")
              if (is.null(input$click$y)) cat ("No boxplot selected")
              else{
                  y_name=HousePrices[,input$select_G]
                  lvls <- levels(y_name)
                  name<-as.numeric(input$click$y)
                  class <- floor(name)
                  a=which(categ_list %in% input$select_G)
                  b=as.numeric(a)
                  head(subset(data3,data3[,b]==class),5)
              }
            
            
            })
            
          
           
                ggplot(HousePrices, aes(x=x_name, y=as.character(y_name), fill=y_name)) +
                  geom_boxplot()+scale_fill_brewer(palette="RdBu")
            
            
            
          }
          
          else if (input$select_P== "Histogram") {

                        output$slider<-renderUI({
              req(input$select_P== "Histogram")
              sliderInput("n_bins", label="Number of bins", min = 1, max = 50, value = input$n_bins)

            })
            
            url <- a("What is an histogram?", href="https://en.wikipedia.org/wiki/Histogram")
            
            output$tab <- renderUI({
              
              tagList(url)
              
            })
  
            
            ggplot( HousePrices, aes( x=x_name, color=y_name, fill=y_name) ) +
              geom_histogram(position="identity", alpha=0.5,bins=input$n_bins) +
              ggtitle( "Frequency histogram")
           
          }
    
          else if (input$select_P== "Scatterplot") {
            
            url <- a("What is a Scatterplot?", href="https://en.wikipedia.org/wiki/Scatter_plot")
            output$tab <- renderUI({
              tagList(url)
            })
            
            output$plot_info <- renderText({
              req(input$select_P== "Scatterplot")
              if (is.null(input$click$y)) paste0("No data point selected")
              
              else{
              a=strtrim(input$click$y,6)
              paste0("Main variable value =",a)
            }
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
  
   output$model_summary<-renderPrint({
     preds=paste(input$checkGroup,collapse="+")
     fml = as.formula(paste(response,"~", preds))
     fit = lm(fml, data=HousePrices)
     summary(fit)
     
   })
   
   output$model_summary2<-renderPrint({
     preds=paste(input$checkGroup,collapse="+")
     fml = as.formula(paste(response,"~", preds))
     fit = lm(fml, data=HousePrices)
     c=as.numeric(input$select_criterion)
     best <-stepAIC(fit, data = HousePrices, maxit=10,k=c)$terms
     final_model <- lm(best, data = HousePrices)
     summary(final_model)
     
    
      
   })
   
   output$qqplot<- renderPlotly({
     preds=paste(input$checkGroup,collapse="+")
     fml = as.formula(paste(response,"~", preds))
     fit = lm(fml, data=HousePrices)
     c=as.numeric(input$select_criterion)
     best <-stepAIC(fit, data = HousePrices, maxit=10,k=c)$terms
     final_model <- lm(best, data = HousePrices)
     summary(final_model)
     
     p2 <- ggplot(final_model, aes(qqnorm(.stdresid)[[1]], .stdresid))+geom_point(na.rm = TRUE)
     p2 <- p2+geom_abline()+xlab("Theoretical Quantiles")+ylab("Standardized Residuals")
     p2 <- p2+ggtitle("Normal Q-Q")+theme_bw()
     
     print(ggplotly(p2))
     
   })
   
   output$rf<-renderPlotly({
     preds=paste(input$checkGroup,collapse="+")
     fml = as.formula(paste(response,"~", preds))
     fit = lm(fml, data=HousePrices)
     c=as.numeric(input$select_criterion)
     best <-stepAIC(fit, data = HousePrices, maxit=10,k=c)$terms
     final_model <- lm(best, data = HousePrices)
     summary(final_model)
     
     p33<-ggplot(final_model, aes(x = .fitted, y = .resid)) + geom_point()
     p3<-p33+stat_smooth(method="loess")+geom_hline(yintercept=0, col="red", linetype="dashed")
     p3<-p33+xlab("Fitted values")+ylab("Residuals")
     p3<-p33+ggtitle("Residual vs Fitted Plot")+theme_bw()
     
     print(ggplotly(p3))

   })
  
   
   
   
}


# Run the application 
shinyApp(ui = ui, server = server)
