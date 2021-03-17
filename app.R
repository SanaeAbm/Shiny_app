

#Loading libraries:
library(shiny)
library(shinyjs)
library(tidyverse)
library(dplyr)
library(AER)
library(leaflet)
library(magrittr)
library(MASS)
library(plotly)
library(shinythemes)


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
                            h3("PREDICTING HOUSE PRICING IN WINDSOR, CANADA", align="center"),
                            br()
                            
                          )
                        )
                      ),
                      leafletOutput('map', width = '100%', height = '300px'),
                      absolutePanel(top = 10, right = 10, id = 'controls'
                      ),
                      DT::DTOutput("esTable"),
                      br(),
                      h4("All models are wrong but some are useful",align='center')
)
                      
#Data tyding:
all_list<<-as.vector(names(HousePrices))
cont_list<<-c("price","lotsize" )
categ_list<<-setdiff(all_list,cont_list)
predictors_lis<<-c(categ_list,"lotsize")
response<<-"price"
HousePrices[,categ_list] %<>% lapply(function(x) as.factor(as.numeric(x)))
plot_list<<-c("Histogram","Scatterplot","Boxplot")

data2<-HousePrices
data2[,categ_list] %<>% lapply(function(x) as.numeric(as.factor(x)))
data3<-HousePrices[,categ_list]

`%not_in%` <- purrr::negate(`%in%`)

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
                                         selected = "lotsize"

                                         ),
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
                           fluidRow(column(width=7,
                                     h4("Testing normality"),
                                     plotlyOutput( "qqplot")),
                                    column (width = 5, 
                                            br(),br(),br(),br(),br(),
                                            div(style="width:500px;",fluidRow(verbatimTextOutput("shapiro", placeholder = TRUE)),
                                            fluidRow(verbatimTextOutput("shapiro_result", placeholder = TRUE)))),
                                    
                                         
                                    column(width=7,
                                           h4("Testing Homocedasticity"),   
                                            plotlyOutput( "rf")),
                                     column (width = 5,br(),br(),br(),br(),br(),
                                             div(style="width:500px;",fluidRow(verbatimTextOutput("bptest", placeholder = TRUE)),
                                                 fluidRow(verbatimTextOutput("bptest_result", placeholder = TRUE))))
                                     
                                     
                           ))),
                  tabPanel("Prediction",fluidPage(
                    useShinyjs(),
                    h4("Introduce a value for the selected variables in the previous section"),
                           uiOutput("fact_inputs"),
                           mainPanel(h4("The predicted house price according to the inputs is:"),
                            div(style="width:500px;",fluidRow(verbatimTextOutput("var_selected", placeholder = TRUE)))
                           )
                           
                    
                    
                           ))
))

              
             






# Define UI for application:
ui <- fluidPage(theme = shinytheme("cerulean"),
                navbarPage("",
                           intro_panel,
                           data_panel,
                           descript_panel,
                           model
                           
)
)



# Define server logic:
server <- function(input, output) { 
  
  output$data <- DT::renderDataTable({
    DT::datatable(HousePrices, options = list(lengthMenu = c(10, 20, 30), pageLength = 10))
    
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
              sliderInput("n_bins", label="Number of bins", min = 5, max = 50, value = input$n_bins)

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
     preds<<-paste(input$checkGroup,collapse="+")
     fml<<- as.formula(paste(response,"~", preds))
     fit<<- lm(fml, data=HousePrices)
     summary(fit)
     
   })
   
   output$model_summary2<-renderPrint({
     c=as.numeric(input$select_criterion)
     best <<-stepAIC(fit, data = HousePrices, maxit=10,k=c)$terms
     final_model <<- lm(best, data = HousePrices)
     summary(final_model)
     
    
      
   })
   
   output$qqplot<- renderPlotly({
     
     p2 <- ggplot(final_model, aes(qqnorm(.stdresid)[[1]], .stdresid))+geom_point(na.rm = TRUE)
     p2 <- p2+geom_abline()+xlab("Theoretical Quantiles")+ylab("Standardized Residuals")
     p2 <- p2+ggtitle("Normal Q-Q")+theme_bw()
     
     print(ggplotly(p2))
     
   })
   
   output$rf<-renderPlotly({
     
     p33<-ggplot(final_model, aes(x = .fitted, y = .resid)) + geom_point()
     p3<-p33+stat_smooth(method="loess")+geom_hline(yintercept=0, col="red", linetype="dashed")
     p3<-p33+xlab("Fitted values")+ylab("Residuals")
     p3<-p33+ggtitle("Residual vs Fitted Plot")+theme_bw()
     
     print(ggplotly(p3))

   })
  
   
   
   output$fact_inputs<- renderUI({
     lapply(input$checkGroup,function(x){
       textInput(inputId=x,
                    label=x,
                    value=1

        )

     }
    )

    
     
     
   })
   
     
     output$var_selected<- renderText({

       vars_sel<- as.vector(input$checkGroup)
       df = data.frame(matrix(""))
       
       sel<-function(){
         for (i in 1:length(predictors_lis)){
             if(predictors_lis[i] %not_in% vars_sel){
               a=predictors_lis[i]
               df[1,a]<<-0
               
             }
             
             else{
               
              if(predictors_lis[i]=="lotsize"){
               df[1,predictors_lis[i]]<<-as.numeric(input$lotsize)
              }
               if(predictors_lis[i]=="garage"){
                 df[1,predictors_lis[i]]<<-input$garage
               }
               
               if(predictors_lis[i]=="bedrooms"){
                 df[1,predictors_lis[i]]<<-input$bedrooms
               }
               
               if(predictors_lis[i]=="bathrooms"){
                 df[1,predictors_lis[i]]<<-input$bathrooms
               }
               
               if(predictors_lis[i]=="stories"){
                 df[1,predictors_lis[i]]<<-input$stories
               }
               
               if(predictors_lis[i]=="driveway"){
                 df[1,predictors_lis[i]]<<-input$driveway
               }
               
               if(predictors_lis[i]=="recreation"){
                 df[1,predictors_lis[i]]<<-input$recreation
               }
               
               if(predictors_lis[i]=="fullbase"){
                 df[1,predictors_lis[i]]<<-input$fullbase
               }
               
               if(predictors_lis[i]=="gasheat"){
                 df[1,predictors_lis[i]]<<-input$gasheat
               }
               
               if(predictors_lis[i]=="aircon"){
                 df[1,predictors_lis[i]]<<-input$aircon
               }
               
               if(predictors_lis[i]=="prefer"){
                 df[1,predictors_lis[i]]<<-input$prefer
               }
               
             }
           df$matrix....<-NULL
         }
         
       }
       
       sel()
       pred3<-predict(final_model,df)
       
       
      
       
     }) 
     
     output$shapiro<- renderPrint({
       res<<-residuals(final_model,type="response")
       shapiro<<-shapiro.test(res)
       shapiro
       
     })
     
     output$shapiro_result<- renderPrint({
       
       if(shapiro[2]<0.05){
         cat("We reject the null hypothesis, the residuals are not normally distributed")
       }
       else{
          cat("Residuals are normally distributed")
         
       }
       
       
       
     })
     
     output$bptest<- renderPrint({
       bp<<-bptest(fit)
       bp
       
     })
     
     output$bptest_result<- renderPrint({
       
       if(bp[4]<0.05){
         cat("We reject the null hypothesis, the residuals are not homoscedastic")
       }
       else{
         cat("Residuals are homoscedastic")
         
       }
       
       
       
     })
    
}
     
   
   



# Run the application 
shinyApp(ui = ui, server = server)
