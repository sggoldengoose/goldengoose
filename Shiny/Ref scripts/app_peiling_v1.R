library(shinydashboard)
library(shinythemes)
library(shiny)
library(tidyverse)
library(ggHoriPlot)
library(ggplot2)

##DATA PROCESSING
sales_data <- read_csv("data/sales_prep.csv")
rental_data <- read_csv("data/rental_prep.csv")
postal_data <- read_csv("data/postal_districts.csv")
exam <- read_csv("data/Exam_data.csv")


header <- dashboardHeader(title = tags$a(href='https://singaporegoldengoose.netlify.app/index.html',
                                         tags$img(src='banner.png'), titleWidth = 230))
  
sidebar <- dashboardSidebar(
  sidebarMenu(
  menuItem("Introduction", tabName = "intro", icon = icon("smile")),
  menuItem("Buy", icon = icon("coins")),
  menuItem("  - Overview", tabName = "buyoverview"),
  menuItem("  - Price Sensitivity", tabName = "buyprice"),
  menuItem("  - Time Trend", tabName = "buytimetrend"),
  menuItem("Rent", icon = icon("bed")),
  menuItem("  - Overview", tabName = "rentoverview"),
  menuItem("  - Price Sensitivity", tabName = "rentprice"),
  menuItem("  - Time Trend", tabName = "renttimetrend")
)
)

##INTRO

body <- dashboardBody(
  tabItems(
    ##tab "Introduction" Body
    tabItem(tabName = "intro",
            fluidPage(theme = shinytheme('flatly'), #https://rstudio.github.io/shinythemes/
                    titlePanel(h1("Singapore Golden Goose: How to spot a Golden Goose (a.k.a.) Profitable Rental Property in Singapore")),
                    fluidRow(
                      infoBox(h4("No. of Sales Transactions"), "82,360", icon=icon("stamp"), color="navy", fill=TRUE),
                      infoBox(h4("No. of Lease Transactions"), "298,639", icon=icon("bed"), color="orange", fill=TRUE),
                      infoBox(h4("Period Covered"), "Jan 2019 to Feb 2022", icon=icon("calendar"), color="yellow", fill=TRUE),
                      column(width = 10,
                             h3('Our Application'),
                             p('Using SGGoldenGoose Application, landlords-wannabes can analyse and visualize sales and rental transactions in ease, such that more informed choices about investing in private residential property in Singapore to earn passive rental income can be made.'),
                             h3('Data'),
                             p('We have used data from Real Estate Information System (REALIS) database in this application. Sales and rental transaction records in the Singapore’s private residential market from years Jan 2019 to Feb 2022 have been extracted.'),
                             p('-  The Sale dataset consists of 21 variables and 82,360 datapoints'),
                             p('-  The Rental dataset consistS of 9 variables and 298,639 datapoints'),
                             h3('Application Features'),
                             p('As seen in the sidebar menu on the left, this application has 2 modules – Sales and Rent. Each module has 3 tabs:'),
                             p('- Overview: allows you to select varaibles such as postal district (please refer to the postal district map below for referance), size, tenure etc to identify interesting patterns via interactive visualisations.'),
                             p('- Price Sensitivity: allow you to select up to two attributes to view their relationship against sale/rental price in $/sqm.'),
                             p('- Time Trend: show time trends of sales/rental prices ($/sqm).')
                             )
                             )
                    ),
            column(width = 10,
                   tags$img(src = 'district.png', style="display: block; margin-left: auto; margin-right: auto;"))
            
            ),
    
##tab "Buy" Overview

    tabItem(tabName = "buyoverview",
            
            fluidRow(
              infoBoxOutput("BuyTransactionNum"),
              infoBoxOutput("MeanBuy"),
              infoBoxOutput("LastBuyDate")
            ),
          
            fluidRow(
              box(
                title = "Map",width = 6, status = "danger", solidHeader = TRUE,
                collapsible = TRUE,
                plotOutput("plot3", height = 500)
              ),
              box(
                title = "Violin Plot", width = 6, status = "primary", solidHeader = TRUE,
                collapsible = TRUE,
                plotOutput("plot3", height = 500)
              )),
            
            fluidRow(
              box(title = "Which District?", width=4,status = "warning", solidHeader = TRUE,
                  selectInput(inputId = "postaldistrict",
                               label = "Postal District:",
                               choices = c("1 - Raffles Place, Cecil, Marina, People's Park" = "ENGLISH",
                                           "Maths" = "MATHS",
                                           "Science" = "SCIENCE"),
                               selected = "ENGLISH")),
              box(title = "What type of Property?", width=4, status = "warning", solidHeader = TRUE,
                   radioButtons(inputId = "propertytype",
                               label = "Property Type:",
                               choices = c("English" = "ENGLISH",
                                           "Maths" = "MATHS",
                                           "Science" = "SCIENCE"),
                               selected = "ENGLISH"),
                   radioButtons(inputId = "tenure",
                                label = "Tenure:",
                                choices = c("English" = "ENGLISH",
                                            "Maths" = "MATHS",
                                            "Science" = "SCIENCE"),
                                selected = "ENGLISH"),
                   selectInput(inputId = "floorarea",
                               label = "Floor Area (SQM):",
                               choices = c("English" = "ENGLISH",
                                           "Maths" = "MATHS",
                                           "Science" = "SCIENCE"),
                               selected = "ENGLISH"),
                   selectInput(inputId = "age",
                               label = "Age at Transaction:",
                               choices = c("English" = "ENGLISH",
                                           "Maths" = "MATHS",
                                           "Science" = "SCIENCE"),
                               selected = "ENGLISH")),
              box(title = "Scope of past transactions?", width=4, status = "warning", solidHeader = TRUE,
                   radioButtons(inputId = "typeofsales",
                                label = "Type of Sales:",
                                choices = c("English" = "ENGLISH",
                                            "Maths" = "MATHS",
                                            "Science" = "SCIENCE"),
                                selected = "ENGLISH"),
                   dateInput(inputId = "saledatefrom",
                               label = "Date (From):"),
                   dateInput(inputId = "saledateto",
                                       label = "Date (To):")
                ))
              

                      
            ),

##tab "Buy" Price Sensitivity

tabItem(tabName = "buyprice"),

##tab "Buy" Time trend

tabItem(tabName = "buytimetrend",
fluidRow(
  box(title = "Which District?", width=4,status = "warning", solidHeader = TRUE,
      selectInput(inputId = "postaldistrict",
                  label = "Postal District:",
                  choices = postal_data$Postal_Desc,
                  selected = "ENGLISH"),
 # title = "What type of Property?", width=4, status = "warning", solidHeader = TRUE,
      radioButtons(inputId = "propertytype",
                   label = "Property Type:",
                   choices = unique(sales_data$Property_Type_Bin))
      ),
  box(title = "Horizon Plot", width=10, status = "warning", solidHeader = TRUE
  )
  ),
fluidRow(
  box(title = "Horizon Plot", width=10, status = "warning", solidHeader = TRUE
  ))
)

,
##tab "Rent" Overview

tabItem(tabName = "rentoverview",
        
        fluidRow(
          infoBoxOutput("RentTransactionNum"),
          infoBoxOutput("MeanRent"),
          infoBoxOutput("LastRentDate")
        ),
        
        fluidRow(
          box(
            title = "Map",width = 6, status = "danger", solidHeader = TRUE,
            collapsible = TRUE,
            plotOutput("plot3", height = 500)
          ),
          box(
            title = "Violin Plot", width = 6, status = "primary", solidHeader = TRUE,
            collapsible = TRUE,
            plotOutput("plot3", height = 500)
          )),
        
        fluidRow(
          box(title = "Which District?", width=4,status = "warning", solidHeader = TRUE,
              selectInput(inputId = "postaldistrict",
                          label = "Postal District:",
                          choices = c("English" = "ENGLISH",
                                      "Maths" = "MATHS",
                                      "Science" = "SCIENCE"),
                          selected = "ENGLISH")),
          box(title = "What type of Property?", width=4, status = "warning", solidHeader = TRUE,
              radioButtons(inputId = "propertytype",
                           label = "Property Type:",
                           choices = c("English" = "ENGLISH",
                                       "Maths" = "MATHS",
                                       "Science" = "SCIENCE"),
                           selected = "ENGLISH"),
              radioButtons(inputId = "bedroom",
                           label = "No. of Bedrooms:",
                           choices = c("English" = "ENGLISH",
                                       "Maths" = "MATHS",
                                       "Science" = "SCIENCE"),
                           selected = "ENGLISH"),
              selectInput(inputId = "floorarea",
                          label = "Floor Area (SQM):",
                          choices = c("English" = "ENGLISH",
                                      "Maths" = "MATHS",
                                      "Science" = "SCIENCE"),
                          selected = "ENGLISH"),
              selectInput(inputId = "rentsqm",
                          label = "Monthly Rent/SQM ($):",
                          choices = c("English" = "ENGLISH",
                                      "Maths" = "MATHS",
                                      "Science" = "SCIENCE"),
                          selected = "ENGLISH"),
              radioButtons(inputId = "Age at Transaction",
                          label = "rentage:",
                          choices = c("English" = "ENGLISH",
                                   "Maths" = "MATHS",
                                   "Science" = "SCIENCE"),
                          selected = "ENGLISH")),
          box(title = "Scope of past transactions?", width=4, status = "warning", solidHeader = TRUE,
              dateInput(inputId = "leasedatefrom",
                        label = "Date (From):"),
              dateInput(inputId = "leasedateto",
                        label = "Date (To):")
              )
        ))
        
,

##tab "Rent" Price Sensitivity

tabItem(tabName = "rentprice"),

##tab "Rent" Time trend

tabItem(tabName = "renttimetrend",
fluidRow(
  box(title = "Which District?", width=6,status = "warning", solidHeader = TRUE,
      selectInput(inputId = "postaldistrict",
                  label = "Postal District:",
                  choices = c("1 - Raffles Place, Cecil, Marina, People's Park" = "ENGLISH",
                              "Maths" = "MATHS",
                              "Science" = "SCIENCE"),
                  selected = "ENGLISH")),
  box(title = "What type of Property?", width=6, status = "warning", solidHeader = TRUE,
      radioButtons(inputId = "propertytype",
                   label = "Property Type:",
                   choices = c("English" = "ENGLISH",
                               "Maths" = "MATHS",
                               "Science" = "SCIENCE"),
                   selected = "ENGLISH"),
      radioButtons(inputId = "tenure",
                   label = "Tenure:",
                   choices = c("English" = "ENGLISH",
                               "Maths" = "MATHS",
                               "Science" = "SCIENCE"),
                   selected = "ENGLISH"),
      selectInput(inputId = "floorarea",
                  label = "Floor Area (SQM):",
                  choices = c("English" = "ENGLISH",
                              "Maths" = "MATHS",
                              "Science" = "SCIENCE"),
                  selected = "ENGLISH"),
      selectInput(inputId = "age",
                  label = "Age at Transaction:",
                  choices = c("English" = "ENGLISH",
                              "Maths" = "MATHS",
                              "Science" = "SCIENCE"),
                  selected = "ENGLISH")),
  box(
    plotOutput("distPlot")
  ))
)
)


)
  

##UI Page##

ui <- dashboardPage(
  skin = 'yellow', 
  header, 
  sidebar, 
  body)

## Server Page##

server <- function(input, output){
  
##output Buy infobox - WIP
  output$BuyTransactionNum <- renderInfoBox({
    infoBox(
      "No. of Sales Transactions", paste0("2,000"), icon = icon("list-ol"), #      "No. of Sales Transactions", paste0(25 + input$count, "%"), icon = icon("list-ol"),
      color = "navy", fill = TRUE
    )
  })
  
  output$MeanBuy <- renderInfoBox({
    infoBox(
      "Mean Transaction Price ($)", "$1,345,111", icon = icon("money"),
      color = "orange", fill = TRUE
    )
  })
  
  output$LastBuyDate <- renderInfoBox({
    infoBox(
      "Last Transaction Date", "MMM-YY", icon = icon("calendar", lib = "glyphicon"),
      color = "orange", fill = TRUE
    )
  })
  
  output$distPlot <- renderPlot({
    p <- ggplot(exam, aes_string(ENGLISH)) +
      geom_histogram(bins = 20,
                     color="black",
                     fill="light blue")
    p
  })
  
}
  
shinyApp(ui, server)
  
