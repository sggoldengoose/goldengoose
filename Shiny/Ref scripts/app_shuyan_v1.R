
library(tidyverse)
library(shinydashboard)
library(shinythemes)
library(shiny)
library(ggstatsplot)

##LOAD DATA
sales <- read_csv("data/sales_prep.csv")
sales <- sales %>%
  mutate(across(where(is.character), as.factor)) %>%
  mutate(Postal_District = as.factor(Postal_District)) %>%
  mutate(Time_to_CBD_min = Time_to_CBD_s/60) %>%
  mutate(Completion = case_when(Completion_Date == "Uncompleted" ~2022,
                                TRUE ~ as.numeric(as.character(Completion_Date))))

rent <- read_csv("data/rental_prep.csv")
rent <- rent  %>%
  mutate(across(where(is.character), as.factor)) %>%
  mutate(Postal_District = as.factor(Postal_District)) %>%
  mutate(Time_to_CBD_min = Time_to_CBD_s/60)


##START SHINY
header <- dashboardHeader(title = tags$a(href='https://singaporegoldengoose.netlify.app/index.html',
                                         tags$img(src='banner.png'), titleWidth = 230))

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Introduction", tabName = "intro", icon = icon("smile")),
    menuItem("Buy", icon = icon("coins")),
    menuItem("  - Overview", tabName = "buyoverview"),
    menuItem("  - Price Sensitivity", tabName = "buyprice"),
    menuItem("  - Time Trend", tabName = "buytimetrend"),
    menuItem("  - Mythbusting", tabName = "buymyths"),
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
                   tags$img(src = 'district.png', style="display: block; margin-left: auto; margin-right: auto;")
            )
    ),
    
    ##tab "Buy" Overview
    
    tabItem(tabName = "buyoverview"),
    
    ##tab "Buy" Price Sensitivity
    
    tabItem(tabName = "buyprice",
            
            fluidRow(
              box(title = "Choose attributes",
                  width=3,
                  height = 500,
                  status="primary",
                  solidHeader = TRUE,
                  radioButtons(
                    inputId="buy_price_var1",
                    label = "Attribute",
                    choices = c("Postal District" = "Postal_District",
                                "Type of Sale" = "Type_of_Sale",
                                "Size" = "Floor_Area_SQM_Bin",
                                "Property Type" = "Property_Type_Bin",
                                "Age" ="Age",
                                "Tenure" = "Tenure_Bin",
                                "Distance (m) to Nearest MRT" = "Dist_to_MRT_m",
                                "Driving Time (mins) to CBD" = "Time_to_CBD_min"),
                    selected = "Postal_District")
                  ),
              box(title = "Effect of Attribute on Price ($/sqm)",
                  width=9,
                  height = 500,
                  status="primary",
                  solidHeader = TRUE,
                  plotOutput(outputId = "buy_price_plot1")
                  )
            ),
            fluidRow(
              box(title = "Postal District Reference Map",
                  width=8,
                  status = "primary",
                  collapsible = TRUE,
                  solidHeader = TRUE,
                  tags$img(src = 'district_names.png', 
                           align = "left",
                           width="100%")
              )
            )
    ),

    ##tab "Buy" myths
    
    tabItem(tabName = "buymyths",
            
            fluidRow(
              
              tabBox(width=12, 
                     height=1000,
                tabPanel("Age - Size",
                         h3("Are older houses really bigger?"),
                         box(width=3,
                             height=600,
                             selectInput(inputId = "buy_myths_var1",
                                         label = "Choose housing type",
                                         choices = c("All"="All",
                                         "Condo/Apartment"="Condo/Apartment",
                                         "Executive Condo" = "Executive Condominium",
                                         "Terrace House" = "Terrace House",
                                         "Semi-D" = "Semi-Detached House",
                                         "Detached House" = "Detached House")
                                         )
                             ),
                         box(width=9,
                             height=600,
                             plotOutput(outputId = "buy_myth_plot1"))
                         ),
                tabPanel("Housing Type - MRT")
              )
            )
    ),
    
    ##tab "Buy" Time trend
    
    tabItem(tabName = "buytimetrend"),
    
    ##tab "Rent" Overview
    
    tabItem(tabName = "rentoverview"),
    
    ##tab "Rent" Price Sensitivity
    
    tabItem(tabName = "rentprice"),
    
    ##tab "Rent" Time trend
    
    tabItem(tabName = "renttimetrend")
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
  
  ##tab "Buy" Price Sensitivity
  output$buy_price_plot1 <- renderPlot({
    
    cat <- c("Postal_District", "Type_of_Sale", "Floor_Area_SQM_Bin", "Property_Type_Bin", "Tenure_Bin")
    cont <- c("Age", "Dist_to_MRT_m", "Time_to_CBD_min")

    if (input$buy_price_var1 %in% cat){
      p1 <- ggplot(data=sales,
                   aes_string(x=input$buy_price_var1, y="Unit_Price_PSM")) +
        geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))+
        scale_y_continuous(labels = scales::comma) +
        labs(title = paste("Violin plot of", input$buy_price_var1, "against unit price ($psm)"),
             subtitle = "some obervations",
             y = "Unit Price ($/sqm)") +
        theme_bw()+
        theme(plot.title = element_text(size = rel(1.5), face = "bold"),
              plot.subtitle = element_text(size = rel(1.3)))
    }

    if (input$buy_price_var1 %in% cont){
      p1 <- ggplot(data=sales,
                   aes_string(x=input$buy_price_var1, y="Unit_Price_PSM")) +
        geom_point()+
        geom_smooth(method=lm) +
        scale_y_continuous(labels = scales::comma) +
        labs(title = paste("Scatter plot of ", input$buy_price_var1, " against unit price ($psm)"),
             subtitle = "some obervations",
             y = "Unit Price ($/sqm)") +
        theme_bw()+
        theme(plot.title = element_text(size = rel(1.5), face = "bold"),
              plot.subtitle = element_text(size = rel(1.3)))
    }

    p1
    
  })
  
  output$buy_myth_plot1 <- renderPlot(
    width=900,
    height=600,
    {

    if(input$buy_myths_var1=="All"){
      p2 <- ggplot(data=sales,
                   aes(x=Completion, y=Area_SQM)) +
        geom_point()+
        scale_y_continuous(limits = c(0,3000)) +
        facet_wrap(vars(Property_Type_Bin)) +
        labs(title = "Floor area (sqm) against completion year by housing type",
             subtitle = "some obervations",
             x= "Completion Year",
             y = "Floor Area (sqm)") +
        theme_bw()+
        theme(plot.title = element_text(size = rel(1.5), face = "bold"),
              plot.subtitle = element_text(size = rel(1.3)))
        
    }
      
      else{
        p2 <- sales %>%
          filter(Property_Type_Bin == input$buy_myths_var1) %>%
          filter(Area_SQM<=3000) %>%
          ggscatterstats(
          x = Completion,
          y = Area_SQM,
          marginal = FALSE
        )
      }
    
    p2
  })
  
}

shinyApp(ui =ui, server = server)

