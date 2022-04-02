library(shinydashboard)
library(shinythemes)
library(shiny)
library(tmap)
library(sf)
library(tidyverse)
library(lubridate)
library(mapview)
library(shinyWidgets)
library(plotly)
library(scales)
library(anytime)
library(fontawesome)
library(extrafont)
library(sjlabelled)

#################################  Data Prep for All ######################################################

sales <- read_csv("data/sales_prep.csv")
rent <- read_csv("data/rental_prep.csv")


#################################  Data Prep for Overview Tabs ############################################
sales_int <- sales %>%
  mutate("Sale_Date_p" = format((as.Date(Sale_Date, "%d-%b-%y")), format = "%Y-%m-%d"))
  
sales_sf <- st_as_sf(sales_int, coords = c("longitude","latitude")) 

rent_int <- rent %>%
  mutate(No_of_Bedroom_Bin = factor(No_of_Bedroom_Bin, levels = c("1", "2", "3", ">= 4"))) %>%
  mutate(Lease_month = substring(Lease_Commencement_Date,1,3)) %>%
  mutate(Lease_month = match(Lease_month, month.abb)) %>%
  mutate(Lease_year = as.numeric(substring(Lease_Commencement_Date,5,6))) %>%
  mutate(Lease_Date = paste0('01-',Lease_Commencement_Date)) %>%
  mutate(Lease_Date = anydate(as.Date(Lease_Date, "%d-%b-%y")))

rent_sf <- st_as_sf(rent_int, coords = c("longitude","latitude")) 


#################################  Site Menu Bar  ###########################################################

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

#################################  INTRODUCTION TAB  ###########################################################

body <- dashboardBody(
  tags$head(tags$style(HTML((".small-box {height: 90px}"),('.content-wrapper { overflow: auto; }')))),
  tabItems(
    ##tab "Introduction" Body
    tabItem(tabName = "intro",
            fluidPage(
              theme = shinytheme('flatly'), #https://rstudio.github.io/shinythemes/
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
    
#################################  Buy > Overview  TAB ###########################################################

    tabItem(tabName = "buyoverview",
            
            fluidRow(
              valueBoxOutput("BuyTransactionNum"),
              valueBoxOutput("MedBuy"),
              valueBoxOutput("LastBuyDate")
            ),
          
            fluidRow(
              box(
                title = "Map - Sales Transactions",width = 6, status = "danger", solidHeader = TRUE,
                collapsible = TRUE,
                tmapOutput("mapPlot", height = 430)
              ),
              box(
                title = "Violin Plot - Distribution of Sales Transaction Price", width = 6, status = "primary", solidHeader = TRUE,
                collapsible = TRUE,
                plotlyOutput("violinPlot", height = 430)
              )),
            
            fluidRow(
                box(title = "Which District? What Price?", width=3, height=230, status = "warning", solidHeader = TRUE,
                    pickerInput(
                      inputId = "postaldistrict", 
                      label = "Postal District:",
                      choices = c("1 - Raffles Place, Cecil, Marina, People's Park" = "1",
                                           "2 - Anson, Tanjong Pagar" = "2",
                                           "3 - Queenstown, Tiong Bahru" = "3",
                                           "4 - Telok Blangah, Harbourfront" = "4",
                                           "5 - Pasir Panjang, Hong Leong Garden, Clementi New Town" = "5",
                                           "6 - High Street, Beach Road (part)" = "6",
                                           "7 - Middle Road, Golden Mile" = "7",
                                           "8 - Little India" = "8",
                                           "9 - Orchard, Cairnhill, River Valley" = "9",
                                           "10 - Ardmore, Bukit Timah, Holland Road, Tanglin" = "10",
                                           "11 - Watten Estate, Novena, Thomson" = "11",
                                           "12 - Balestier, Toa Payoh, Serangoon" = "12",
                                           "13 - Macpherson, Braddell" = "13",
                                           "14 - Geylang, Eunos" = "14",
                                           "15 - Katong, Joo Chiat, Amber Road" = "15",
                                           "16 - Bedok, Upper East Coast, Eastwood, Kew Drive" = "16",
                                           "17 - Loyang, Changi" = "17",
                                           "18 - Tampines, Pasir Ris" = "18",
                                           "19 - Serangoon Garden, Hougang, Punggol" = "19",
                                           "20 - Bishan, Ang Mo Kio" = "20",
                                           "21 - Upper Bukit Timah, Clementi Park, Ulu Pandan" = "21",
                                           "22 - Jurong" = "22",
                                           "23 - Hillview, Dairy Farm, Bukit Panjang, Choa Chu Kang" = "23",
                                           "24 - Lim Chu Kang, Tengah" = "24",
                                           "25 - Kranji, Woodgrove" = "25",
                                           "26 - Upper Thomson, Springleaf" = "26",
                                           "27 - Yishun, Sembawang" = "27",
                                           "28 - Seletar" = "28"
                                           ),
                      selected = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28"),
                      options = list(
                        `actions-box` = TRUE,
                        `deselect-all-text` = "Clear All ok?",
                        `select-all-text` = "All districts please!!",
                        `none-selected-text` = "zero"
                    ), 
                    multiple = TRUE
                  ),
                  sliderInput(inputId = "pricepsm",
                              label = "Unit Price ($/SQM):",
                              min = 1072,
                              max = 53131,
                              value = c(3000,15000))),
                box(title = "What Size? What Age?", width=3,height=230, status = "warning", solidHeader = TRUE,
                    selectInput(inputId = "floorarea",
                                label = "Floor Area (SQM):",
                                choices = c("0 - 60" = "0 - 60",
                                            "61 - 90" = "61 - 90",
                                            "91 - 130" = "91 - 130",
                                            ">130" = ">130"),
                                selected = c("0 - 60","61 - 90")),
                    sliderInput(inputId = "age",
                                label = "Age (For Completed Properties):",
                                min = 0,
                                max = 92,
                                value = c(0,10))
                               ),
              box(title = "Tenure? Type of Sales?", width=2, height=230, status = "warning", solidHeader = TRUE,
                  awesomeCheckboxGroup(inputId = "tenure",
                                       label = "Tenure:",
                                       choices = c("< 99" = "<99",
                                                   "99 - 110" = "99 - 110",
                                                   "929 - Freehold" = "929 - Freehold"),
                                       selected = "99 - 110",
                                       inline = TRUE, 
                                       status = "primary"),
                  awesomeCheckboxGroup(inputId = "typeofsales",
                                       label = "Type of Sales:",
                                       choices = c("New Sale" = "New Sale",
                                                   "Resale" = "Resale",
                                                   "Sub Sale" = "Sub Sale"),
                                       selected = c("New Sale","Resale", "Sub Sale"), 
                                       inline = TRUE, 
                                       status = "primary")
                ),
              box(title = "Past Sales Transactions?", width=2, height=230, status = "warning", solidHeader = TRUE,
                  dateInput(inputId = "saledatefrom",
                            label = "Date (From):",
                            value = '2019-01-01'),
                  dateInput(inputId = "saledateto",
                            label = "Date (To):",
                            value = '2022-02-28')
              ),
              box(title = "Property Type?", width=2, height=230, status = "warning", solidHeader = TRUE,
                  awesomeCheckboxGroup(inputId = "propertytype",
                               label = "Property Type:",
                               choices = c("Condo / Apartment" = "Condo/Apartment",
                                           "Detached House" = "Detached House",
                                           "Executive Condominium" = "Executive Condominium",
                                           "Semi-Detached House" = "Semi-Detached House",
                                           "Terrace House" = "Terrace House"),
                               selected = "Condo/Apartment",
                               inline = TRUE, 
                               status = "primary")
                  
              ))
                
            ),

#################################  Buy > Price Sensitivity TAB  #################################################

tabItem(tabName = "buyprice"),


#################################  Buy > Time Trend TAB  #################################################

tabItem(tabName = "buytimetrend"),

#################################  Rent > Overview TAB ###########################################################

tabItem(tabName = "rentoverview",
        
        fluidRow(
          valueBoxOutput("RentTransactionNum"),
          valueBoxOutput("MedRent"),
          valueBoxOutput("LastRentDate")
        ),
        
        fluidRow(
          box(
            title = "Map - Lease Transactions",width = 6, status = "danger", solidHeader = TRUE,
            collapsible = TRUE,
            tmapOutput("mapPlotRent", height = 430)
          ),
          box(
            title = "Violin Plot - Distribution of Monthly Rent", width = 6, status = "primary", solidHeader = TRUE,
            collapsible = TRUE,
            plotlyOutput("violinPlotRent", height = 430)
          )),
        
        fluidRow(
          box(title = "Which District? What Monthly Rent?", width=3, height = 230, status = "warning", solidHeader = TRUE,
              pickerInput(
                inputId = "postaldistrictrent", 
                label = "Postal District:",
                choices = c("1 - Raffles Place, Cecil, Marina, People's Park" = "1",
                            "2 - Anson, Tanjong Pagar" = "2",
                            "3 - Queenstown, Tiong Bahru" = "3",
                            "4 - Telok Blangah, Harbourfront" = "4",
                            "5 - Pasir Panjang, Hong Leong Garden, Clementi New Town" = "5",
                            "6 - High Street, Beach Road (part)" = "6",
                            "7 - Middle Road, Golden Mile" = "7",
                            "8 - Little India" = "8",
                            "9 - Orchard, Cairnhill, River Valley" = "9",
                            "10 - Ardmore, Bukit Timah, Holland Road, Tanglin" = "10",
                            "11 - Watten Estate, Novena, Thomson" = "11",
                            "12 - Balestier, Toa Payoh, Serangoon" = "12",
                            "13 - Macpherson, Braddell" = "13",
                            "14 - Geylang, Eunos" = "14",
                            "15 - Katong, Joo Chiat, Amber Road" = "15",
                            "16 - Bedok, Upper East Coast, Eastwood, Kew Drive" = "16",
                            "17 - Loyang, Changi" = "17",
                            "18 - Tampines, Pasir Ris" = "18",
                            "19 - Serangoon Garden, Hougang, Punggol" = "19",
                            "20 - Bishan, Ang Mo Kio" = "20",
                            "21 - Upper Bukit Timah, Clementi Park, Ulu Pandan" = "21",
                            "22 - Jurong" = "22",
                            "23 - Hillview, Dairy Farm, Bukit Panjang, Choa Chu Kang" = "23",
                            "24 - Lim Chu Kang, Tengah" = "24",
                            "25 - Kranji, Woodgrove" = "25",
                            "26 - Upper Thomson, Springleaf" = "26",
                            "27 - Yishun, Sembawang" = "27",
                            "28 - Seletar" = "28"
                ),
                selected = c("1","3","5","7","9","13","15","18","19","21","22","24","26","27"),
                options = list(
                  `actions-box` = TRUE,
                  `deselect-all-text` = "Clear All ok?",
                  `select-all-text` = "All districts please!!",
                  `none-selected-text` = "zero",
                  size = 10
                ), 
                multiple = TRUE
              ),
              sliderInput(inputId = "monthlyrent",
                          label = "Monthly Rent ($):",
                          min = 307,
                          max = 60000,
                          value = c(307,4000))),
          box(title = "What Size? Near to MRT?", width=3, height = 230, status = "warning", solidHeader = TRUE,
              selectInput(inputId = "floorarearent",
                          label = "Floor Area (SQM):",
                          choices = c("0 - 60" = "0 - 60",
                                      "61 - 90" = "61 - 90",
                                      "91 - 130" = "91 - 130",
                                      ">130" = ">130"),
                          selected = "91 - 130"),
              sliderInput(inputId = "distancetomrt",
                          label = "Distance to MRT (meter) :",
                          min = 26,
                          max = 3322,
                          value = c(26,600))
          ),
          box(title = "Property Type?", width=2, height = 230, status = "warning", solidHeader = TRUE,
              awesomeCheckboxGroup(inputId = "bedroomno",
                                 label = "No. of Bedrooms:",
                                 choices = c("1" = "1",
                                             "2" = "2",
                                             "3" = "3",
                                             "4 or more" = ">= 4"),
                                 selected = c("1","2","3", ">= 4"),
                                 inline = TRUE, 
                                 status = "primary"),
              
              checkboxGroupInput(inputId = "propertytyperent",
                                 label = "Property Type:",
                                 choices = c("Condo/Apartment" = "Condo/Apartment",
                                             "Executive Condominium " = "Executive Condominium"),
                                 selected = "Condo/Apartment")

          ),
          box(title = "Lease Commencement (From)?", width=2, height = 230, status = "warning", solidHeader = TRUE,
              radioGroupButtons(inputId = "rentyearfrom",
                          label = "Year:",
                          choices = c("2019" = "19",
                                      "2020" = "20",
                                      "2021" = "21",
                                      "2022" = "22"),
                          selected = "21",
                          size = 'xs'),
              selectInput(inputId = "rentmonthfrom",
                          label = "Month:",
                          choices = c("1" = "1",
                                      "2" = "2",
                                      "3" = "3",
                                      "4" = "4",
                                      "5" = "5",
                                      "6" = "6",
                                      "7" = "7",
                                      "8" = "8",
                                      "9" = "9",
                                      "10" = "10",
                                      "11" = "11",
                                      "12" = "12"),
                          selected = "8")
          ),
          box(title = "Lease Commencement (To)?", width=2, height = 230, status = "warning", solidHeader = TRUE,
              radioGroupButtons(inputId = "rentyearto",
                                label = "Year:",
                                choices = c("2019" = "19",
                                            "2020" = "20",
                                            "2021" = "21",
                                            "2022" = "22"),
                                selected = "22",
                                size = 'xs'),
              selectInput(inputId = "rentmonthto",
                          label = "Month:",
                          choices = c("1" = "1",
                                      "2" = "2",
                                      "3" = "3",
                                      "4" = "4",
                                      "5" = "5",
                                      "6" = "6",
                                      "7" = "7",
                                      "8" = "8",
                                      "9" = "9",
                                      "10" = "10",
                                      "11" = "11",
                                      "12" = "12"),
                          selected = "2")
          ))
        
),
        

#################################  Rent > Price Sensitivity TAB #################################################

tabItem(tabName = "rentprice"),

#################################  Rent > Time Trend TAB  #################################################

tabItem(tabName = "renttimetrend")

)
)

  
  

################################################  UI Page  #################################################

ui <- dashboardPage(
  skin = 'yellow', 
  header, 
  sidebar, 
  body)

################################################  Server Page  #################################################

server <- function(input, output){

#######################################  Server > Sales Overview  #################################################
  
  dataset = reactive({
    sales_sf %>%
      filter(`Postal_District` %in% input$postaldistrict) %>%
      filter(`Property_Type_Bin` %in% input$propertytype) %>%
      filter(`Tenure_Bin` %in% input$tenure) %>%
      filter(`Floor_Area_SQM_Bin` %in% input$floorarea) %>%
      filter(`Age` <= input$age[2]) %>%
      filter(`Age` >= input$age[1]) %>%
      filter(`Unit_Price_PSM` >= input$pricepsm[1]) %>%
      filter(`Unit_Price_PSM` <= input$pricepsm[2]) %>%
      filter(`Type_of_Sale` %in% input$typeofsales) %>% 
      filter(`Sale_Date_p` >= input$saledatefrom) %>% 
      filter(`Sale_Date_p` <= input$saledateto)
  })
  
  output$mapPlot <- renderTmap({
    tm_shape(shp = dataset(),
             bbox = st_bbox(sales_sf)) +
      tm_symbols(col = "Type_of_Sale", title.col = "Type of Sale", 
                 palette=c('#FC8D62','#66C2A5','#8DA0CB'), alpha = 0.7, size = 0.1,
                 popup.vars=c("Transaction Date"="Sale_Date", "Transacted Price ($)"="Transacted_Price", "Area (sqm)"="Area_SQM", "Unit Price ($/sqm)"="Unit_Price_PSM")) + 
      tm_basemap("Esri.WorldTopoMap") +
      tm_view(symbol.size.fixed=FALSE) 
  })
  
  output$violinPlot <- renderPlotly({
    p <- ggplot(dataset(), 
                aes_string(x="Type_of_Sale", y="Transacted_Price" , fill ="Type_of_Sale")) +
         geom_violin(alpha = 0.5, show.legend = F) + 
         geom_boxplot(show.legend = F) +
         labs(x = "Type of Sale", y = "Transaction Price ($)") +
         scale_y_continuous(labels = scales::comma) +
         theme_classic() +
         theme(legend.position = 'none',
            text=element_text(size=12,  family="Arial black", color ="#6D9EC1", margin = margin(t = 0, r = 20, b = 0, l = 0)),
            axis.text=element_text(size=9,  family="Arial black", margin = margin(t = 0, r = 20, b = 0, l = 0)))
    
    
     ggplotly(p, tooltip = NULL)
  })
  
  output$BuyTransactionNum <- renderValueBox({
    valueBox(
      prettyNum(nrow(dataset()), big.mark = ","),
      "No. of Sales Transactions",icon = icon("list-ol"), #      "No. of Sales Transactions", paste0(25 + input$count, "%"), icon = icon("list-ol"),
      color = "orange"
    )
  })
  output$MedBuy <- renderValueBox({
    valueBox(
      dollar(median(dataset()$'Transacted_Price')),
      "Median Transacted Price($)", icon = icon("money-bill-alt"),
      color = "orange"
    )
  })
  output$LastBuyDate <- renderValueBox({
    valueBox(
      max(dataset()$`Sale_Date_p`),
      "Last Transaction Date", icon = icon("calendar", lib = "glyphicon"),
      color = "orange"
    )
  })
  
  
########################################  Server > Rent Overview  #################################################
  
  dataset_rent = reactive({
    rent_sf %>%
      filter(`Postal_District` %in% input$postaldistrictrent) %>%
      filter(`Property_Type` %in% input$propertytyperent) %>%
      filter(`Floor_Area_SQM_Bin` %in% input$floorarearent) %>%
      filter(`No_of_Bedroom_Bin` %in% input$bedroomno) %>%
      filter(`Dist_to_MRT_m` <= input$distancetomrt[2]) %>%
      filter(`Dist_to_MRT_m` >= input$distancetomrt[1]) %>%
      filter(`Monthly_Rent` >= input$monthlyrent[1]) %>%
      filter(`Monthly_Rent` <= input$monthlyrent[2]) %>% 
      filter(`Lease_Date` >= anydate(paste("20",input$rentyearfrom,"-", input$rentmonthfrom,"-01", sep=""))) %>% 
      filter(`Lease_Date` <= anydate(paste("20",input$rentyearto,"-", input$rentmonthto,"-01", sep="")))

  })
  
  output$mapPlotRent <- renderTmap({
    tm_shape(shp = dataset_rent(),
             bbox = st_bbox(rent_sf)) +
      tm_symbols(size= 0.1, col = "No_of_Bedroom_Bin", title.col = "No. of Bedrooms", 
                 palette=c('#FC8D62','#66C2A5','#8DA0CB', 'purple'), alpha = 0.7, 
                 popup.vars=c("Lease Commencement Date"="Lease_Commencement_Date", "Monthly Rent ($)"="Monthly_Rent", "Area (sqm)"="Floor_Area_SQM_Bin")) + 
      tm_basemap("Esri.WorldTopoMap") +
      tm_view(symbol.size.fixed=FALSE) 
  })
  
  output$violinPlotRent <- renderPlotly({
    p <- ggplot(dataset_rent(), 
                aes_string(x="No_of_Bedroom_Bin", "Monthly_Rent", fill ="No_of_Bedroom_Bin")) +
      geom_violin(alpha = 0.5, show.legend = F) + 
      geom_boxplot(show.legend = F) +
      labs(x = "No. of Bedrooms", y = "Monthly Rent ($)") +
      scale_y_continuous(labels = scales::comma) +
      theme_classic() +
      theme(legend.position = 'none',
            text=element_text(size=12,  family="Arial black", color ="#6D9EC1", margin = margin(t = 0, r = 20, b = 0, l = 0)),
            axis.text=element_text(size=9,  family="Arial black", margin = margin(t = 0, r = 20, b = 0, l = 0)))
    
    
    ggplotly(p, tooltip = NULL)
  })
  
    
  output$RentTransactionNum <- renderValueBox({
    valueBox(
      prettyNum(nrow(dataset_rent()), big.mark = ","),
      "No. of Lease Transactions",icon = icon("list-ol"), 
      color = "orange"
    )
  })
  output$MedRent <- renderValueBox({
    valueBox(
      dollar(median(dataset_rent()$'Monthly_Rent')),
      "Median Monthly Rent ($)", icon = icon("money-bill-alt"),
      color = "orange"
    )
  })
  
  output$LastRentDate <- renderValueBox({
    valueBox(substring(as.character(max(dataset_rent()$`Lease_Date`)),1,7),
      "Last Lease Commencement Date", icon = icon("calendar", lib = "glyphicon"),
      color = "orange"
    )
  })
  
}
  
shinyApp(ui, server)
  