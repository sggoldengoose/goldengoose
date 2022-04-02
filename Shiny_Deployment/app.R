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
library(ggstatsplot)
library(ggside)
library(ggHoriPlot)
library(data.table)
library(ggthemes)
library(shinycssloaders)


#################################  Load data ######################################################

sales <- readRDS("data/sales_prep.rds")

rent <- readRDS("data/rental_prep.rds")

launch <- read_csv("data/recent_launch.csv")

sales <- sales %>%
  mutate(Sale_Date_DDMMYY = as.Date(Sale_Date, "%d-%b-%y"))

rent <- rent  %>%
  mutate(Rent_Date_DDMMYY = as.Date(paste0("01-", Lease_Commencement_Date), format = "%d-%b-%y"))

#################################  Site Menu Bar  ###########################################################

header <- dashboardHeader(title = tags$a(href='https://singaporegoldengoose.netlify.app/index.html',
                                         tags$img(src='banner.png'), titleWidth = 230))
  
sidebar <- dashboardSidebar(
  sidebarMenu(
  menuItem("Introduction", tabName = "intro", icon = icon("smile")),
  menuItem("Buy", icon = icon("coins")),
  menuItem("  - Overview", tabName = "buyoverview"),
  menuItem("  - Price Sensitivity", tabName = "buyprice"),
  menuItem("  - Time Trend", tabName = "buytimetrend", startExpanded = TRUE,
      menuSubItem('- All Postal Districts', tabName='buyTimeAll'),
      menuSubItem('- Each Postal District', tabName='buyTimeEach')),
  menuItem("Rent", icon = icon("bed")),
  menuItem("  - Overview", tabName = "rentoverview"),
  menuItem("  - Price Sensitivity", tabName = "rentprice"),
  menuItem("  - Time Trend", tabName = "renttimetrend", startExpanded = TRUE,
      menuSubItem('- All Postal Districts', tabName='rentTimeAll'),
      menuSubItem('- Each Postal District', tabName='rentTimeEach')),
  menuItem("Mythbusting", icon = icon("question")),
  menuItem(" - Myth 1", tabName = "myth1"),
  menuItem(" - Myth 2", tabName = "myth2"),
  menuItem(" - Myth 3", tabName = "myth3")
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
                    titlePanel(h2("Singapore Golden Goose: How to spot a Golden Goose (a.k.a.) Profitable Rental Property in Singapore")),
                    fluidRow(
                      infoBox(h6("No. of Sales Transactions"), "82,360", icon=icon("stamp"), color="navy", fill=TRUE, width = 3),
                      infoBox(h6("No. of Lease Transactions"), "298,639", icon=icon("bed"), color="orange", fill=TRUE, width = 3),
                      infoBox(h6("Period Covered"), "Jan 2019 to Feb 2022", icon=icon("calendar"), color="yellow", fill=TRUE, width = 3),
                      infoBox(h6("Data Source"), "URA's REALIS Database", icon=icon("sourcetree"), color="olive", fill=TRUE, width = 3),
                      column(width = 12,
                             h3('Our Application'),
                             p('Using SGGoldenGoose Application, landlords-wannabes can analyse and visualize sales and rental transactions in ease, such that more informed choices about investing in private residential property in Singapore to earn passive rental income can be made.'),
                             h3('Application Features'),
                             p('As seen in the sidebar menu on the left, this application has 4 key modules:'),
                             p('1) Overview: select variables such as postal district (please refer to the postal district map below for referance), size, tenure etc to study price distribution of past transactions in a reactive map and a violin plot.'),
                             p('2) Price Sensitivity: generate insights on how the unit price varies across different property characteristics. Performed statistical tests to check if the effect of each attribute on price is statistically significant.'),
                             p('3) Time Trend: explore how property market reacted to significant COVID-19 events by zooming in specific property types or postal districts.'),
                             p('4) Mythbusting: Pre-conceptions about property market demystified! Are older houses bigger? Are some housing types more accessible than others? Does launch phasing matter when buying new condominiums?'),
                             p(a("User Guide for the Application", href="https://onedrive.live.com/View.aspx?resid=A191734FD72A1486!1897&wdSlideId=256&wdModeSwitchTime=1648919938188&authkey=!AHdUeqqf10fl-Ng"))
                      )
                    )
            ),
            column(width = 12,
                   tags$img(src = 'district.png', style="display: block; margin-left: auto; margin-right: auto; max-height: 500px; max-width: 500px;"))
            
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
                withSpinner(tmapOutput("mapPlot", height = 350), color = "orange")
              ),
              box(
                title = "Violin Plot - Distribution of Sales Transaction Price", width = 6, status = "primary", solidHeader = TRUE,
                collapsible = TRUE,
                withSpinner(plotlyOutput("violinPlot", height = 350), color = "orange")
              )),
            
            fluidRow(
                box(title = "Which District? What Price?", width=3, height=260, status = "warning", solidHeader = TRUE,
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
                                  "28 - Seletar" = "28"),
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
                box(title = "What Size? What Age?", width=3,height=260, status = "warning", solidHeader = TRUE,
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
              box(title = "Tenure? Type of Sales?", width=2, height=260, status = "warning", solidHeader = TRUE,
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
              box(title = "Past Sales Transactions?", width=2, height=260, status = "warning", solidHeader = TRUE,
                  dateInput(inputId = "saledatefrom",
                            label = "Date (From):",
                            value = '2019-01-01'),
                  dateInput(inputId = "saledateto",
                            label = "Date (To):",
                            value = '2022-02-28')
              ),
              box(title = "Property Type?", width=2, height=260, status = "warning", solidHeader = TRUE,
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

    tabItem(tabName = "buyprice",
            
            fluidRow(
              box(title = "Choose Input",
                  width=3,
                  height=580,
                  status="warning",
                  solidHeader = TRUE,
                  radioButtons(
                    inputId="buy_price_var1",
                    label = "Attribute",
                    choices = c("Postal District (no test)" = "Postal_District",
                                "Type of Sale" = "Type_of_Sale",
                                "Size" = "Floor_Area_SQM_Bin",
                                "Property Type" = "Property_Type_Bin",
                                "Age" ="Age",
                                "Tenure" = "Tenure_Bin",
                                "Distance (m) to Nearest MRT" = "Dist_to_MRT_m",
                                "Driving Time (mins) to CBD" = "Time_to_CBD_min"),
                    selected = "Postal_District"),
                  radioButtons(
                    inputId = "buy_price_test",
                    label = "Statistical test",
                    choices = c("Parametric" = "p",
                                "Non-parametric" = "n",
                                "Robust" = "r",
                                "Bayes" = "b"),
                    selected = "r")
              ),
              
              box(title = "Effect of Attribute on Price ($/sqm)",
                  width=9,
                  height=580,
                  status="primary",
                  solidHeader = TRUE,
                  withSpinner(plotOutput(outputId = "buy_price_plot1"), color = "orange")
              )
            ),
            
            fluidRow(
              box(title = "Postal District Reference Map",
                  width=8,
                  status = "danger",
                  collapsible = TRUE,
                  collapsed = TRUE,
                  solidHeader = TRUE,
                  tags$img(src = 'district_names.png', 
                           align = "left",
                           width="100%")
              )
            )
    ),


#################################  Buy > Time Trend TAB  #################################################

    
    tabItem(tabName = "buyTimeAll",
            fluidRow(
              box(title = "What type of Property?", width=2, status = "warning", solidHeader = TRUE,
                  radioButtons(inputId = "buyTime_propertyType",
                               label = "Property Type:",
                               choices = c("All" = "All",
                                           "Condo/Apartment" = "Condo/Apartment",
                                           "Executive Condo" = "Executive Condominium",
                                           "Terrace House" = "Terrace House",
                                           "Semi-Detached House" = "Semi-Detached House",
                                           "Detached House" = "Detached House"),
                               selected = "All"),
                  radioButtons(inputId = "buyTime_typeofSale",
                               label = "Type of Sale:",
                               choices = c("All" = "All",
                                           "New Sale" = "New Sale",
                                           "Resale" = "Resale",
                                           "Sub Sale" = "Sub Sale"),
                               selected = "All")
              ),
              box(title = "Horizon Plot of Month-to-Month Change in Median Sales Unit Price ($/sqm)", width=10, height=680, status = "primary", solidHeader = TRUE,
                  withSpinner(plotOutput(outputId = "hist_Sales_Plot"), color = "orange")
              )
            ),
            fluidRow(
              box(title = "Postal District Reference Map",
                  width=8,
                  status = "danger",
                  collapsible = TRUE,
                  collapsed = TRUE,
                  solidHeader = TRUE,
                  tags$img(src = 'district_names.png', 
                           align = "left",
                           width="100%")
              )
            ),
            fluidRow(
              box(title = "Covid Timeline Reference",
                  width=8,
                  status = "danger",
                  collapsible = TRUE,
                  collapsed = TRUE,
                  solidHeader = TRUE,
                  tags$img(src = 'covid_timeline.jpg', 
                           align = "left",
                           width="50%")
              )
            )
    ),
    
    tabItem(tabName = "buyTimeEach",
            fluidRow(
              box(title = "Which District?", width=2,status = "warning", solidHeader = TRUE,
                  selectInput(inputId = "buyTimeEach_postalDesc",
                              label = "Postal District:",
                              choices = c("1 - Raffles Place, Cecil, Marina, People's Park",
                                          "2 - Anson, Tanjong Pagar",
                                          "3 - Queenstown, Tiong Bahru",
                                          "4 - Telok Blangah, Harbourfront",
                                          "5 - Pasir Panjang, Hong Leong Garden, Clementi New Town",
                                          "6 - High Street, Beach Road (part)",
                                          "7 - Middle Road, Golden Mile",
                                          "8 - Little India",
                                          "9 - Orchard, Cairnhill, River Valley",
                                          "10 - Ardmore, Bukit Timah, Holland Road, Tanglin",
                                          "11 - Watten Estate, Novena, Thomson",
                                          "12 - Balestier, Toa Payoh, Serangoon",
                                          "13 - Macpherson, Braddell",
                                          "14 - Geylang, Eunos",
                                          "15 - Katong, Joo Chiat, Amber Road",
                                          "16 - Bedok, Upper East Coast, Eastwood, Kew Drive",
                                          "17 - Loyang, Changi",
                                          "18 - Tampines, Pasir Ris",
                                          "19 - Serangoon Garden, Hougang, Punggol",
                                          "20 - Bishan, Ang Mo Kio",
                                          "21 - Upper Bukit Timah, Clementi Park, Ulu Pandan",
                                          "22 - Jurong",
                                          "23 - Hillview, Dairy Farm, Bukit Panjang, Choa Chu Kang",
                                          "24 - Lim Chu Kang, Tengah",
                                          "25 - Kranji, Woodgrove",
                                          "26 - Upper Thomson, Springleaf",
                                          "27 - Yishun, Sembawang",
                                          "28 - Seletar"),
                              selected = "1 - Raffles Place, Cecil, Marina, People's Park"),
                  # title = "What type of Property?", width=2, status = "warning", solidHeader = TRUE,
                  radioButtons(inputId = "buyTimeEach_propertyType",
                               label = "Property Type:",
                               choices = c("All" = "All",
                                           "Condo/Apartment" = "Condo/Apartment",
                                           "Executive Condo" = "Executive Condominium",
                                           "Terrace House" = "Terrace House",
                                           "Semi-Detached House" = "Semi-Detached House",
                                           "Detached House" = "Detached House")),
                  radioButtons(inputId = "buyTimeEach_TypeofSale",
                               label = "Type of Sale:",
                               choices = c("All" = "All",
                                           "New Sale" = "New Sale",
                                           "Resale" = "Resale",
                                           "Sub Sale" = "Sub Sale"))
              ),
              box(title = "Time Series Box Plot of Monthly Change in Sales Unit Price ($/sqm)", width=10, status = "primary", solidHeader = TRUE,
                  withSpinner(plotlyOutput(outputId = "tsBoxplot_Sales_Plot"), color = "orange")
              )
            )
    ),


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
                withSpinner(tmapOutput("mapPlotRent", height = 350), color = "orange")
              ),
              box(
                title = "Violin Plot - Distribution of Monthly Rent", width = 6, status = "primary", solidHeader = TRUE,
                collapsible = TRUE,
                withSpinner(plotlyOutput("violinPlotRent", height = 350), color = "orange")
              )),
            
            fluidRow(
              box(title = "Which District? What Monthly Rent?", width=3, height = 250, status = "warning", solidHeader = TRUE,
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
              box(title = "What Size? Near to MRT?", width=3, height = 250, status = "warning", solidHeader = TRUE,
                  selectInput(inputId = "floorarearent",
                              label = "Floor Area (sqm):",
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
              box(title = "Property Type?", width=2, height = 250, status = "warning", solidHeader = TRUE,
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
              box(title = "Lease Commencement (From)?", width=2, height = 250, status = "warning", solidHeader = TRUE,
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
              box(title = "Lease Commencement (To)?", width=2, height = 250, status = "warning", solidHeader = TRUE,
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

    tabItem(tabName = "rentprice",
            
            fluidRow(
              box(title = "Choose Input",
                  status="warning",
                  width=3,
                  height=580,
                  solidHeader = TRUE,
                  radioButtons(
                    inputId="rent_price_var1",
                    label = "Attribute",
                    choices = c("Postal District (no test)" = "Postal_District",
                                "Property Type" = "Property_Type",
                                "Size" = "Floor_Area_SQM_Bin",
                                "No. of Bedrooms" = "No_of_Bedroom_Bin",
                                "Distance (m) to Nearest MRT" = "Dist_to_MRT_m",
                                "Driving Time (mins) to CBD" = "Time_to_CBD_min"),
                    selected = "Postal_District"),
                  radioButtons(
                    inputId = "rent_price_test",
                    label = "Statistical test",
                    choices = c("Parametric" = "p",
                                "Non-parametric" = "n",
                                "Robust" = "r",
                                "Bayes" = "b"),
                    selected = "r")
              ),
              box(title = "Effect of Attribute on Rental Unit Price ($/sqm)",
                  width=9,
                  height = 580,
                  status="primary",
                  solidHeader = TRUE,
                  withSpinner(plotOutput(outputId = "rent_price_plot1"), color = "orange")
              )
            ),
            
            fluidRow(
              box(title = "Postal District Reference Map",
                  width=8,
                  status = "danger",
                  collapsible = TRUE,
                  collapsed = TRUE,
                  solidHeader = TRUE,
                  tags$img(src = 'district_names.png', 
                           align = "left",
                           width="100%")
              )
            )
    ),

#################################  Rent > Time Trend TAB  #################################################

    tabItem(tabName = "rentTimeAll",
            fluidRow(
              box(title = "What type of Property?", width=2, status = "warning", solidHeader = TRUE,
                  radioButtons(inputId = "rentTime_propertyType",
                               label = "Property Type:",
                               choices = c("All" = "All",
                                           "Condo/Apartment" = "Condo/Apartment",
                                           "Executive Condo" = "Executive Condominium",
                                           "Terrace House" = "Terrace House",
                                           "Semi-Detached House" = "Semi-Detached House",
                                           "Detached House" = "Detached House"))
              ),
              box(title = "Horizon Plot of Month-to-Month Change in Rental Unit Price ($/sqm)", width=10, height=680, status = "primary", solidHeader = TRUE,
                  withSpinner(plotOutput(outputId = "hist_Rent_Plot"), color="orange")
              )
            ),
            fluidRow(
              box(title = "Postal District Reference Map",
                  width=8,
                  status = "danger",
                  collapsible = TRUE,
                  collapsed = TRUE,
                  solidHeader = TRUE,
                  tags$img(src = 'district_names.png', 
                           align = "left",
                           width="100%")
              )
            ),
            fluidRow(
              box(title = "Covid Timeline Reference",
                  width=8,
                  status = "danger",
                  collapsible = TRUE,
                  collapsed = TRUE,
                  solidHeader = TRUE,
                  tags$img(src = 'covid_timeline.jpg', 
                           align = "left",
                           width="50%")
              )
            )
    ),
    
    tabItem(tabName = "rentTimeEach",
            fluidRow(
              box(title = "Which District?", width=2,status = "warning", solidHeader = TRUE,
                  selectInput(inputId = "rentTimeEach_postalDesc",
                              label = "Postal District:",
                              choices = c("1 - Raffles Place, Cecil, Marina, People's Park",
                                          "2 - Anson, Tanjong Pagar",
                                          "3 - Queenstown, Tiong Bahru",
                                          "4 - Telok Blangah, Harbourfront",
                                          "5 - Pasir Panjang, Hong Leong Garden, Clementi New Town",
                                          "6 - High Street, Beach Road (part)",
                                          "7 - Middle Road, Golden Mile",
                                          "8 - Little India",
                                          "9 - Orchard, Cairnhill, River Valley",
                                          "10 - Ardmore, Bukit Timah, Holland Road, Tanglin",
                                          "11 - Watten Estate, Novena, Thomson",
                                          "12 - Balestier, Toa Payoh, Serangoon",
                                          "13 - Macpherson, Braddell",
                                          "14 - Geylang, Eunos",
                                          "15 - Katong, Joo Chiat, Amber Road",
                                          "16 - Bedok, Upper East Coast, Eastwood, Kew Drive",
                                          "17 - Loyang, Changi",
                                          "18 - Tampines, Pasir Ris",
                                          "19 - Serangoon Garden, Hougang, Punggol",
                                          "20 - Bishan, Ang Mo Kio",
                                          "21 - Upper Bukit Timah, Clementi Park, Ulu Pandan",
                                          "22 - Jurong",
                                          "23 - Hillview, Dairy Farm, Bukit Panjang, Choa Chu Kang",
                                          "24 - Lim Chu Kang, Tengah",
                                          "25 - Kranji, Woodgrove",
                                          "26 - Upper Thomson, Springleaf",
                                          "27 - Yishun, Sembawang",
                                          "28 - Seletar"),
                  selected = "1 - Raffles Place, Cecil, Marina, People's Park"),
                  # title = "What type of Property?", width=2, status = "warning", solidHeader = TRUE,
                  radioButtons(inputId = "rentTimeEach_propertyType",
                               label = "Property Type:",
                               choices = c("All" = "All",
                                           "Condo/Apartment" = "Condo/Apartment",
                                           "Executive Condo" = "Executive Condominium",
                                           "Terrace House" = "Terrace House",
                                           "Semi-Detached House" = "Semi-Detached House",
                                           "Detached House" = "Detached House"))
              ),
              box(title = "Time Series Box Plot of Monthly Change in Rental Unit Price ($/sqm)", width=10, status = "primary", solidHeader = TRUE,
                  withSpinner(plotlyOutput(outputId = "tsBoxplot_Rent_Plot"), color = "orange")
              )
            )
    ),

#################################  Mythbusting > Myth1 TAB  #################################################

    tabItem(tabName = "myth1",
            h3("Are older houses bigger?"),
            fluidRow(
              column(width=5,
                     box(title = "Select Input",
                         solidHeader=TRUE,
                         status = "warning",
                         width=NULL,
                         radioButtons(inputId = "myth1_var1",
                                      label = "Choose housing type",
                                      choices = c("Condo/Apartment"="Condo/Apartment",
                                                  "Executive Condo" = "Executive Condominium",
                                                  "Terrace House" = "Terrace House",
                                                  "Semi-Detached House" = "Semi-Detached House",
                                                  "Detached House" = "Detached House"),
                                      selected = "Condo/Apartment"),
                         radioButtons(
                           inputId = "myth1_test",
                           label = "Statistical test",
                           choices = c("Parametric" = "p",
                                       "Non-parametric" = "n",
                                       "Robust" = "r",
                                       "Bayes" = "b"),
                           selected = "r")
                     ),
                     box(title = "Key Observations",
                         solidHeader=TRUE,
                         status = "danger",
                         width=NULL,
                         p("Many say that older houses are bigger in Singapore. It was also reported in 2021 that more are buying older properties because they are bigger.", 
                           a("(CNA Article)", href="https://www.channelnewsasia.com/business/singapore-wfh-bigger-space-new-homes-hdb-condo-sales-1338181")),
                         p("The average size of houses showed a clearly decreasing trend over time, except for semi-detached houses and terrace houses. 
                           For condominiums/apartments, the average size of uncompleted houses sold were about half the size of those built before 1990. 
                           The first few executive condominium launches in the 1990s were about 25% larger than those still under construction."),
                         p("It should be noted that this is only based on houses transacted in the past 3 years and does not cover all private houses that have been built, 
                                        so it may not be truly representative of the population."))
              ),
              column(width=7,
                     box(title = "Floor Area against Completion Year",
                         height = 760,
                         width=NULL,
                         solidHeader = TRUE,
                         status = "primary",
                         withSpinner(plotOutput(outputId = "myth1_plot"), color = "orange")
                     )
              )
            )
    ),

#################################  Mythbusting > Myth2 TAB  #################################################

    tabItem(tabName = "myth2",
            h3("Are some houses more accessible than others?"),
            fluidRow(
              column(width=5, 
                     box(title = "Select Input",
                         solidHeader=TRUE,
                         status = "warning",
                         width=NULL,
                         radioButtons(inputId = "myth2_var1",
                                      label = "Choose type of access",
                                      choices = c("MRT Access" = "Dist_to_MRT_m",
                                                  "Driving Time to CBD" = "Time_to_CBD_min")
                         ),
                         radioButtons(
                           inputId = "myth2_test",
                           label = "Statistical test",
                           choices = c("Parametric" = "p",
                                       "Non-parametric" = "n",
                                       "Robust" = "r",
                                       "Bayes" = "b"),
                           selected = "r")
                     ),
                     box(title = "Key Observations",
                         solidHeader=TRUE,
                         status = "danger",
                         width=NULL,
                         htmlOutput(outputId = "myth2_text")
                     )
              ),
              column(width=7,
                     box(title = "Accesibility Measures by Housing Type",
                         width=NULL,
                         solidHeader = TRUE,
                         status = "primary",
                         height=750,
                         withSpinner(plotOutput(outputId = "myth2_plot",
                                    height=430), color = "orange")
                     )
              )
            ),
    ),

#################################  Mythbusting > Myth3 TAB  #################################################

    tabItem(tabName = "myth3",
            h3("Does pricing of new project launch changes overtime?"),
            fluidRow(
              column(width=3, 
                     box(title = "Which Project Launch?",
                         solidHeader=TRUE,
                         status = "warning",
                         width=NULL,
                         selectInput(inputId = "buyRecentLaunch_projName",
                                     label = "Project Name:",
                                     choices = launch$Project_Name)
                         ),
                       box(title = "Key Observations",
                           solidHeader=TRUE,
                           status = "danger",
                           width=NULL,
                           htmlOutput(outputId = "myth3_text")
                     )
              ),
              column(width=9,
                     box(title = "Quarterly Change in Sales Price of Newly Launched Projects by Project Name",
                         width=NULL,
                         solidHeader = TRUE,
                         status = "primary",
                         height=800,
                         withSpinner(plotlyOutput(outputId = "myth3_plot"), color = "orange")
                     )
              )
            )
      )
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
  
  sales_sf <- sales %>%
    mutate("Sale_Date_p" = format((as.Date(Sale_Date, "%d-%b-%y")), format = "%Y-%m-%d")) %>%
    st_as_sf(coords = c("longitude","latitude")) 
  
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
                 popup.vars=c("Transaction Date"="Sale_Date", 
                              "Transacted Price ($)"="N_Transacted_Price", 
                              "Area (sqm)"="N_Area_SQM", 
                              "Unit Price ($/sqm)"="Unit_Price_PSM")) + 
      tm_basemap("Esri.WorldTopoMap") +
      tm_view(symbol.size.fixed=FALSE) 
  })
  
  output$violinPlot <- renderPlotly({
    p <- ggplot(dataset(), 
                aes_string(x="Type_of_Sale", 
                           y="N_Transacted_Price" , 
                           fill ="Type_of_Sale")) +
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
      dollar(median(dataset()$'N_Transacted_Price')),
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

######################################## Server > Buy Price Sensitivity ##########################################
  
  output$buy_price_plot1 <- renderPlot(
    height= 500,
    res=90,
    {
      
      cat <- c("Type_of_Sale", "Floor_Area_SQM_Bin", "Property_Type_Bin", "Tenure_Bin")
      cont <- c("Age", "Dist_to_MRT_m", "Time_to_CBD_min")
      
      clean_names <- function(var_name){
        og <- c("Postal_District", "Type_of_Sale", "Floor_Area_SQM_Bin", "Property_Type_Bin", "Tenure_Bin", "Age", "Dist_to_MRT_m", "Time_to_CBD_min")
        clean <- c("Postal District", "Type of Sale","Floor Area (sqm)", "Property Type", "Tenure", "Age", "Distance to MRT (m)", "Driving Time to CBD (min)")
        
        return(clean[match(var_name, og)])
      }
      
      if (input$buy_price_var1 =="Postal_District"){
        
        p1 <- ggplot(data =sales,
                     aes_string(x=input$buy_price_var1, y="Unit_Price_PSM")) +
          geom_violin(fill = "#8DA0CB")+
          geom_boxplot(width=0.1) +
          scale_y_continuous(labels = scales::comma) +
          labs(title = paste("Violin plot of Unit Price ($/sqm) against", clean_names(input$buy_price_var1)),
               x = clean_names(input$buy_price_var1),
               y = "Unit Price ($/sqm)") +
          theme_bw()+
          theme(plot.title = element_text(size = rel(1.5), face = "bold"),
                plot.subtitle = element_text(size = rel(1.3)),
                text=element_text(size=12, face="bold", color ="#6D9EC1", margin = margin(t = 0, r = 20, b = 0, l = 0)),
                axis.text=element_text(size=10, margin = margin(t = 0, r = 20, b = 0, l = 0)))
      }
      
      if (input$buy_price_var1 %in% cat){
        
        p1 <- ggbetweenstats(data = sales,
                             x = !!input$buy_price_var1,
                             y = Unit_Price_PSM,
                             type = input$buy_price_test,
                             pairwise.display = "significant",
                             point.args = list(alpha=0),
                             violin.args = list(fill = "#6D9EC1", alpha = 0.5),
                             xlab = clean_names(input$buy_price_var1),
                             ylab = "Unit Price ($/sqm)",
                             title = paste("Violin Plot of Unit Price ($/sqm) against", clean_names(input$buy_price_var1))) +
          theme_bw()+
          theme(legend.position ="none",
                plot.title = element_text(size = rel(1.5), face = "bold"),
                plot.subtitle = element_text(size = rel(1.3)),
                text=element_text(size=12, face="bold", color ="#6D9EC1", margin = margin(t = 0, r = 20, b = 0, l = 0)),
                axis.text=element_text(size=10, margin = margin(t = 0, r = 20, b = 0, l = 0)))
        
      }
      
      if (input$buy_price_var1 %in% cont){
        p1 <- ggscatterstats(data=sales,
                             x = !!input$buy_price_var1,
                             y = Unit_Price_PSM,
                             type = input$buy_price_test,
                             marginal = TRUE,
                             xlab = clean_names(input$buy_price_var1),
                             ylab = "Unit Price ($/sqm)",
                             title = paste("Scatter Plot of Unit Price ($/sqm) against", clean_names(input$buy_price_var1))) +
          theme_bw()+
          theme(plot.title = element_text(size = rel(1.5), face = "bold"),
                plot.subtitle = element_text(size = rel(1.3)),
                text=element_text(size=12, face="bold", color ="#6D9EC1", margin = margin(t = 0, r = 20, b = 0, l = 0)),
                axis.text=element_text(size=10, margin = margin(t = 0, r = 20, b = 0, l = 0)))
      }
      
      p1
      
    })
  
########################################  Server > Buy Time Trend #####################################################
  
  output$hist_Sales_Plot <- renderPlot(
    #width=900,
    height=600,
    res = 80,
    {
      sales_selected = sales
      
      if (input$buyTime_propertyType != "All") {
        sales_selected = sales_selected %>%
          filter(Property_Type_Bin == input$buyTime_propertyType) 
      }
      
      if (input$buyTime_typeofSale != "All") {
        sales_selected = sales_selected %>%
          filter(Type_of_Sale == input$buyTime_typeofSale) 
      }
      
      if (nrow(sales_selected) > 0) {
        ### Dates of Different Tightened Measures Phases in Singapore
        data_events_dates <- data.frame(date = c("20/04/07","20/06/02","20/06/19","20/12/28","21/05/08","21/07/22","21/09/27","21/11/22"))
        
        data_events_dates$date <- as.Date(data_events_dates$date , format = "%y/%m/%d")
        data_events_dates 
        
        ### Dates of Detection of First Case of Covid-19 Variants in Singapore
        data_events_key <- data.frame(date = c("20/01/23", "21/04/01","21/12/06"))
        
        data_events_key$date <- as.Date(data_events_key$date , format = "%y/%m/%d")
        data_events_key 
        
        
        ### Dates of Key Events with Text and Facet Grid to Place the Label
        data_events <- data.frame(date = c("20/01/23","20/04/07","20/06/02","20/06/19","20/12/28","21/04/01","21/05/08","21/07/22","21/09/27","21/11/22","21/12/06"),
                                  name = c("1st Alpha", "Circuit Breaker", "Phase 1", "Phase 2", "Phase 3", "1st Delta", "Phase 3HA","Phase 2HA", "Stabilisation", "Transition","1st Omicron"),
                                  Postal_District=c("1","2","1","2","1","2","1","2","1","2","1"))
        
        data_events$date <- as.Date(data_events$date , format = "%y/%m/%d")
        data_events$Postal_District <- factor(data_events$Postal_District, levels = unique(data_events$Postal_District))
        
        monthly_sales <- sales_selected %>%
          mutate(Sale_Date_MMYY = floor_date(sales_selected$Sale_Date_DDMMYY,  # Create year-month column
                                             "month")) %>%
          group_by(Sale_Date_MMYY, Postal_District) %>% 
          summarise(monthly_median = median(Unit_Price_PSM)) %>%
          ungroup() %>%
          arrange(Postal_District, Sale_Date_MMYY) %>%
          group_by(Postal_District) %>%
          mutate(monthly_change = (monthly_median-lag(monthly_median))/lag(monthly_median)*100) %>%
          ungroup()
        
        monthly_sales <- monthly_sales[!(is.na(monthly_sales$monthly_change)), ]
        
        if (nrow(monthly_sales) > 1) {
          cutpoints <- monthly_sales  %>% 
            mutate(
              outlier = between(
                monthly_change, 
                quantile(monthly_change, 0.25, na.rm=T)-
                  1.5*IQR(monthly_change, na.rm=T),
                quantile(monthly_change, 0.75, na.rm=T)+
                  1.5*IQR(monthly_change, na.rm=T))) %>% 
            filter(outlier)
          
          ori <- sum(range(cutpoints$monthly_change))/2
          sca <- seq(range(cutpoints$monthly_change)[1], 
                     range(cutpoints$monthly_change)[2], 
                     length.out = 7)[-4]
          
          p <- monthly_sales %>% ggplot() +
            geom_horizon(aes(Sale_Date_MMYY, 
                             monthly_change,
                             fill = ..Cutpoints..), 
                         origin = 0, horizonscale = sca) +
            scale_fill_hcl(palette = 'RdBu') +
            facet_grid(Postal_District ~.) +
            theme_few() +
            theme(
              panel.spacing.y=unit(0, "lines"),
              strip.text.y = element_text(size=10,face="bold", angle = 0, hjust = 0),
              axis.text.x =element_text(size=8, face="bold", hjust=-0.1),
              axis.text.y = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.ticks.y = element_blank(),
              panel.border = element_blank()
            )  +
            scale_x_date(expand=c(0,0), 
                         date_breaks = "3 months", 
                         date_labels = "%b-%y") +
            # remove legend
            #theme(legend.position = "none") +
            geom_vline(data = data_events_dates, aes(xintercept = date), color = "orange", size=1) +
            geom_vline(data = data_events_key, aes(xintercept = date), color = "red", size=1) +
            guides(fill=guide_legend(title="% Change from Previous Month"))
          
          p +
            geom_label(data = data_events, aes(x = date, label = name, y=0.6), color = "black", size=3, fontface=2, fill="white")
        } else {
          p <- ggplot() +
            ggtitle(paste0("Insufficient Sales Data Change for (",input$buyTime_propertyType, ")"))
          p
        }
      } else {
        p <- ggplot() +
          ggtitle(paste0("There were no sales transactions for (",input$buyTime_propertyType, ") - ", input$buyTime_TypeofSale))
        p
      }
    }
  )
  
  output$tsBoxplot_Sales_Plot <- renderPlotly(
    {
      
      sales_selected = sales
      
      if (input$buyTimeEach_propertyType != "All") {
        sales_selected = sales_selected %>%
          filter(Postal_Desc == input$buyTimeEach_postalDesc & Property_Type_Bin == input$buyTimeEach_propertyType) 
      } else {
        sales_selected = sales_selected %>%
          filter(Postal_Desc == input$buyTimeEach_postalDesc)
      }
      
      if (input$buyTimeEach_TypeofSale != "All") {
        sales_selected = sales_selected %>%
          filter(Type_of_Sale == input$buyTimeEach_TypeofSale) 
      }
      
      if (nrow(sales_selected) > 0) {
        sales_selected$year_month <- floor_date(sales_selected$Sale_Date_DDMMYY,  # Create year-month column
                                                "month")
        
        p <- ggplot(sales_selected, aes(x=reorder(format(year_month,'%b%y'),year_month), y=Unit_Price_PSM)) + 
          geom_boxplot(
            # custom boxes
            color="skyblue",
            fill="skyblue",
            alpha=0.2,
            
            # custom outliers
            outtlier.colour="red",
            outlier.fill="red",
            outlier.size=3
          ) +
          theme_few() + 
          theme(text=element_text(size=12, face="bold", color ="#6D9EC1"),
                axis.text=element_text(size=6, face="bold"),
                plot.title = element_text(size = 12)) +
          xlab("Month of Sale") +
          ylab("Unit Price ($/sqm)") +
          ggtitle(paste0("Sales Unit Price ($/sqm) in Postal District ",input$buyTimeEach_postalDesc, " (",input$buyTimeEach_propertyType, ") - ", input$buyTimeEach_TypeofSale)) +
          stat_summary(
            fun = median,
            geom = 'line',
            aes(group = 1),
            position = position_dodge(width = 0.9) 
          )
        
        output = ggplotly(p,tooltip="text")
        
        # overrides black outline of outliers
        output$x$data[[1]]$marker$line$color = "red"
        # overrides black extreme outlier color
        output$x$data[[1]]$marker$outliercolor = "red"
        # overrides black not as extreme outlier color
        output$x$data[[1]]$marker$color = "red"
        
        output
      } else {
        p <- ggplot() +
          ggtitle(paste0("There were no sales transactions for Postal District ",input$buyTimeEach_postalDesc, " (",input$buyTimeEach_propertyType, ") - ", input$buyTimeEach_TypeofSale))
        p
      }
    })
  
########################################  Server > Rent Overview  #################################################
  
  rent_sf <- rent %>%
    mutate(No_of_Bedroom_Bin = factor(No_of_Bedroom_Bin, levels = c("1", "2", "3", ">= 4"))) %>%
    mutate(Lease_month = substring(Lease_Commencement_Date,1,3)) %>%
    mutate(Lease_month = match(Lease_month, month.abb)) %>%
    mutate(Lease_year = as.numeric(substring(Lease_Commencement_Date,5,6))) %>%
    mutate(Lease_Date = paste0('01-',Lease_Commencement_Date)) %>%
    mutate(Lease_Date = anydate(as.Date(Lease_Date, "%d-%b-%y"))) %>%
    st_as_sf(coords = c("longitude","latitude")) 
  
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
                 popup.vars=c("Lease Commencement Date"="Lease_Commencement_Date", 
                              "Monthly Rent ($)"="Monthly_Rent", 
                              "Area (sqm)"="Floor_Area_SQM_Bin")) + 
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

  
########################################  Server > Rent Price Sensitivity  #################################################

  output$rent_price_plot1 <- renderPlot(
    height= 500,
    res=90,{
    cat <- c("Floor_Area_SQM_Bin", "No_of_Bedroom_Bin", "Property_Type")
    cont <- c("Age", "Dist_to_MRT_m", "Time_to_CBD_min")
    
    clean_names <- function(var_name){
      og <- c("Postal_District", "Floor_Area_SQM_Bin", "No_of_Bedroom_Bin", "Property_Type", "Dist_to_MRT_m", "Time_to_CBD_min")
      clean <- c("Postal District", "Floor Area (sqm)", "No of Bedrooms", "Property Type", "Distance to MRT (m)", "Driving Time to CBD (min)")
      
      return(clean[match(var_name, og)])
    }
    
    if (input$rent_price_var1 == "Postal_District"){
      p5 <- ggplot(data =rent,
                   aes_string(x=input$rent_price_var1, y="Unit_Price_PSM")) +
        geom_violin(fill = "#8DA0CB")+
        geom_boxplot(width=0.1) +
        scale_y_continuous(labels = scales::comma) +
        labs(title = paste("Violin plot of Rental Unit Price ($/sqm) against", clean_names(input$rent_price_var1)),
             x = clean_names(input$rent_price_var1),
             y = "Unit Price ($/sqm)") +
        theme_bw()+
        theme(plot.title = element_text(size = rel(1.5), face = "bold"),
              plot.subtitle = element_text(size = rel(1.3)),
              text=element_text(size=12, face="bold", color ="#6D9EC1", margin = margin(t = 0, r = 20, b = 0, l = 0)),
              axis.text=element_text(size=10, face="bold", margin = margin(t = 0, r = 20, b = 0, l = 0)))
    }
    
    if (input$rent_price_var1 %in% cat){
      p5 <- ggbetweenstats(data = rent,
                           x = !!input$rent_price_var1,
                           y = Unit_Price_PSM,
                           type = input$rent_price_test,
                           pairwise.display = "significant",
                           point.args = list(alpha=0),
                           violin.args = list(fill = "#6D9EC1", alpha = 0.5),
                           xlab = clean_names(input$rent_price_var1),
                           ylab = "Unit Price ($/sqm)",
                           title = paste("Violin plot of Rental Unit Price ($/sqm) against", clean_names(input$rent_price_var1))) +
        theme_bw()+
        theme(legend.position ="none",
              plot.title = element_text(size = rel(1.5), face = "bold"),
              plot.subtitle = element_text(size = rel(1.3)),
              text=element_text(size=12, face="bold", color ="#6D9EC1", margin = margin(t = 0, r = 20, b = 0, l = 0)),
              axis.text=element_text(size=10, face="bold", margin = margin(t = 0, r = 20, b = 0, l = 0)))
    }
    
    if (input$rent_price_var1 %in% cont){
      p5 <- ggscatterstats(data=rent,
                           x = !!input$rent_price_var1,
                           y = Unit_Price_PSM,
                           type = input$rent_price_test,
                           marginal = TRUE,
                           xlab = clean_names(input$rent_price_var1),
                           ylab = "Unit Price ($/sqm)",
                           title = paste("Scatter Plot of Rental Unit Price ($/sqm) against", clean_names(input$rent_price_var1))) +
        theme_bw()+
        theme(plot.title = element_text(size = rel(1.5), face = "bold"),
              plot.subtitle = element_text(size = rel(1.3)),
              text=element_text(size=12, face="bold", color ="#6D9EC1", margin = margin(t = 0, r = 20, b = 0, l = 0)),
              axis.text=element_text(size=10, face="bold", margin = margin(t = 0, r = 20, b = 0, l = 0)))
    }
    
    p5
    
  }) 
  
########################################  Server > Rent Time Trend  ####################################################  

  output$hist_Rent_Plot <- renderPlot(
    #  width=900,
    height=600,
    res = 80,
    {
      
      rent_selected = rent
      
      if (input$rentTime_propertyType != "All") {
        rent_selected = rent_selected %>%
          filter(Property_Type == input$rentTime_propertyType) 
      }
      
      if (nrow(rent_selected) > 0) { 
        ### Dates of Different Tightened Measures Phases in Singapore
        data_events_dates <- data.frame(date = c("20/04/07","20/06/02","20/06/19","20/12/28","21/05/08","21/07/22","21/09/27","21/11/22"))
        
        data_events_dates$date <- as.Date(data_events_dates$date , format = "%y/%m/%d")
        data_events_dates 
        
        ### Dates of Detection of First Case of Covid-19 Variants in Singapore
        data_events_key <- data.frame(date = c("20/01/23", "21/04/01","21/12/06"))
        
        data_events_key$date <- as.Date(data_events_key$date , format = "%y/%m/%d")
        data_events_key 
        
        
        ### Dates of Key Events with Text and Facet Grid to Place the Label
        data_events <- data.frame(date = c("20/01/23","20/04/07","20/06/02","20/06/19","20/12/28","21/04/01","21/05/08","21/07/22","21/09/27","21/11/22","21/12/06"),
                                  name = c("1st Alpha", "Circuit Breaker", "Phase 1", "Phase 2", "Phase 3", "1st Delta", "Phase 3HA","Phase 2HA", "Stabilisation", "Transition","1st Omicron"),
                                  Postal_District=c("1","2","1","2","1","2","1","2","1","2","1"))
        
        data_events$date <- as.Date(data_events$date , format = "%y/%m/%d")
        data_events$Postal_District <- factor(data_events$Postal_District, levels = unique(data_events$Postal_District))
        
        #rent_selected <- rent_selected[!(is.na(rent_selected$Postal_District)), ]
        
        
        monthly_rent <- rent_selected %>%
          mutate(Rent_Date_MMYY = floor_date(rent_selected$Rent_Date_DDMMYY,  # Create year-month column
                                             "month")) %>%
          group_by(Rent_Date_MMYY, Postal_District) %>% 
          summarise(monthly_median = median(Unit_Price_PSM)) %>%
          ungroup() %>%
          arrange(Postal_District, Rent_Date_MMYY) %>%
          group_by(Postal_District) %>%
          mutate(monthly_change = (monthly_median-lag(monthly_median))/lag(monthly_median)*100) %>%
          ungroup()
        
        monthly_rent <- monthly_rent[!(is.na(monthly_rent$monthly_change)), ]
        
        if (nrow(monthly_rent) > 1) {
          cutpoints <- monthly_rent  %>% 
            mutate(
              outlier = between(
                monthly_change, 
                quantile(monthly_change, 0.25, na.rm=T)-
                  1.5*IQR(monthly_change, na.rm=T),
                quantile(monthly_change, 0.75, na.rm=T)+
                  1.5*IQR(monthly_change, na.rm=T))) %>% 
            filter(outlier)
          
          ori <- sum(range(cutpoints$monthly_change))/2
          sca <- seq(range(cutpoints$monthly_change)[1], 
                     range(cutpoints$monthly_change)[2], 
                     length.out = 7)[-4]
          
          
          p <- monthly_rent %>% ggplot() +
            geom_horizon(aes(Rent_Date_MMYY, 
                             monthly_change, 
                             fill = ..Cutpoints..), 
                         origin = 0, horizonscale = sca) +
            scale_fill_hcl(palette = 'RdBu') +
            facet_grid(Postal_District ~.) +
            theme_few() +
            theme(
              panel.spacing.y=unit(0, "lines"),
              strip.text.y = element_text(size=10,face="bold", angle = 0, hjust = 0),
              axis.text.x =element_text(size=8, face="bold", hjust=-0.1),
              axis.text.y = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.ticks.y = element_blank(),
              panel.border = element_blank()
            )  +
            scale_x_date(expand=c(0,0), 
                         date_breaks = "3 months", 
                         date_labels = "%b-%y") +
            # remove legend
            #theme(legend.position = "none") +
            geom_vline(data = data_events_dates, aes(xintercept = date), color = "orange", size=1) +
            geom_vline(data = data_events_key, aes(xintercept = date), color = "red", size=1) + 
            guides(fill=guide_legend(title="% Change from Previous Month"))
          
          p +
            geom_label(data = data_events, aes(x = date, label = name, y=0.6), color = "black", size=3, fontface=2, fill="white")
        } else {
          p <- ggplot() +
            ggtitle(paste0("Insufficient Rent Data Change for (",input$rentTime_propertyType, ")"))
          p
        }
      } else {
        p <- ggplot() +
          ggtitle(paste0("There were rental transactions for (",input$rentTime_propertyType, ")"))
        p
      }
    }
  )
  
  output$tsBoxplot_Rent_Plot <- renderPlotly({
    
    rent_selected = rent
    #rent_selected = rent %>%
    # filter(Postal_Desc == input$rent_postalDesc & Property_Type == input$rent_propertyType) 
    
    if (input$rentTimeEach_propertyType != "All") {
      rent_selected = rent_selected %>%
        filter(Postal_Desc == input$rentTimeEach_postalDesc & Property_Type == input$rentTimeEach_propertyType) 
    } else {
      rent_selected = rent_selected %>%
        filter(Postal_Desc == input$rentTimeEach_postalDesc)
    }
    
    if (nrow(rent_selected) > 0) {
      rent_selected$year_month <- floor_date(rent_selected$Rent_Date_DDMMYY,  # Create year-month column
                                             "month")
      
      
      p <- ggplot(rent_selected, aes(x=reorder(format(year_month,'%b%y'),year_month), y=Unit_Price_PSM)) + 
        geom_boxplot(
          # custom boxes
          color="skyblue",
          fill="skyblue",
          alpha=0.2,
          
          # custom outliers
          outtlier.colour="red",
          outlier.fill="red",
          outlier.size=3
        ) +
        theme_few() + 
        theme(text=element_text(size=12, face="bold", color ="#6D9EC1"),
              axis.text=element_text(size=6, face="bold"),
              plot.title = element_text(size = 12)) +
        xlab("Month of Rental") +
        ylab("Rental Unit Price ($/sqm)") +
        expand_limits(x=0) +
        ggtitle(paste0("Rental Unit Price ($/sqm) in Postal District ",input$rentTimeEach_postalDesc, " (",input$rentTimeEach_propertyType, ")")) +
        stat_summary(
          fun = median,
          geom = 'line',
          aes(group = 1),
          position = position_dodge(width = 0.9) 
        )
      
      output = ggplotly(p,tooltip="text")
      
      # overrides black outline of outliers
      output$x$data[[1]]$marker$line$color = "red"
      # overrides black extreme outlier color
      output$x$data[[1]]$marker$outliercolor = "red"
      # overrides black not as extreme outlier color
      output$x$data[[1]]$marker$color = "red"
      
      output
    } else {
      p <- ggplot() +
        ggtitle(paste0("There were no rental transactions for Postal District ",input$rentTimeEach_postalDesc, " (",input$rentTimeEach_propertyType, ")"))
      p
    }
  }
  )
    
########################################  Server > Mythbusting Myth 1  ################################################# 

  output$myth1_plot <- renderPlot(
    width=900,
    height=700,
    res = 90,
    {
      p2_data <- sales %>%
        select(c(`Completion`, `N_Area_SQM`, `Property_Type_Bin`)) %>%
        filter(!is.na(`Completion`))
      
      p2 <-p2_data %>%
        filter(Property_Type_Bin == input$myth1_var1) %>%
        ggbetweenstats(
          x = Completion,
          xlab = "Completion Year",
          y = N_Area_SQM,
          ylab = "Floor Area (sqm)",
          type = input$myth1_test,
          pairwise.comparisons = FALSE,
          point.args = list(alpha=0),
          violin.args = list(fill = "#6D9EC1", alpha = 0.5),
          title = paste("Floor Area (sqm) against Completion Year for", input$myth1_var1)) +
        theme_bw() +
        theme(plot.title = element_text(size = rel(1.5), face = "bold"),
              plot.subtitle = element_text(size = rel(1.3)),
              legend.position ="none",
              text=element_text(size=12, face="bold", color ="#6D9EC1", margin = margin(t = 0, r = 20, b = 0, l = 0)),
              axis.text=element_text(size=10, face="bold", margin = margin(t = 0, r = 20, b = 0, l = 0)))
      
      p2
    })
  
########################################  Server > Mythbusting Myth 2  #################################################  

  output$myth2_plot <- renderPlot(
    width=900,
    height=600,
    res = 90,
    {
      
      if(input$myth2_var1=="Dist_to_MRT_m"){
        p3 <- ggbetweenstats(data = sales,
                             x= Property_Type_Bin,
                             xlab = "Property Type",
                             y= Dist_to_MRT_m,
                             ylab = "Distance to MRT (m)",
                             type = input$myth2_test,
                             pairwise.display = "significant",
                             point.args = list(alpha=0),
                             violin.args = list(fill = "#6D9EC1", alpha = 0.5),
                             centrality.label.args = list(size=4, nudge_x =0.4, nudge_y =50),
                             title = "Distribution of Distance to MRT by Housing Type") +
          theme_bw()+
          theme(plot.title = element_text(size = rel(1.5), face = "bold"),
                plot.subtitle = element_text(size = rel(1.3)),
                legend.position ="none",
                text=element_text(size=12, face="bold", color ="#6D9EC1", margin = margin(t = 0, r = 20, b = 0, l = 0)),
                axis.text=element_text(size=10, face="bold", margin = margin(t = 0, r = 20, b = 0, l = 0)))
        
      }
      
      else{
        p3 <- ggbetweenstats(data = sales,
                             x= Property_Type_Bin,
                             xlab = "Property Type",
                             y= Time_to_CBD_min,
                             ylab = "Driving time to CBD (mins)",
                             type = input$myth2_test,
                             pairwise.display = "significant",
                             point.args = list(alpha=0),
                             violin.args = list(fill = "#6D9EC1", alpha = 0.5),
                             centrality.label.args = list(size=4, nudge_x =0.4, nudge_y =2),
                             title = "Driving Time to CBD by Housing Type") +
          theme_bw()+
          theme(plot.title = element_text(size = rel(1.5), face = "bold"),
                plot.subtitle = element_text(size = rel(1.3)),
                legend.position ="none",
                text=element_text(size=12, face="bold", color ="#6D9EC1", margin = margin(t = 0, r = 20, b = 0, l = 0)),
                axis.text=element_text(size=10, face="bold", margin = margin(t = 0, r = 20, b = 0, l = 0)))
        
      }
      
      p3
    })
  
  output$myth2_text <- renderUI({
    if(input$myth2_var1=="Dist_to_MRT_m"){
      writeup <- c("Landed housing types are often perceived as less accessible than other types of housing.",
                   "From the plot, we can see that on average, non-landed housing types (condo/apartments and executive condominiums) are nearer to an MRT station the landed housing types. 
                   Mean distance to the nearest MRT of non-landed housing types was significantly shorter than that of landed housing types. 
                   While the differences between landed housing types were statistically significant, the differences were not very pronounced.",
                   "Condo/apartments and detached houses have a larger variation in distance to nearest MRT station.",
                   "It should be noted that this is only based on houses transacted in the past 3 years and does not cover all private houses that have been built, 
                   so it may not be truly representative of the population.")
      
      display_text <- HTML(paste0(writeup, sep="", collapse="<br><br>"))
    }
    
    else{
      writeup <- c("The mean driving time to CBD for all 5 property types were significantly different from each other.",
                   "The travel time between executive condominiums and the CBD was the longest, followed by landed housing types. 
                   While the mean travel time to CBD between the landed housing types were significantly different, the difference is not large.",
                   "Executive condominiums are a type of hybrid public-private housing subsidised by the Government. 
                   Unlike other types of private housing, they are considered public housing and subject to some restrictions during the first 10 years after completion. 
                   As such, they tend to be located further from the CBD where land is cheaper.",
                   "The travel time to the CBD for condos/apartments has a large variation and with some units less than 10 minutes from the CBD. 
                   Buyers looking for a place with better access the CBD would have more luck looking for condo/apartments than the landed housing types.
                   High-value landed properties near to the CBD are probably rare and less frequently transacted in the market so none were detected in this dataset.",
                   "It should be noted that this is only based on houses transacted in the past 3 years and does not cover all private houses that have been built, 
                   so it may not be truly representative of the population.")
      
      display_text <- HTML(paste0(writeup, collapse="<br><br>"))
    }
    
    display_text
    
  })
   
########################################  Server > Mythbusting Myth 3  #################################################  
 
   output$myth3_plot <- renderPlotly({
    
    
    sales_selected = sales %>%
      filter(Project_Name == input$buyRecentLaunch_projName) %>%
      mutate(year_quart = paste0(year(Sale_Date_DDMMYY),"Q", quarter(Sale_Date_DDMMYY))) %>%
      mutate(year_quart = as.factor(year_quart))
    
    
    if (nrow(sales_selected) > 0) {
      postal_desc = unique(sales_selected$Postal_Desc)
      
      # New facet label names for Floor_Area_SQM_Bin variable
      floor_area.labs <- c("<60 sqm", "61 - 90 sqm", "91 - 130 sqm", ">130 sqm")
      names(floor_area.labs) <- c("0 - 60", "61 - 90", "91 - 130", ">130")
      
      
      p <- ggplot(sales_selected, aes(x=year_quart, y=Unit_Price_PSM)) + 
        geom_boxplot(
          # custom boxes
          color="skyblue",
          fill="skyblue",
          alpha=0.2,
          
          # custom outliers
          #outtlier.colour="red",
          #outlier.fill="red",
          #outlier.size=3
        ) +
        facet_wrap(~Floor_Area_SQM_Bin, nrow = 4, labeller = labeller(Floor_Area_SQM_Bin = floor_area.labs)) +
        theme_few() + 
        theme(plot.title = element_text(size = 11, face = "bold", margin = margin(t = -30), vjust = -30),
              legend.position ="none",
              text=element_text(size=12, face="bold", color ="#6D9EC1"),
              axis.text=element_text(size=8, face="bold")) +
        xlab("Period of Sale") +
        ylab("Unit Price ($/sqm)") +
        ggtitle(paste0("Unit Price ($/sqm) for Sales for Project - ", input$buyRecentLaunch_projName, " (District: ", postal_desc , ")")) +
        stat_summary(
          fun = median,
          geom = 'line',
          aes(group = 1),
          position = position_dodge(width = 0.9) 
        )
      
      output = ggplotly(p,tooltip="text", height = 720)
      
      for (x in 1:4) {
        # overrides black outline of outliers
        output$x$data[[x]]$marker$line$color = "red"
        # overrides black extreme outlier color
        output$x$data[[x]]$marker$outliercolor = "red"
        # overrides black not as extreme outlier color
        output$x$data[[x]]$marker$color = "red"
      }
      
     output     
      
    } else {
      p <- ggplot() +
        ggtitle(paste0("There were no sales transactions for Project - ", input$buyRecentLaunch_projName))
      p
    }
  })
  output$myth3_text <- renderUI({
    writeup <- c("Most people assume that earliest bird will catch the fattest worm and that buying a new condominium unit in an earlier launch phase will mean a better deal.",
                   "In addition, there is also a belief that bigger floor area will equate to lower Unit Price ($/sqm).",
                   "Based on the observation from projects with new sales of more than 100 units in the past 3 years, we can observe that those 2 statements are not always true.",
                   "For example, in Irwell Hill Residences, the median pricing across all unit sizes and over time was almost constant. For One Holland Village Residences, the Unit Price ($/sqm)
                    was highest for its largest units (floor area >130 sqm), compared to its other units of smaller sizes.",
                   "Some projects like Leedon Green and OLA observed a dip in Unit Price ($/sqm) for a short period before rising again. On the other hand, some like the smaller units (<60sqm) in
                   saw a consistent increase in unit price throughout the 3 years period.", 
                   "Therefore, we can observe that different developers have different strategies for pricing their launch phases and the earliest launch is not always the lowest priced.")
      
    display_text <- HTML(paste0(writeup, sep="", collapse="<br><br>"))
    
    display_text
    
  })
}
  
shinyApp(ui, server)
  