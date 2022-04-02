packages = c('tidyverse','lubridate')

for(p in packages){
  if(!require(p, character.only = T)){
    install.packages(p)
  }
  library(p, character.only = T)
}

sales <- read_csv("data/sales_tm_joined.csv")
rent <- read_csv("data/rental_tm_joined.csv")
postal_district <- read_csv("data/postal_districts.csv")

#Selection of Columns
sales_selected <- sales %>% select("Project_Name", "Transacted_Price", 
                                   "Sale_Date", "Address", "Type_of_Sale", "Area_SQM", "Unit_Price_PSM",
                                   "Property_Type", "Number_of_Units", "Tenure", "Completion_Date", 
                                   "Postal_Code", "Postal_District")


rent_selected <- select(rent, -c("Floor_Area_SQFT"))

# Data Processing

# Rental Data
# 1) Rental Housing SQM (Get the mid-point of the Floor Area Range)
rent_selected <- rent_selected %>%
  mutate("Avg_Floor_Area_SQM" = sapply(strsplit((Floor_Area_SQM), split=" - "), function(x) mean(as.numeric(x))))

# 2) Rental Housing Rental Price per SQM, Normalise the Rental Price against SQM
rent_selected <- rent_selected %>%
  mutate("Unit_Price_PSM" = round(Monthly_Rent / Avg_Floor_Area_SQM))

# 3) Bin the Floor Area to 4 Groups (60 SQM and below, 61 - 90 SQM, 91 - 130 SQM, 130 SQM and above)
rent_selected <- rent_selected %>%
  mutate("Floor_Area_SQM_Bin" = ifelse(Avg_Floor_Area_SQM %in% 0:60, "0 - 60",
                                  ifelse(Avg_Floor_Area_SQM %in% 61:90, "61 - 90",
                                         ifelse(Avg_Floor_Area_SQM %in% 91:130, "91 - 130",
                                                ">130"))))

# 4) Group No_of_Bedroom of 4 or more units Together (i.e. > 3 units)
rent_selected <- rent_selected %>%
  mutate("No_of_Bedroom_Bin" = ifelse(No_of_Bedroom > 3, ">= 4",
                                                     No_of_Bedroom))

# 5) Join the Rental Dataset with the Postal District Description
rent_selected <- left_join(rent_selected, postal_district,
                           by =c ('Postal_District' = 'Postal_District'))

# 6) Change "Non-landed Properties" to "Condo/Apartment"
rent_selected$Property_Type[rent_selected$Property_Type == "Non-landed Properties"] <- "Condo/Apartment"  

# 7) Write to CSV
write_csv(rent_selected, "data/rental_prep.csv")

# Sales Data
# 1) Split Multiple Units into Multiple Rows
sales_multiple <- subset(sales_selected, Number_of_Units>1)
sales_multiple <- sales_multiple[rep(seq_len(nrow(sales_multiple)), sales_multiple$Number_of_Units-1), ]

sales_selected <- rbind(sales_selected,sales_multiple)

# 2) Calculate Normalised Transacted_Price and Area_SQM
sales_selected <- sales_selected %>%
  mutate("N_Transacted_Price" = round(Transacted_Price / Number_of_Units))

sales_selected <- sales_selected %>%
  mutate("N_Area_SQM" = round(Area_SQM / Number_of_Units, 1))


# 3) Bin the Normalised Floor Area to 4 Groups (60 SQM and below, 61 - 90 SQM, 91 - 130 SQM, 130 SQM and above)
sales_selected <- sales_selected %>%
  mutate("Floor_Area_SQM_Bin" = ifelse(N_Area_SQM %in% 0:60, "0 - 60",
                                       ifelse(N_Area_SQM %in% 61:90, "61 - 90",
                                              ifelse(N_Area_SQM %in% 91:130, "91 - 130",
                                                     ">130"))))

# 4) Join the Sales Dataset with the Postal District Description
sales_selected <- left_join(sales_selected, postal_district,
                           by =c ('Postal_District' = 'Postal_District'))

# 5) Tenure: To extract the Tenure Period from the Tenure Description (i.e. extract the 1st word)
sales_selected <- sales_selected %>%
  mutate("Tenure_Period" = word(Tenure, 1))

# 6) Tenure Bin: To group Tenure into 3 data bins “<99”, “99 – 110” and “929 - Freehold"
sales_selected <- sales_selected %>%
  mutate("Tenure_Bin" = ifelse(Tenure_Period %in% 0:98, "<99",
                                       ifelse(Tenure_Period %in% 99:110, "99 - 110",
                                              ("929 - Freehold"))))

# 7) Sale Date: To bin according to YYMM
sales_selected <- sales_selected %>%
  mutate("Sale_Date_MMYY" = format((as.Date(Sale_Date, "%d-%b-%y")), format = "%b-%y"))

# 8) Age: New derived field from “Completion Date” to bin based on distribution 
sales_selected <- sales_selected %>%
  mutate("Age" = ifelse((Completion_Date == "-"), "NA",
                    ifelse((Completion_Date == "Uncompleted"), "NA",
                        as.numeric(year(Sys.Date()))-as.numeric(Completion_Date))))

# 9) Combine Property Type: Condo and Apartment Together
sales_selected <- sales_selected %>%
  mutate("Property_Type_Bin" = ifelse(Property_Type == "Apartment", "Condo/Apartment",
                               ifelse(Property_Type == "Condominium", "Condo/Apartment",
                                     Property_Type)))

# 10) Write to CSV
write_csv(sales_selected, "data/sales_prep.csv")
