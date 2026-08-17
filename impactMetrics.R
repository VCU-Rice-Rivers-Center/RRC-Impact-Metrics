# Load packages
library(googlesheets4)
library(dplyr)
library(lubridate)
library(strex)

######################
### Authentication ###
######################

# Fetch the key from the system environment
gcp_key <- Sys.getenv("GCP_SA_KEY")

if (nzchar(gcp_key)) {
  # Authenticate using the github secret key
  gs4_auth(path = gcp_key)
  message("Authenticated using GCP Service Account Secret")
} else {
  # Authenticate using a browser if testing locally
  gs4_auth()
  message("Authenticated via browser.")
}

#################
### Read Data ###
#################

# Sheet ID for reservation request form responses:
sheet_id <- "1ed3zy3QERXUd7yMlfxtwxqAu3sqhpNWP-TScr1Y_dMM"
reservations <- read_sheet(sheet_id)

##################
### Processing ###
##################

# Formatting free text columns from list -> character
reservations$`How many Non-VCU affiliated individuals will there be?` <- as.character(reservations$`How many Non-VCU affiliated individuals will there be?`)
reservations$`How many VCU employees will there be?` <- as.character(reservations$`How many VCU employees will there be?`)
reservations$`How many VCU students will there be?` <- as.character(reservations$`How many VCU students will there be?`)


# Calculate additional columns:
reservations_v2 <- reservations %>%
  mutate(
    # Calculate number of days requested
    length_of_stay_days = as.numeric(difftime(`Requested departure date`, `Requested arrival date`, units = "days")) + 1,
    
    # Extract year
    year = year(`Requested arrival date`),
    
    # Approximate count of VCU students from free text response
    count_vcu_students_low = str_first_number(`How many VCU students will there be?`),
    count_vcu_students_high = str_last_number(`How many VCU students will there be?`),
    
    # Approximate count of VCU staff/faculty from free text response
    count_vcu_staff_low = str_first_number(`How many VCU employees will there be?`),
    count_vcu_staff_high = str_last_number(`How many VCU employees will there be?`),
    
    # Approximate count of non-VCU persons from free text response
    count_non_vcu_low = str_first_number(`How many Non-VCU affiliated individuals will there be?`),
    count_non_vcu_high = str_last_number(`How many Non-VCU affiliated individuals will there be?`)
    )
