# Load packages
library(googlesheets4)
library(dplyr)
library(tidyr)
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
reservations <- reservations %>%
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

#########################
### Calculate Metrics ###
#########################

# 1. Count of students visiting per year

students_per_year <- reservations %>%
  group_by(year) %>%
  summarize(
    students_per_year_low_estimate = sum(count_vcu_students_low, na.rm = TRUE),
    students_per_year_high_estimate = sum(count_vcu_students_high, na.rm = TRUE)
  )

# 2. Count of VCU staff and faculty visiting per year

vcu_staff_per_year <- reservations %>%
  group_by(year) %>%
  summarize(
    vcu_staff_per_year_low_estimate = sum(count_vcu_staff_low, na.rm = TRUE),
    vcu_staff_per_year_high_estimate = sum(count_vcu_staff_high, na.rm = TRUE)
  )

# 3. Count of non-VCU visitors per year

non_vcu_per_year <- reservations %>%
  group_by(year) %>%
  summarize(
    non_vcu_per_year_low_estimate = sum(count_non_vcu_low, na.rm = TRUE),
    non_vcu_per_year_high_estimate = sum(count_non_vcu_high, na.rm = TRUE)
  )

# 4. Days, proportion, and estimated revenue per year of lodge usage

lodge_usage_days <- reservations %>%
  filter(grepl("Inger Rice Lodge", `Facility Requested`)) %>%
  group_by(year) %>%
  summarize(
    lodge_usage_days = sum(length_of_stay_days)
  )

lodge_usage_proportion <- lodge_usage_days %>%
  mutate(
    lodge_usage_proportion = as.numeric(lodge_usage_days)/365
  )

lodge_cost <- 15 # $15 per person per day

lodge_revenue <- reservations %>%
  filter(grepl("Inger Rice Lodge", `Facility Requested`)) %>% 
  filter(`Is your group in the School of Life Sciences and Sustainability` == "No") %>%
  replace_na(list(count_vcu_students_low = 0, count_vcu_students_high = 0,
                  count_vcu_staff_low = 0, count_vcu_staff_high = 0,
                  count_non_vcu_low = 0, count_non_vcu_high = 0)
             ) %>%
  mutate(
    total_persons_low_estimate = as.numeric(count_vcu_students_low) + as.numeric(count_vcu_staff_low) + as.numeric(count_non_vcu_low),
    total_persons_high_estimate = as.numeric(count_vcu_students_high) + as.numeric(count_vcu_staff_high) + as.numeric(count_non_vcu_high),
    revenue_per_event_low_estimate = length_of_stay_days*lodge_cost*total_persons_low_estimate,
    revenue_per_event_high_estimate = length_of_stay_days*lodge_cost*total_persons_high_estimate
  ) %>%
  group_by(year) %>%
  summarize(
    lodge_revenue_low_estimate = sum(revenue_per_event_low_estimate, na.rm = TRUE),
    lodge_revenue_high_estimate = sum(revenue_per_event_high_estimate, na.rm = TRUE)
    )

# 5. Days, proportion, and estimated revenue per year of conference room usage

conference_usage_days <- reservations %>%
  filter(grepl("Rice Education Building's Conference Room", `Facility Requested`)) %>%
  group_by(year) %>%
  summarize(
    conference_usage_days = sum(length_of_stay_days)
  )

conference_usage_proportion <- conference_usage_days %>%
  mutate(
    conference_usage_proportion = as.numeric(conference_usage_days)/365
  )

conference_cost <- 400

conference_revenue <- reservations %>%
  filter(grepl("Rice Education Building's Conference Room", `Facility Requested`)) %>% 
  filter(`Is your group in the School of Life Sciences and Sustainability` == "No") %>%
  mutate(
    revenue_per_event = length_of_stay_days*conference_cost
  ) %>%
  group_by(year) %>%
  summarize(conference_revenue = sum(revenue_per_event))
  

# 6. Proportion of events with waved fees

      # Fees are waived for groups affiliated with SLSS

fees_waived_proportion <- nrow(
  reservations %>%
    select(`Is your group in the School of Life Sciences and Sustainability`) %>%
    filter(`Is your group in the School of Life Sciences and Sustainability` == "Yes")
) / nrow(reservations)
