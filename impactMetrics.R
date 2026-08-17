# Load packages
library(googlesheets4)

# Read the RRC Reservation Form response spreadsheet

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

sheet_id <- "1ed3zy3QERXUd7yMlfxtwxqAu3sqhpNWP-TScr1Y_dMM"
reservations <- read_sheet(sheet_id)
