table_names <- c("crime_scene_report",
                 "drivers_license",
                 "facebook_event_checkin",
                 "get_fit_now_check_in",
                 "get_fit_now_member",
                 "income", 
                 "interview",
                 "person")

url_base <- "https://raw.githubusercontent.com/atheobold/groupworthy-data-science/refs/heads/main/labs/instructions/data/lab-5-data/"

# Function to paste the URL with the name of the table, 
# Store the resulting table in the Global Environment
read_in_tables <- function(x){ 
  assign(x, 
         value = readr::read_csv(
           str_c(url_base, x, ".csv", sep = "")
           ), 
         envir = .GlobalEnv)
}

# For each name in the table names, read in the table and store it as the name.
purrr::walk(.x = table_names, 
            .f = read_in_tables)
