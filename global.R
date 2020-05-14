source("scripts/libraries.R")
source("scripts/functions.R")
# source("scripts/modules.R")



level_options <- c("Level 1" = 1, 
                   "Level 2" = 2)


    
display_options <- c("Expenses only",
                     "Income only",
                     "Both",
                     "Net Position") 



blank_table <- tibble::tribble(
    ~`Category Level1`, ~Amount,    ~`Category Level2`, ~`Income (Y)?`, ~Timeframe, ~Frequency, ~`Reference date`, ~`Stop after`,
    "Job Wage",   2500L,              "Work",            "Y",     "week",         2L,              NA,          NA,
    "Eating Out",    100L,            "Living",             NA,     "week",         NA,              NA,          NA,
    "Groceries",    150L,            "Living",             NA,     "week",         NA,              NA,          NA,
    "Rent",   1300L,      "Accomodation",             NA,    "month",         NA,              NA,          NA,
    "Electricity",    100L,      "Accomodation",             NA,    "month",         3L,              NA,          NA,
    "Water",     50L,      "Accomodation",             NA,    "month",         3L,              NA,          NA,
    "Gas",     50L,      "Accomodation",             NA,    "month",         3L,              NA,          NA,
    "Internet",     50L,      "Accomodation",             NA,    "month",         NA,              NA,          NA,
    "Mobile Phone",     50L,      "Accomodation",             NA,    "month",         NA,              NA,          NA,
    "Car Rego",    700L, "Transport Related",             NA,     "year",         NA,              NA,          NA,
    "Car Insurance",     40L, "Transport Related",             NA,    "month",         NA,              NA,          NA,
    "Car Servicing",    400L, "Transport Related",             NA,    "month",         6L,              NA,          NA,
    "Petrol",     40L, "Transport Related",             NA,     "week",         NA,              NA,          NA,
    "Parking",      0L, "Transport Related",             NA,     "week",         NA,              NA,          NA,
    "Netflix",     15L,     "Entertainment",             NA,    "month",         NA,              NA,          NA,
    "Spotify",     15L,     "Entertainment",             NA,    "month",         NA,              NA,          NA,
    "Movies",     15L,     "Entertainment",             NA,    "month",         NA,              NA,          NA,
    "Public Transport",      0L, "Transport Related",             NA,     "week",         NA,              NA,          NA,
    "Other Medical",      0L,            "Health",             NA,     "year",         NA,              NA,          NA,
    "Health Insurance",      0L,            "Health",             NA,    "month",         NA,              NA,          NA
  ) %>% 
    mutate(`Reference date` = format(today() - 1, "%d/%m/%Y")) %>% 
    add_row() %>% 
    add_row() %>% 
    add_row() %>% 
    add_row() %>% 
    mutate_all(.funs = ~replace_na(., replace = ""))




steps <- seq(50, 200, 50)
lengthMenu_options <- list(c(-1, steps), c("All", as.character(steps)))



