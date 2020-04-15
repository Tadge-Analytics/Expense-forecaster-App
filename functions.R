# instructions text

instructions_text <- "Double click on the table to add additional expense / income items. \n
The chart will only update after 'Category level 1', 'Timeframe', 'Amount' and 'Reference date' are entered. \n
Do you data entry in Excel, by clicking the 'Download button'. Re-upload when you're ready."



# generate blank table

generate_blank_table <- function() {
  
  tibble::tribble(
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
  
  
}








# prep the import

process_the_rates_file <- function(rates_file_path, sheet = "forecast rates") {
  
  
  rates_file_path %>% 
    
    # file_path <- "Categorisations done.xlsx"
    # sheet <- "forecast rates"
    # file_path %>%
    
    read_excel(sheet = sheet, col_types = "text") %>% 
    
    distinct(`Category Level1`, .keep_all = T) %>% 
    
    mutate(`Reference date` = format(as.Date(as.numeric(`Reference date`), origin = "1899-12-30"), "%d/%m/%Y")) %>% 
    
    add_row() %>% 
    add_row() %>% 
    add_row() %>% 
    add_row() %>% 
    mutate_all(.funs = ~replace_na(., replace = ""))
  
}






correct_col_types <- function(prepped_import) {
  
  prepped_import %>%
    
    # generate_blank_table() %>%
    
    mutate(`Category Level1` = trimws(toupper(`Category Level1`)), 
           `Category Level2` = trimws(toupper(`Category Level2`)),
           
           Timeframe = tolower(Timeframe), 
           Frequency = as.numeric(Frequency),
           Amount = as.numeric(Amount),
           `Reference date` = dmy(`Reference date`),
           `Stop after` = as.numeric(`Stop after`))
  
}







# determine future dates

prep_from_rates_and_dates <- function(prepped_import, days_to_add = 370) {
  
  prepped_import %>%
    
    # days_to_add <- 370
    # generate_blank_table() %>% 
    
    filter(`Category Level1` != "" &
             Timeframe  != "" &
             Amount != "" &
             `Reference date` != "") %>% 
    
    correct_col_types() %>% 
    
    
    mutate(Frequency = if_else(is.na(Frequency), 1, Frequency),
           Amount = if_else(str_trim(str_to_upper(`Income (Y)?`)) == "Y", Amount, -Amount),
           Date = pmap(list(`Reference date`, today() + days_to_add, Timeframe), function(.x, .y, .z) seq.Date(.x, .y, by = .z))) %>%
    unnest(Date) %>%
    
    group_by(`Category Level1`) %>%
    filter((row_number()-1) %% Frequency == 0) %>%
    filter(Date >= today()) %>% 
    filter(if_else(is.na(`Stop after`), TRUE, row_number() <= `Stop after`)) %>% 
    
    ungroup() %>%
    select(Date, `Category Level1`, `Category Level2`, Amount)
  
  
}





pre_plotting_data_prep <- function(prepped_data, days_to_add = 370, chart_setting = "Both") {
  
  # days_to_add <- 370
  # prepped_data <- generate_blank_table() %>%
  #   prep_from_rates_and_dates()
  
  new_prepped_data <- prepped_data %>% 
    filter(Amount != 0) %>% 
    {if (chart_setting == "Expenses only") {filter(., Amount < 0) } else {.}} %>% 
    {if (chart_setting == "Income only") {filter(., Amount > 0) } else {.}} %>% 
    mutate(income_or_expense = if_else(Amount > 0, "Income", "Expense"),
           abbreviation = if_else(Amount > 0, " (Inc.)", " (Exp.)")) %>% 
    
    group_by(`Category Level2`) %>% 
    mutate(distinct = n_distinct(income_or_expense) > 1) %>% 
    ungroup() %>% 
    mutate(`Category Level2` = if_else(distinct, paste0(`Category Level2`, abbreviation), `Category Level2`)) %>% 
    select(-distinct, -abbreviation)
  
  
  new_prepped_data %>%   
    distinct(`Category Level1`, `Category Level2`, income_or_expense) %>% 
    crossing(Date = seq.Date(today(), today() + days_to_add, 1)) %>% 
    left_join(new_prepped_data, by = c("Date", "Category Level1", "Category Level2", "income_or_expense")) %>%
    mutate(Amount = replace_na(Amount, 0)) 
  
}









# turn the prepped data into a plot

plot_the_data <- function(fully_prepped_data, view_level = 1, chart_setting = "Both") {
  
  # days_to_add <- 370
  # fully_prepped_data <- generate_blank_table() %>%
  #   prep_from_rates_and_dates() %>%
  #   pre_plotting_data_prep()
  
  
  ggplot_of_spend <-
    fully_prepped_data %>%
    
    {if (chart_setting == "Net Position") {
      
      group_by(., Date, income_or_expense) %>%
        summarise(Amount = sum(Amount)) %>% 
        group_by(income_or_expense) %>% 
        mutate(cumulative_amount = cumsum(Amount)) %>% 
        ungroup() %>% 
        mutate(income_or_expense = fct_reorder(factor(income_or_expense), -cumulative_amount, max)) %>% 
        
        ggplot() +
        aes(Date) +
        geom_area(aes(y = abs(cumulative_amount), fill = income_or_expense), stat = "identity", position = "identity", alpha = 0.7) +
        geom_line(aes(y = cumulative_amount), stat = "summary", fun = sum, linetype = "dotted") 
      
      
    } else {if (view_level == 2) {
      
      group_by(., Date, `Category Level2`) %>%
        summarise(Amount = sum(Amount)) %>% 
        group_by(`Category Level2`) %>% 
        mutate(cumulative_amount = cumsum(Amount)) %>% 
        ungroup() %>% 
        mutate(`Category Level2` = fct_reorder(factor(`Category Level2`), -cumulative_amount, sum)) %>% 
        
        ggplot() +
        aes(Date) +
        geom_bar(aes(y = cumulative_amount, fill = `Category Level2`), stat = "identity")
      
      
    } else {
      
      group_by(., `Category Level1`) %>%
        mutate(cumulative_amount = cumsum(Amount)) %>% 
        ungroup() %>% 
        mutate(`Category Level1` = fct_reorder(factor(`Category Level1`), -cumulative_amount, sum)) %>% 
        
        
        ggplot() +
        aes(Date) +
        geom_bar(aes(y = cumulative_amount, fill = `Category Level1`), stat = "identity")
      
      
    }}} +
    
    labs(fill = "",
         x = NULL,
         y = NULL) +
    theme_minimal() +
    
    {if (chart_setting == "Expenses only") 
    {scale_y_continuous(trans = "reverse", labels = scales::dollar_format())} else 
    {scale_y_continuous(labels = scales::dollar_format())}} 
  
  
  
  
  ggplot_of_spend %>% 
    
    ggplotly() %>%
    config(displayModeBar = F) %>%
    layout(xaxis=list(fixedrange=TRUE)) %>%
    layout(yaxis=list(fixedrange=TRUE))
  
  
}





