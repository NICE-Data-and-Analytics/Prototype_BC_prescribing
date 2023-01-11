
# Functions for tables ----------------------------------------------------

# Create a reactable data table for a given medicine name
create_data_table <- function(med_df, medicine){
    
    temp <- med_df %>% 
        filter(treatment_name == medicine) %>% 
        group_by(date, treatment_name) %>% 
        summarise("Daily doses" = sum(numerator),
                  "Doses per 100,000 population" = round(sum(numerator)/(first(denominator)/100000), 2),
                  # People, assuming quarter is 91 days
                  "Estimated people" = round(numerator/91,0)) %>% 
        ungroup() %>% 
        arrange(desc(date)) %>% 
        rename("Date" = date, "Medicine" = treatment_name) %>% 
        reactable(
            filterable = FALSE,
            searchable = FALSE,
            highlight = TRUE,
            striped = TRUE,
            compact = TRUE,
            defaultPageSize = 15,
            style = list(fontSize = "14px"),
            defaultColDef = colDef(
                format = colFormat(separators = TRUE)))
    
}

# Create a reactable table of TAs for a given medicine name
create_med_TA_table <- function(meds_ta_df, medicine){
    
    med_reg <- med_regex(medicine)
    
    temp <- meds_ta_df %>% 
        filter(stringr::str_detect(title, med_reg)) %>% 
        arrange(desc(pub_date)) %>% 
        reactable(
            filterable = FALSE,
            searchable = FALSE,
            highlight = TRUE,
            striped = TRUE,
            compact = TRUE,
            style = list(fontSize = "14px"),
            columns = list(
                guidance_no = colDef(name = "Guidance number"),
                title = colDef(name = "Guidance title",
                               minWidth = 400),
                pub_date = colDef(name = "Publication date")
            ))
    
    return(temp)
}

# Create a reactable table for estimated population calculations
create_estimate_table <- function(estimate_table_df, medicine){
    
    med_reg <- med_regex(medicine)
    
    temp <- estimate_table_df %>% 
        filter(stringr::str_detect(medicine, med_reg)) %>% 
        select(-medicine) %>% 
        reactable(filterable = FALSE,
                  searchable = FALSE,
                  highlight = TRUE,
                  striped = TRUE,
                  compact = TRUE,
                  defaultPageSize = 20,
                  style = list(fontSize = "14px"),
                  columns = list(
                      estimate = colDef(name = "Estimate of treatment population",
                                        minWidth = 400),
                      percentage = colDef(name = "Percentage of people",
                                          format = colFormat(percent = TRUE, digits = 2)),
                      people = colDef(name = "Number of people",
                                      format = colFormat(separators = TRUE, digits = 0))))
    
    return(temp)
}