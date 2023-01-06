
# Helper functions --------------------------------------------------------

# Create a regex for a given medicine name
med_regex <- function(medicine){
    
    first <- stringr::str_sub(medicine, 1, 1)
    rest <- stringr::str_sub(medicine, 2, -1)
    med_regex <- paste0("[", 
                        stringr::str_to_upper(first), 
                        stringr::str_to_lower(first),
                        "]",
                        rest)
    return(med_regex)
}


# Functions for tables ----------------------------------------------------

# Create a reactable table of TAs for a given medicine name

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
            defaultPageSize = 20,
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
            style = list(fontSize = "14px"),
            columns = list(
                guidance_no = colDef(name = "Guidance number"),
                title = colDef(name = "Guidance title",
                               minWidth = 400),
                pub_date = colDef(name = "Publication date")
            ))
    
    return(temp)
}


# Functions for charts ----------------------------------------------------

# Define a vertical line for a plotly chart (e.g. date of TA publication)
define_vline <- function(input_date){
    
    temp <- list(type = "line",
                 fillcolor = "black",
                 line = list(color = "black",
                             dash = "dot",
                             width = 2.5),
                 opacity = 0.5,
                 x0 = input_date,
                 x1 = input_date,
                 xref = "x",
                 y0 = 0,
                 y1 = 0.95,
                 yref = "paper")
    return(temp)
}

# Function to label a vertical line on a plotly chart, 
# yheight variable allows for adjusting of overlapping labels
label_vline <- function(fig, input_date, label, yheight = 1.01){
    
    temp <- fig %>% 
        add_annotations(y = yheight,
                        x = input_date,
                        xref = "x",
                        yref = "paper",
                        text = label,
                        showarrow = FALSE,
                        xanchor = "center",
                        font = list(color = "#000000",
                                    font = "Arial",
                                    size = 12))
    return(temp)
}


# Create a chart showing usage of a single medicine

single_med_plot <- function(nat_meds_df, medicine, plot_title, ylabel){
    
    
    tmp_df <- nat_meds_df %>% 
        filter(treatment_name == medicine) %>% 
        group_by(date, treatment_name) %>% 
        summarise(numerator = sum(numerator)/91) %>% 
        ungroup()
    
    plot <- tmp_df %>% 
        plot_ly(x = ~date,
                y = ~numerator) %>% 
        add_trace(type = "scatter",
                  mode = "lines+markers",
                  fill = 'tozeroy') %>% 
        layout(title = list(text = stringr::str_wrap(plot_title, width = 95),
                            font = list(size = 12),
                            x = 0.05,
                            y = 0.96,
                            yanchor = "top"),
               yaxis = list(title = list(text = ylabel,
                                         standoff = 5),
                            rangemode = "tozero",
                            range = list(0, max(tmp_df$numerator) * 1.2)),
               xaxis = list(title = FALSE,
                            tickformat = "%b<br>%Y"),
               hovermode = "x unified",
               margin = list(t = 65)) %>% 
        config(displaylogo = FALSE,
               modeBarButtonsToRemove = c("zoom", "pan", "select", "lasso"))
    
    return(plot)
}

# Function to add vertical lines to plot (e.g. TA publication dates)

add_TA_lines <- function(plot, meds_ta_df, nat_meds_df, medicine){
    
    # Filter for TAs within the range of our data -----------------------------
    
    med_reg <- med_regex(medicine)
    
    tmp_df <- nat_meds_df %>% filter(treatment_name == medicine)
    
    min_date <- min(tmp_df$date)
    max_date <- max(tmp_df$date)
    
    tmp_ta_df <- meds_ta_df %>% 
        filter(stringr::str_detect(title, med_reg),
               between(pub_date, as.Date(min_date), as.Date(max_date)))
    
    # Add lines to plot -------------------------------------------------------
    
    line_dates <- tmp_ta_df %>% pull(pub_date)
    
    line_labels <- tmp_ta_df %>% pull(guidance_no)
    
    line_list <- purrr::map(line_dates, define_vline)
    
    plot <- plot %>% 
        layout (shapes = line_list)
    
    for (i in seq_along(line_dates)){
        plot <- plot %>% label_vline(line_dates[i], line_labels[i])
    }
    
    return(plot)
}

# Shiny plot --------------------------------------------------------------

create_med_plot <- function(nat_meds_df, medicine, ylabel){
    
    
    tmp_df <- nat_meds_df %>% 
        filter(treatment_name == medicine) %>% 
        group_by(date, treatment_name) %>% 
        summarise(numerator = sum(numerator)/91) %>% 
        ungroup()
    
    plot <- tmp_df %>% 
        plot_ly(x = ~date,
                y = ~numerator) %>% 
        add_trace(type = "scatter",
                  mode = "lines+markers",
                  fill = 'tozeroy') %>% 
        layout(yaxis = list(title = list(text = ylabel,
                                         standoff = 5),
                            rangemode = "tozero",
                            range = list(0, max(tmp_df$numerator) * 1.2)),
               xaxis = list(title = FALSE,
                            tickformat = "%b<br>%Y"),
               hovermode = "x unified") %>% 
        config(displaylogo = FALSE,
               displayModeBar = FALSE)
    
    return(plot)
}



# Indication text ---------------------------------------------------------

write_indication_text <- function(indications_df, medicine){
    
    med_reg <- med_regex(medicine)
    
    temp_df <- indications_df %>% 
        filter(stringr::str_detect(medicine, med_reg))
    
    indication_text <- ""
    
    for (i in seq_len(nrow(temp_df))){
        
        tmp_text <- paste0("<b>",
                           pull(temp_df[i,"indication"]),
                           "</b><br>",
                           pull(temp_df[i,"description"]),
                           "<br><br>")
        
        indication_text <- stringr::str_c(indication_text, tmp_text)
    }
    
    return(indication_text)
}


