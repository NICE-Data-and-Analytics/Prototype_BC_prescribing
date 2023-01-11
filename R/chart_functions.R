

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

# Define a horizontal line for a plotly chart (e.g. estimated treatment population)
define_hline <- function(y_value){
    
    temp <- list(type = "line",
                 fillcolor = "black",
                 line = list(color = "black",
                             #dash = "dot",
                             width = 2.5),
                 opacity = 0.5,
                 x0 = 0,
                 x1 = 1,
                 xref = "paper",
                 y0 = y_value,
                 y1 = y_value,
                 yref = "y")
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

# Function to label a vertical line on a plotly chart, 
label_hline <- function(fig, y_value, label){
    
    temp <- fig %>% 
        add_annotations(y = y_value,
                        x = 0.1,
                        xref = "paper",
                        yref = "y",
                        text = label,
                        showarrow = FALSE,
                        xanchor = "center",
                        font = list(color = "#000000",
                                    font = "Arial",
                                    size = 12))
    return(temp)
}

# Create plot --------------------------------------------------------------

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


# Add lines to plot -------------------------------------------------------

# TA publications dates and expected usage

add_TA_lines <- function(plot, meds_ta_df, nat_meds_df, estimate_text_df, med_input){
    
    med_reg <- med_regex(med_input)
    
    # Filter for TAs within the range of our data -----------------------------
    
    tmp_df <- nat_meds_df %>% 
        filter(stringr::str_detect(treatment_name, med_reg))
    
    min_date <- min(tmp_df$date)
    max_date <- max(tmp_df$date)
    
    tmp_ta_df <- meds_ta_df %>% 
        filter(stringr::str_detect(title, med_reg),
               between(pub_date, as.Date(min_date), as.Date(max_date)))
    
    ta_dates <- tmp_ta_df %>% pull(pub_date)
    
    line_labels <- tmp_ta_df %>% pull(guidance_no)
    
    line_list <- purrr::map(ta_dates, define_vline)
    
    # Determine whether to include estimate -----------------------------------
    if (sum(stringr::str_detect(unique(estimate_text_df$medicine), med_reg)) > 0) {
        
        temp_df <- estimate_text_df %>%
            filter(stringr::str_detect(medicine, med_reg))
        
        estimate <- temp_df[,"people_qtr"] %>%
            pull() %>%
            define_hline() %>% 
            list()
        
        line_list <- c(line_list, estimate)
    }
    
    # Add lines to plot -------------------------------------------------------
    
    plot <- plot %>% 
        layout (shapes = line_list)
    
    for (i in seq_along(ta_dates)){
        
        plot <- plot %>% label_vline(ta_dates[i], line_labels[i])
    }
    
    if (sum(stringr::str_detect(unique(estimate_text_df$medicine), med_reg)) > 0){
        
        plot <- plot %>% label_hline(pull(temp_df[,"people_qtr"])*1.08, "Expected usage")
    }
    
    return(plot)
}