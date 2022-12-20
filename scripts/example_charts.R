
# Load packages -----------------------------------------------------------

library(dplyr)
library(plotly)

# Load data ---------------------------------------------------------------

nat_meds_df <- readr::read_csv("data/clean/bc_meds_nat_2022_10.csv")
stp_meds_df <- readr::read_csv("data/clean/bc_meds_stp_2022_10.csv")
ta_dates <- readr::read_csv("data/raw/TA_dates.csv")


# Functions for lines ------------------------------------------------------

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

define_hline <- function(y_value){
    
    temp <- list(type = "line",
                 fillcolor = "black",
                 line = list(color = "black",
                             dash = "dot",
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


# Create ADD chart (all meds except pembrolizumab)-------------------------

chart_title <- "<b>Prescribing of breast cancer medicines</b>"

nat_meds_df %>% 
    filter(treatment_name != "pembrolizumab") %>% 
    group_by(date, treatment_name) %>% 
    summarise(numerator = sum(numerator),
              denominator = first(denominator),
              dd_per_100000 = numerator/(denominator/100000)) %>% 
    ungroup() %>% 
    plot_ly(x = ~date,
            y = ~dd_per_100000,
            color = ~treatment_name) %>% 
    add_trace(type = "scatter",
              mode = "lines+markers") %>% 
    layout(title = list(text = stringr::str_wrap(chart_title, width = 95),
                        font = list(size = 12),
                        x = 0.05,
                        y = 0.96,
                        yanchor = "top"),
           yaxis = list(title = list(text = "Daily doses per 100,000 population",
                                     standoff = 5),
                        rangemode = "tozero"),
           xaxis = list(title = FALSE),
           hovermode = "x unified",
           margin = list(t = 65)) %>% 
    config(displaylogo = FALSE,
           modeBarButtonsToRemove = c("zoom", "pan", "select", "lasso"))

# Abemaciclib estimate----------------------------------------------------------

chart_title <- "<b>Observed vs expected usage of abemaciclib</b>"

nat_meds_df %>% 
    filter(treatment_name == "abemaciclib") %>% 
    group_by(date, treatment_name) %>% 
    summarise(numerator = sum(numerator),
              denominator = first(denominator),
              dd_per_100000 = numerator/(denominator/100000)) %>% 
    ungroup() %>% 
    plot_ly(x = ~date,
            y = ~dd_per_100000) %>% 
    add_trace(type = "scatter",
              mode = "lines+markers",
              fill = 'tozeroy') %>% 
    layout(title = list(text = stringr::str_wrap(chart_title, width = 95),
                        font = list(size = 12),
                        x = 0.05,
                        y = 0.96,
                        yanchor = "top"),
           yaxis = list(title = list(text = "Daily doses per 100,000 population",
                                     standoff = 5),
                        rangemode = "tozero",
                        range = list(0, 1200)),
           xaxis = list(title = FALSE,
                        tickformat = "%b<br>%Y"),
           hovermode = "x unified",
           margin = list(t = 65)) %>% 
    config(displaylogo = FALSE,
           modeBarButtonsToRemove = c("zoom", "pan", "select", "lasso")) %>% 
    layout(shapes = list(define_vline("2019-02-27"),
                         define_vline("2021-09-25"),
                         define_hline(1015))) %>% 
    label_vline("2019-02-27", "TA563") %>% 
    label_vline("2021-09-25", "TA725") %>% 
    label_hline(970, "Expected usage")

