
# Load packages -----------------------------------------------------------

library(dplyr)
library(plotly)

# Load data ---------------------------------------------------------------

nat_meds_df <- readr::read_csv(here::here("data/bc_meds_nat_2022_10.csv"))
stp_meds_df <- readr::read_csv(here::here("data/bc_meds_stp_2022_10.csv"))
meds_ta_df <- readr::read_csv(here::here("data/bc_med_tas_df.csv"))


# Functions for lines ------------------------------------------------------

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

