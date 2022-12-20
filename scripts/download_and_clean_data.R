
library(dplyr)
library(stringr)
library(ggplot2)

# prevents data loading in with numbers in scientific format
options(scipen=999)

# Import and clean data ---------------------------------------------------

## The below code shows how you can import the data straight from the website. 
# To get the specific URL I right clicked the ZIP file on the NHSD website and clicked copy link address.
temp <- tempfile()
download.file("https://files.digital.nhs.uk/AE/807A0E/Output-Utilisation.zip", temp)

# Read in the national csv and prepare for charting
national_df <- readr::read_csv(unz(temp, "nice-tech-apps-eng-apr22-Nat-Utilisation.csv"))
    
clean_df <- national_df %>% 
    # filter the data to get only abemaciclib and neratinib
    filter(str_detect(treatment_name, "abemaciclib|neratinib")) %>% 
    # Group the data by the columns we want to keep
    group_by(year, quarter, year_quarter, treatment_name, 
             numerator_unit, denominator_unit, value_unit) %>% 
    # Sum the numerator to include both prim/sec care, take the first population value as it is same for all, and
    # recalculate the ADDs per 100,000
    summarise(numerator = sum(numerator),
              denominator = first(denominator),
              value = numerator/(denominator/100000)) %>% 
    # Add ungroup to make sure the modified table isn't saved as a grouped table. This can cause issues when graphing
    ungroup() %>% 
    # Calculate people and people per 100,000 population
    mutate(people = numerator/91,
           people_per_100000 = people/(denominator/100000)) %>% 
    # This is a very convoluted way of converting the year and quarter columns into a date, this will make 
    # graphing much easier as we can use the date column on the x axis
    mutate(year = as.character(year),
           quarter = as.character(quarter),
           year = case_when(
               quarter  %in% c("1", "2", "3") ~ paste0("20", str_sub(year, 3, 4)),
               quarter == "4" ~ paste0("20", str_sub(year, 6, 7))),
           quarter = case_when(
               quarter == "4" ~ "3",
               quarter == "3" ~ "12",
               quarter == "2" ~ "9",
               quarter == "1" ~ "6"),
           date = lubridate::make_date(year, quarter, "1")) %>% 
    # remove the columns we dont need and move date to start
    select(-c(year, quarter)) %>% 
    select(date, everything()) 


# Make chart --------------------------------------------------------------

# Simple chart made using data
clean_df %>% 
    filter(str_detect(treatment_name, "abemaciclib")) %>% 
    ggplot(aes(x = date, y = people)) +
    geom_line(color = "#228096", size = 1) +
    geom_point(size = 2, color = "#228096") +
    geom_vline(xintercept = lubridate::make_date("2019", "02", "27"), 
               linetype = "dashed", 
               colour = "black", size = 0.6)+
    theme_minimal()
