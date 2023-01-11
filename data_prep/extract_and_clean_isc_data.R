
library(dplyr)
library(stringr)

options(scipen=999)


# Import data -------------------------------------------------------------

## The below code shows how you can import the data straight from the website. 
# To get the specific URL I right clicked the ZIP file on the NHSD website and clicked copy link address.
temp <- tempfile()
download.file("https://files.digital.nhs.uk/6A/C6063C/Output-Utilisation.zip", temp)

# Read in the national and STP csv files and prepare for charting
nat_meds_df <- readr::read_csv(unz(temp, "nice-tech-apps-eng-oct22-Nat-Utilisation.csv"))
stp_meds_df <- readr::read_csv(unz(temp, "nice-tech-apps-eng-oct22-STP-Utilisation.csv"))

# Read in STP to region lookup file downloaded from ONS Open Geography Portal. This will map STPs to regions.
# This is also useful because it gives us the GSS code column which we can use to join to ONS shape files 
# if we want to make heatmaps
stp_to_reg_lookup <- readr::read_csv(here::here("data/STP21_NHSER21_EN_LU.csv"), 
                                     col_names = c("stp_gss_cd", "stp_ods_cd", "stp_name", 
                                                   "reg_gss_cd", "reg_ods_cd", "reg_name")) %>% 
    select(-stp_name)


# Wrangle national data --------------------------------------------------------

nat_meds_df <- nat_meds_df %>% 
    select(-c(data_type, treatment_type, provider_code, provider_name, high_level_condition, value, value_unit)) %>% 
    filter(treatment_name %in% c("abemaciclib", "neratinib", "palbociclib", 
                                 "ribociclib", "trastuzumab deruxtecan", 
                                 "trastuzumab emtansine", "tucatinib")) %>% 
    # This is a very convoluted way of converting the year and quarter columns into a date, this will make 
    # graphing much easier as we can use the date column on the x axis
    mutate(quarter = as.character(quarter),
           year = case_when(
               quarter  %in% c("1", "2", "3") ~ paste0("20", str_sub(year, 3, 4)),
               quarter == "4" ~ paste0("20", str_sub(year, 6, 7))),
           quarter = case_when(
               quarter == "4" ~ "3",
               quarter == "3" ~ "12",
               quarter == "2" ~ "9",
               quarter == "1" ~ "6"),
           date = lubridate::make_date(year, quarter, "1"), .before = year,
           # Make date last day of quarter
           date = lubridate::ceiling_date(date, "month") - lubridate::days(1)) %>% 
    # remove the columns we dont need and move date to start
    select(-c(year, quarter, year_quarter)) %>% 
    # Make the unit conversions to daily doses
    # Abemaciclib, palbociclib, tucatinib already in daily doses
    mutate(
        numerator = case_when(
            # Neratinib - daily dose = 240mg
            treatment_name == "neratinib" ~ numerator/240,
            # Ribociclib- 1 tablet = 200mg, daily dose = 450mg
            treatment_name == "ribociclib" ~ (numerator*200)/450,
            # Trastuzumab deruxtecan - 1 vial = 100mg, daily dose = 18.59mg 
            treatment_name == "trastuzumab deruxtecan" ~ (numerator*100)/18.59,
            # Trastuzumab emtansine - 1 vial = 130mg (average,  assumes equal use of 100 & 160 vial) daily dose = 12.39mg
            treatment_name == "trastuzumab emtansine" ~ (numerator*130)/12.39,
            TRUE ~ numerator),
        # Amend the numerator units values
        numerator_unit = case_when(
            treatment_name %in% c("neratinib", "ribociclib", "trastuzumab deruxtecan", "trastuzumab emtansine") ~ "ADD",
            TRUE ~ numerator_unit),
        numerator = round(numerator, 0))

#readr::write_csv(nat_meds_df, here::here("data/bc_meds_nat_2022_10.csv"))

# Wrangle STP data -----------------------------------------------------------

stp_meds_df <- stp_meds_df %>% 
    select(-c(data_type, treatment_type, high_level_condition, value, value_unit)) %>% 
    filter(treatment_name %in% c("abemaciclib", "neratinib", "palbociclib", 
                                 "ribociclib", "trastuzumab deruxtecan", 
                                 "trastuzumab emtansine", "tucatinib")) %>% 
    # This is a very convoluted way of converting the year and quarter columns into a date, this will make 
    # graphing much easier as we can use the date column on the x axis
    mutate(quarter = as.character(quarter),
           year = case_when(
               quarter  %in% c("1", "2", "3") ~ paste0("20", str_sub(year, 3, 4)),
               quarter == "4" ~ paste0("20", str_sub(year, 6, 7))),
           quarter = case_when(
               quarter == "4" ~ "3",
               quarter == "3" ~ "12",
               quarter == "2" ~ "9",
               quarter == "1" ~ "6"),
           date = lubridate::make_date(year, quarter, "1"), .before = year,
           # Make date last day of quarter
           date = lubridate::ceiling_date(date, "month") - lubridate::days(1)) %>% 
    # remove the columns we dont need and move date to start
    select(-c(year, quarter, year_quarter)) %>% 
    # Change col names ahead of join
    rename(stp_ods_cd = provider_code,
           stp_name = provider_name) %>% 
    # Join to region lookup table
    left_join(stp_to_reg_lookup, by = c("stp_ods_cd" = "stp_ods_cd")) %>% 
    # Change column order
    select(date, data_source, treatment_name, stp_ods_cd, stp_gss_cd, stp_name, 
           reg_ods_cd, reg_gss_cd, reg_name, everything()) %>% 
    # Make the unit conversions to daily doses
    # Abemaciclib, palbociclib, tucatinib already in daily doses
    mutate(
        numerator = case_when(
            # Neratinib - daily dose = 240mg
            treatment_name == "neratinib" ~ numerator/240,
            # Ribociclib- 1 tablet = 200mg, daily dose = 450mg
            treatment_name == "ribociclib" ~ (numerator*200)/450,
            # Trastuzumab deruxtecan - 1 vial = 100mg, daily dose = 18.59mg 
            treatment_name == "trastuzumab deruxtecan" ~ (numerator*100)/18.59,
            # Trastuzumab emtansine - 1 vial = 130mg (average,  assumes equal use of 100 & 160 vial) daily dose = 12.39mg
            treatment_name == "trastuzumab emtansine" ~ (numerator*130)/12.39,
            TRUE ~ numerator),
        # Amend the numerator units values
        numerator_unit = case_when(
            treatment_name %in% c("neratinib", "ribociclib", "trastuzumab deruxtecan", "trastuzumab emtansine") ~ "ADD",
            TRUE ~ numerator_unit),
        numerator = round(numerator, 0))

#readr::write_csv(stp_meds_df, here::here("data/bc_meds_stp_2022_10.csv"))
