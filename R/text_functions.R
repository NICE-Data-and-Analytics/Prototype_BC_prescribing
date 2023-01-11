

# Therapeutic indications text-------------------------------------------------

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


# Eligible populaton text -----------------------------------------------------

write_estimate_text <- function(estimate_text_df, medicine){
    
    med_reg <- med_regex(medicine)
    
    temp_df <- estimate_text_df %>% 
        filter(stringr::str_detect(medicine, med_reg))
    
    estimate_text <- temp_df[,"text"] %>% pull()
    
    return(estimate_text)
}

