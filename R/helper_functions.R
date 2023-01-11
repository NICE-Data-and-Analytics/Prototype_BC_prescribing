
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
