
library(dplyr)
library(curl)
library(xml2)
library(rvest)

# The script below first makes an API request for all TAs, before extracting the 
# title and guidance_number for each TA. We can then filter for TAs that include 
# our medicines of interest in the title. Using this list we can then submit a 
# request for each of these TAs and extract the publication date.

# Make API request for all TAs --------------------------------------------

# Set the URL for the guidance of interest
url <- "https://api.nice.org.uk/services/guidance/current/programmes2/TA"

# Set up our handle to pass over the required authorisation details, and details of what we want (xml)
h <- new_handle() %>% 
    handle_setheaders("API-Key" = Sys.getenv("nice_api_key"),
                      "Accept" = "application/vnd.nice.syndication.guidance+xml")

# Submit the request to the API
req <- curl_fetch_memory(url,
                         handle = h)


# Parse XML to get TA title and shorthand ---------------------------------

xml_guideline <- xml2::read_xml(req$content)

ta_df <- xml_guideline %>% 
    xml_find_all("Links") %>% 
    xml_children() %>% 
    purrr::map_df(function(x){
        list(
            guidance_no = xml_attr(x, "uri") %>% 
                stringr::str_extract("TA[0-9]+"),
            guidance_title = xml_attr(x, "title")
        )
    }) %>% 
    tidyr::drop_na() %>% 
    filter(!stringr::str_detect(guidance_title, "(terminated appraisal)"))
    

# Filter for our medicines ------------------------------------------------

meds_regex <- "[Aa]bemaciclib|[Nn]eratinib|[Pp]albociclib|[Rr]ibociclib|[Tt]rastuzumab|[Tt]ucatinib"

ta_for_api <- ta_df %>% 
    filter(stringr::str_detect(guidance_title, meds_regex)) %>% 
    pull(guidance_no)

# Make second API call for all meds ---------------------------------------

# Create a vector of URLs, one for each piece of guidance to request
url_list <- paste0("https://api.nice.org.uk/services/guidance/documents/", ta_for_api)

# Set up our headers to pass over the required authorisation details, and details of what we want (xml)
headers <- list("API-Key" = Sys.getenv("nice_api_key"),
                "Accept" = "application/vnd.nice.syndication.guidance+xml")

# Set up the async request using the CRUL package
dd <- crul::Async$new(urls = url_list, headers = headers)

# Submit the request
res <- dd$get()

# Check that everything is a success
all(vapply(res, function(z) z$success(), logical(1)))

# Extract all of the xml documents into a list
async_out <- lapply(
    X = res, 
    FUN = function(x) {
        
        # Parse the response
        tmp_response <- x$parse("UTF-8")
        
        # Extract the records
        tmp_df <- read_xml(tmp_response)
    })

# Extract what we need from API output-----------------------------------------

ta_output_df <- async_out %>% 
    purrr::map_df(function(x) {
        list(
            guidance_no = xml_child(x, "GuidanceNumber") %>% xml_text(),
            title = xml_child(x, "Title") %>% xml_text(),
            pub_date = xml_child(x, "MetadataApplicationProfile/Issued") %>% xml_text() %>% stringr::str_sub(1,10),
            mod_date = xml_child(x, "MetadataApplicationProfile/Modified") %>% xml_text() %>% stringr::str_sub(1,10)
        )}) %>% 
    mutate(pub_date = lubridate::as_date(pub_date),
           mod_date = lubridate::as_date(mod_date))

#readr::write_csv(ta_output_df, here::here("data/clean/med_TA_list.csv"))
