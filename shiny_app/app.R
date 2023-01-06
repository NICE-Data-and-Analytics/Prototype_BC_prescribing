
# Load packages -----------------------------------------------------------

library(dplyr)
library(shiny)
library(shinydashboard)
library(reactable)
library(shinyWidgets)
library(shinythemes)
library(plotly)


# Source in other files ---------------------------------------------------

source(here::here("scripts/rmd_functions.R"))

# Load in TA table
meds_ta_df <- readr::read_csv(here::here("data/clean/meds_ta_df.csv"), 
                              col_select = 1:3)

# Load in national level data
nat_meds_df <- readr::read_csv(here::here("data/clean/bc_meds_nat_2022_10.csv"))

# Load in indications info
indications_df <- readr::read_csv(here::here("data/clean/bc_med_emc_indications.csv"))


# App ---------------------------------------------------------------------

# Define UI for application that draws a histogram

ui <- dashboardPage(skin = "yellow",
    
    dashboardHeader(title = "Breast cancer medicines",
                    titleWidth = 250),
    
    dashboardSidebar(width = 250,
        
        hr(),
        
        sidebarMenu(id="tabs",
                    menuItem("Background", tabName = "background", icon = icon("bookmark"), selected = TRUE),
                    menuItem("Medicines data", tabName = "meds_data", icon = icon("table")),
                    menuItem("About", tabName = "about", icon = icon("question"))),
        
        hr(),
        
        conditionalPanel("input.tabs == 'meds_data'",
                        fluidRow(
                            column(1),
                            column(10,
                                   selectInput("medicine",
                                               "Select medicine",
                                               choices = setNames(
                                                   unique(nat_meds_df$treatment_name),
                                                   stringr::str_to_title(unique(nat_meds_df$treatment_name))))
                                   )
                            )
                        )
        ),
    
    dashboardBody(
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
        
        tabItems(

            tabItem(tabName = "background"),
            
            tabItem(tabName = "meds_data",
                    tabsetPanel(
                        
                        tabPanel("Observed Usage",
                                 fluidRow(
                                     fixedRow(1),
                                     column(width = 7,
                                            box(width = NULL, title = "Plot", solidHeader = TRUE,
                                            plotlyOutput("usage_chart", height = "500px", width = "auto"))),
                                     column(width = 5,
                                            box(width = NULL, title = "Therapeutic indications", solidHeader = TRUE,
                                                htmlOutput("indications")))),
                                 #br(),
                                 fluidRow(
                                     column(width = 12,
                                            box(width = NULL, title = "Relevant guidance", solidHeader = TRUE,
                                                reactableOutput("TA_table"))))
                                 
                                 ),
                        
                        tabPanel("Underlying Data",
                                 br(),
                                 box(width = NULL, title = "", solidHeader = TRUE,
                                 reactableOutput("data_table"))),
                        
                        tabPanel("Eligible population")
                    )
            ),

            tabItem(tabName = "about")

            )
        )
    )

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$usage_chart <- renderPlotly({
        create_med_plot(nat_meds_df, 
                        medicine = input$medicine,
                        ylabel = "People") %>% 
            add_TA_lines(meds_ta_df, nat_meds_df, input$medicine)
    })
    
    output$data_table <- renderReactable({
        create_data_table(nat_meds_df, input$medicine)
    })
    
    output$TA_table <- renderReactable({
        create_med_TA_table(meds_ta_df, input$medicine)
    })
    
    output$indications <- renderText({
        write_indication_text(indications_df, input$medicine)
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
