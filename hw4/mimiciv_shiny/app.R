# Biostat 203B HW4 Shiny App for MIMIC-IV ICU Cohort
# Author: Jiachen Ai, UID: 206182615

# Load necessary libraries
library(shiny)
library(ggplot2)
library(readr)
library(bigrquery)
library(dbplyr)
library(dplyr)
library(stringr)
library(tidyverse)
library(dqshiny)

# Path to the service account token
# Since the token is in the hw4 directory, 
# I go back one directory by "../"
satoken <- "../biostat-203b-2024-winter-313290ce47a6.json"

# BigQuery authentication using service account
bq_auth(path = satoken)

# Connect to the BigQuery database `biostat-203b-2024-winter.mimic4_v2_2`
con_bq <- dbConnect(
  bigrquery::bigquery(),
  project = "biostat-203b-2024-winter",
  dataset = "mimic4_v2_2",
  billing = "biostat-203b-2024-winter")

# Load required data from BigQuery database in advance 
# to avoid repeated queries
race <- tbl(con_bq, "admissions")
demographics <- tbl(con_bq, "patients") 
diagnoses_names <- tbl(con_bq, "d_icd_diagnoses")
top_3_diagnoses <- tbl(con_bq, "diagnoses_icd")
ADT <- tbl(con_bq, "transfers")
labevents <- tbl(con_bq, "labevents")
icd_procedures <- tbl(con_bq, "d_icd_procedures")
procedures <- tbl(con_bq, "procedures_icd")
chartevents <- tbl(con_bq, "chartevents")

# Since selection of items does not depend on the patient,
# I use collect() to store interested items in local.
items <- tbl(con_bq, "d_items") |>
  select(itemid, label, abbreviation) |>
  filter(abbreviation %in% c("HR", "NBPd", 
                             "NBPs", "RR", 
                             "Temperature F")) |>
  collect()

# Load MIMIC-IV cohort data
mimic_icu_cohort <- read_rds("./mimic_icu_cohort.rds")

# Get the unique patient IDs to be used in the autocomplete input
opts <- mimic_icu_cohort |>
  select(subject_id) |> 
  distinct() |> 
  pull() |>
  as.character()

# Define UI for application
ui <- fluidPage(
  titlePanel("MIMIC-IV ICU Cohort"),
  
  # design the tab layout
  tabsetPanel(
    
    # the first tab
    tabPanel("Patients' Summary Information", 
             
             # design the sidebar layout in the first tab
             sidebarLayout(
               
               # set the selection input for the first tab
               sidebarPanel(
                 
                 # define the first level of selection input
                 selectInput("summary", 
                             "Patient's Summary Information", 
                             choices = c("Demographics", 
                                         "Lab Measurements", 
                                         "Vitals")),
                 
                 # set conditional selection input for demographics
                 conditionalPanel(
                   condition = "input.summary == 'Demographics'",
                   selectInput("demographics", "Demographics", 
                               choices = c("age_intime", 
                                           "gender", 
                                           "race", 
                                           "insurance", 
                                           "marital_status", 
                                           "language"))
                 ),
                 
                 # define the conditional selection input 
                 # for lab measurements
                 conditionalPanel(
                   condition = "input.summary == 'Vitals'",
                   selectInput("vitals", "Vitals",
                               choices = c(
                                 "respiratory_rate",
                                 "systolic_non_invasive_blood_pressure",
                                 "heart_rate",
                                 "temperature_in_Fahrenheit", 
                                 "diastolic_non_invasive_blood_pressure"
                               ))
                 ),
                 
                 # Slider for the number of observations to generate
                 sliderInput("n",
                             "Number of observations:",
                             value = c(0, 500),
                             min = 1,
                             max = 1001)
                 
               ),
               
               
               
               # define the main panel for the first tab
               # and the output be plots
               mainPanel(
                 plotOutput("selected_summary")
               )
             )
    ),
    
    # design the second tab
    tabPanel("Each Patient's ADT & ICU Stay Information", 
             
             # design the sidebar layout in the second tab
             sidebarLayout(
               
               # set the text input for the second tab
               # expect the user to input the patient ID
               sidebarPanel(
                 
                 # define the text input for patient ID
                 # and use autocomplete input
                 autocomplete_input("patient_id", "Patient ID", 
                                    opts, max_options = 60000),
               ),
               
               # define the main panel for the second tab
               # and the output will be plots
               mainPanel(
                 plotOutput("ADT_history"),
                 plotOutput("vitals_line_plot")
               )
             )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Define the plot for the first tab
  output$selected_summary <- renderPlot({
    req(input$summary)  # Ensure input$summary has a value
    
    n <- input$n  # Store the number of observations
    
    # In order to have different plots for different selections,
    # I used `switch` to select the plot based on input$summary
    plot1 <- switch(input$summary,
                    
                    # If the user selects "Demographics"
                    "Demographics" = {
                      
                      # Here I used ifelse to plot 
                      # race and the rest separately
                      # because race has a long label
                      
                      # if the user selects race,
                      if (input$demographics == "race") {
                        
                        # then present the demographics bar plot
                        # with x-axis text rotated
                        ggplot(mimic_icu_cohort, 
                               aes_string(x = input$demographics)) +
                          geom_bar() +
                          labs(title = "Demographics statistics", 
                               x = input$demographics, 
                               y = "Count") +
                          theme_minimal() +
                          theme(axis.text.x = element_text(
                            angle = 90, vjust = 0.5, hjust = 1))}
                      
                      else if (input$demographics == "age_intime") {
                        ggplot(mimic_icu_cohort, 
                               aes_string(x = input$demographics)) +
                          geom_bar() +
                          
                          # set the x-axis limit to the number of observations
                          # to avoid the long tail
                          scale_x_continuous(limit = c(n, n)) +
                          labs(title = "Demographics statistics", 
                               x = input$demographics, 
                               y = "Count") +
                          theme_minimal()
                        
                        
                        # otherwise, present the other demographics plots
                      } else {
                        ggplot(mimic_icu_cohort, 
                               aes_string(x = input$demographics)) +
                          geom_bar() +
                          labs(title = "Demographics statistics", 
                               x = input$demographics, y = "Count") +
                          theme_minimal()
                      }
                    },
                    
                    # If the user selects "Lab Measurements"
                    "Lab Measurements" = {
                      
                      # Using pivot_longer to transform the interested
                      # lab measurements into long format
                      mimic_icu_cohort_long <- mimic_icu_cohort |>
                        pivot_longer(cols = c(sodium, chloride, 
                                              creatinine, potassium, 
                                              glucose, bicarbonate),
                                     names_to = "variable",
                                     values_to = "value")
                      
                      # Present the lab measurements box plot
                      ggplot(mimic_icu_cohort_long, 
                             aes(x = variable, y = value)) +
                        geom_boxplot() +
                        
                        # Set the y-axis limit to the number of observations
                        # so that the plot is not too crowded
                        scale_y_continuous(limit = c(n, n)) +
                        labs(title = "Lab Measurements statistics", 
                             x = "Lab Measurements", y = "Value") +
                        theme_minimal()
                    },
                    
                    # If the user selects "Vitals"
                    "Vitals" = {
                      
                      # Present the histogram of the selected vitals
                      ggplot(mimic_icu_cohort, 
                             aes_string(x = input$vitals)) +
                        
                        # set the x-axis limit to the number of observations
                        # so that the plot is not too crowded
                        scale_x_continuous(limits = c(n, n)) +
                        geom_histogram() +
                        labs(title = "Vitals statistics", 
                             x = input$vitals, y = "Count") +
                        theme_minimal()
                    }
    )
    
    # Return the plot
    plot(plot1)
  })
  
  
  
  # Define the plot for the second tab
  
  
  # First, define the plot for the ADT history
  output$ADT_history <- renderPlot({
    req(input$patient_id)
    
    # transform the input patient_id into numeric
    patient_id <- as.numeric(input$patient_id)
    
    # filter out the race info of the patient
    race <- race |>
      filter(subject_id == patient_id) |>
      collect() 
    
    # filter out other demographics info of the patient
    # and merge the race info with the them
    demographics <- demographics |>
      filter(subject_id == patient_id) |>
      collect() |>
      mutate(race = tolower(race$race[1]))
    
    # collect all diagnoses ids & names for matching
    diagnoses_names <- diagnoses_names |>
      select(icd_code, icd_version, long_title) |>
      collect()
    
    # filter out the top 3 diagnoses of the patient
    # and merge the diagnoses names with them
    top_3_diagnoses <- top_3_diagnoses |>
      filter(subject_id == patient_id) |>
      collect() |>
      left_join(diagnoses_names, 
                by = c("icd_code" = "icd_code",
                       "icd_version" = "icd_version")) |>
      
      # Select the top 3 diagnoses based on frequency
      count(long_title, sort = TRUE) |>
      slice(1:3) 
    
    # Get the patient's admission, discharge time, and care unit info
    ADT <- ADT |>
      filter(subject_id == patient_id) |>
      collect() |>
      
      # Add a new column segment_thickness 
      # to distinguish the ICU/CCU from other care units
      filter(!is.na(careunit)) |>
      mutate(segment_thickness = if_else(
        str_detect(careunit, "(ICU|CCU)"), 10, 6))
    
    # Change the data type of intime and outtime to POSIXct
    # to make them compatible with ggplot
    ADT$intime <- as.POSIXct(ADT$intime)
    ADT$outtime <- as.POSIXct(ADT$outtime)
    
    # select the lab events and the chart time of the patient
    labevents <- labevents |>
      select(subject_id, charttime) |>
      filter(subject_id == patient_id) |>
      distinct(subject_id, charttime) |>
      collect()
    
    # Get the procedures' icd codes and names
    icd_procedures <- icd_procedures |>
      collect()
    
    # filter out the procedures of the patient
    procedures <- procedures |>
      filter(subject_id == patient_id) |>
      collect() |>
      left_join(icd_procedures, 
                by = c("icd_code" = "icd_code",
                       "icd_version" = "icd_version")) 
    
    # Change the data type of chartdate to POSIXct
    # to make it compatible with ggplot
    procedures$chartdate <- as.POSIXct(procedures$chartdate)
    
    
    
    # Plot the ADT history
    ggplot() +
      
      # Use geom_segment to plot the Admission-Discharge-Time,
      geom_segment(data = ADT, 
                   mapping = aes(x = intime, 
                                 xend = outtime, 
                                 y = "ADT", 
                                 yend = "ADT", 
                                 color = careunit, 
                                 size = segment_thickness),
                   na.rm = TRUE) +
      
      # Use geom_point to plot the lab events
      geom_point(data = labevents, 
                 mapping = aes(x = charttime, 
                               y = "Lab"),
                 shape = 3, size = 2) +
      
      # Use geom_point to plot the procedures info
      geom_point(data = procedures, 
                 aes(x = chartdate, 
                     y = "Procedure", 
                     
                     # Get shorter title by regular expression
                     shape = sub(",.*", "", long_title)),
                 size = 3) +
      
      # Manually set the number of shapes according to the number of titles
      # For reproducibility, I use n_distinct to determine 
      # the number of shapes
      scale_shape_manual(
        values = c(1:n_distinct(procedures$long_title))) +
      
      # Set legend position and arrangement
      theme_bw() +
      theme(legend.position = "bottom", 
            legend.box = "vertical", 
            legend.key.size = unit(0, "pt"),
            legend.text = element_text(size = 7)) +
      
      # Set legend titles and arrangement
      guides(color = guide_legend(title = "Care Unit", 
                                  ncol = 3,
                                  keywidth = 1),
             shape = guide_legend(title = "Procedure", 
                                  ncol = 2),
             
             # Remove ADT's legend
             size = FALSE) +
      
      # Add patient information as title and subtitle
      labs(title = paste("Patient", 
                         demographics$subject_id[1], ", ",
                         demographics$gender[1], ", ",
                         demographics$anchor_age[1], "years old, ",
                         demographics$race[1]),
           subtitle = paste(top_3_diagnoses$long_title[1],
                            top_3_diagnoses$long_title[2],
                            top_3_diagnoses$long_title[3],
                            sep = "\n"),
           x = "Calendar Time",
           y = "",
           color = "Care Unit",
           shape = "Procedure") +
      
      # Specify y axis with 3 levels
      scale_y_discrete(limits = c("Procedure", "Lab", "ADT"))
  })
  
  
  # Secondly, plot the vital signs of the patient
  output$vitals_line_plot <- renderPlot({
    req(input$patient_id)
    
    # transform the input patient_id into numeric
    patient_id <- as.numeric(input$patient_id)
    
    # filter out the interested vital signs of the patient
    chartevents <- chartevents |>
      filter(subject_id == patient_id) |>
      filter(itemid %in% c(220045, 220179, 
                           220180, 220210, 
                           223761)) |>
      filter(subject_id == patient_id) |>
      
      # exclude unnecessary columns
      select(-c(hadm_id, caregiver_id, storetime, warning)) |>
      collect() |>
      left_join(items, by = c("itemid" = "itemid")) 
    
    
    # Get the plot of the vital signs
    ggplot(chartevents,
           aes(x = charttime,
               y = valuenum,
               color = abbreviation)) +
      geom_line() +
      geom_point() +
      
      # use facet_grid to show all combinations of abbreviation and stay_id
      facet_grid(abbreviation ~ stay_id, scales = "free") +
      labs(title = paste("Patient", 
                         patient_id, 
                         "ICU stays - Vitals"),
           x = "",
           y = "") +
      theme_light(base_size = 9) +
      
      # remove legend
      theme(legend.position = "none") +
      guides(fill = 'none') +
      
      # To avoid overlapping of the x-axis labels, 
      # using guides with n.dodge = 2
      scale_x_datetime(
        guide = guide_axis(n.dodge = 2))
  })
}

# Finally, run the application successfully
shinyApp(ui = ui, server = server)
