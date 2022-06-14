# User interface for
#############################-
## Concentration App ##
#############################-
## need special file with dna concentrations

if (!require("pacman")) {
 install.packages("pacman") }
library(pacman)

#pacman::p_load(shiny, tidyverse,here, janitor, shinyjs, readxl, DT, update = FALSE)
#pacman::p_unload(here, janitor, readxl)
library(shiny)
library(tidyverse)
library(here)
library(janitor)
library(shinyjs)
library(readxl)
library(DT)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  sidebarLayout(
    sidebarPanel(
      useShinyjs(),

      ## change this from only xlsx eventually; not sure what the files usually are
        fileInput("c_file", h3("Choose concentration file"),
                            accept = c(
                              # "text/csv",
                              # "text/comma-separated-values,text/plain",
                              # ".csv",
                              '.xlsx')
        ),

      ## line
      tags$hr(style="border-color: black;"),
      ## sample id table - editable
      div(
        style = "width: 300px; margin: 10px;",
      DTOutput('sample_id_df'),
      br(),
      textOutput('sample_id_note')
      #checkboxInput("run_sample_ids", label = "Upload sample names?", value = FALSE)
    ),

    ## line
    tags$hr(style="border-color: black;"),
    ## options
    div(style = "lightblue;margin: auto;",
        fluidRow(
          numericInput("start_vol", label = ("Starting Volume?"), value = 30),
          numericInput("final_conc", label = ("Final Concentration Needed?"), value = 20),
          numericInput("final_vol", label = ("Final Volume Needed?"), value = 100)
        ),
        textOutput('vol_warning')),
    br(),

    ## line
    tags$hr(style="border-color: black;"),
    ## calc button
    actionButton(
      inputId = "calculate",
      label = "Calculate?", width = '90%'),
    br(),
    br(),

    # Button to download data as csv
    shinyjs::hidden(p(id = "dl",
        br(),
        downloadButton("download_data", "Download as csv?")
    ))



    ),
    ## actual table output
    mainPanel(
      dataTableOutput("contents")
    )
  )
))
