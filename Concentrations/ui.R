#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

if (!require("pacman")) {
 install.packages("pacman") }
library(pacman)

#pacman::p_load(shiny, tidyverse, here, janitor, shinyjs, readxl, rhandsontable, update = FALSE)
#pacman::p_unload(here, janitor, readxl)
library(shiny)
#library(shinyWidgets)
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

        fileInput("c_file", h3("Choose concentration file"),
                            accept = c(
                              # "text/csv",
                              # "text/comma-separated-values,text/plain",
                              # ".csv",
                              '.xlsx')
        ),

      tags$hr(style="border-color: black;"),
      div(
        style = "width: 300px; margin: 10px;",

      DTOutput('sample_id_df'),
      br(),
      textOutput('sample_id_note')
      #checkboxInput("run_sample_ids", label = "Upload sample names?", value = FALSE)
    ),

    tags$hr(style="border-color: black;"),
    div(style = "lightblue;margin: auto;",
        fluidRow(
          numericInput("start_vol", label = ("Starting Volume?"), value = 30),
          numericInput("final_conc", label = ("Final Concentration Needed?"), value = 20),
          numericInput("final_vol", label = ("Final Volume Needed?"), value = 100)
        ),
        textOutput('vol_warning')),
    br(),

tags$hr(style="border-color: black;"),

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
    mainPanel(
      dataTableOutput("contents")
    )
  )
))
