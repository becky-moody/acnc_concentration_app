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

pacman::p_load(shiny, shinyWidgets, tidyverse, here, janitor, shinyjs, readxl, update = FALSE)
pacman::p_unload(here, janitor, readxl)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  sidebarLayout(
    sidebarPanel(
      useShinyjs(),
      fileInput("c_file", "Choose concentration file",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv",
                '.xlsx')
      ),
      tags$hr(),
      prettySwitch(
        inputId = "calculate",
        label = "Calculate?",
        status = "success",
        fill = TRUE,
        value = FALSE
      ),
      #checkboxInput("calculate", "Calculate?", FALSE),

      # Button
      shinyjs::hidden(p(id = "dl",
      downloadButton("download_data", "Download as csv?")
      ))

    ),
    mainPanel(
      dataTableOutput("contents")
    )
  )
))
