#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  df <- reactive({
    req(input$c_file)
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    inFile <- input$c_file

    if (is.null(inFile))
      return(NULL)

    cat('Reading file\n')
    suppressMessages(readxl::read_xlsx(inFile$datapath))


    #if(inFile$datapath)
    #read.csv(inFile$datapath, header = input$header)
  })

  calc_df <- eventReactive(input$calculate == TRUE,{
    req(input$c_file)
    cat('Running calculations\n')
    df <- df()
    # only want column 2 and 3; rename
    t <- df[,2:3] %>% janitor::clean_names()
    # get 10 column start and stop; +- 2 because using strings to identify these
    row_start_10 <- which(grepl('dsDNA', t$x2)) + 2
    row_stop_10 <- which(grepl('Ratio 260', t$x2)) -2

    # get 10 column from data
    df10 <- t[row_start_10:row_stop_10, 1] %>%
      rename(dna_conc = x2)
    # calculations
    df10_c <- df10 %>%
      mutate(dna_conc = as.numeric(dna_conc),
             # create this weight based on dna_conc size
             weight = case_when(
               dna_conc > 40 ~ 1,
               dna_conc > 30 & dna_conc <= 40 ~ 1/2,
               dna_conc <= 30 ~ 1/3,
               TRUE ~ 1
             ),
             total_dna = dna_conc * 30,
             ul_dna_needed = 2000/total_dna * 30 * weight,
             ul_water_needed = 100 * weight - ul_dna_needed,
             total_vol = ul_dna_needed + ul_water_needed,
             total_conc = ul_dna_needed * dna_conc / total_vol) %>%
      mutate(ul_dna_needed = round(ul_dna_needed, 2),
             ul_water_needed = round(ul_water_needed, 2))

    cat('finished with calculations.\n')

    df10_c
  })

  observe({
    shinyjs::toggle("dl", condition = input$calculate )
  })

  output$contents <- renderDataTable({
    calc_df()
  })



  output$download_data <- downloadHandler(
    filename = function() {
      paste("concentrations",Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(calc_df(), file, row.names = FALSE, col.names =TRUE)
    }
  )




})
