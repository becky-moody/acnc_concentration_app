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

  ## create base sample id table; same layout as excel file
  sample_id_df <- data.frame(row.names =  LETTERS[1:8],
                          x10 = rep(as.character(NA),8),
                          x11 = rep(as.character(NA),8))
  ## this is going to be updated as datatable is edited
  s_df <- sample_id_df

  ## what's getting shown on page
  output$sample_id_df <- renderDT({
  datatable(s_df,
            caption = 'Sample IDs - double click to edit',
            editable = list(target='cell', disable = list(columns = c(0)),
                            numeric = 'none'),
            ## if target = 'column' must do ctrl + enter to save
            ## disable 0 makes rownames uneditable

            ## force size
            width = 100, height = 100,
            ## add lines
            class = 'cell-border stripe',
            ## does have rownaees
            rownames = TRUE,
            selection = 'single',
      ## don't want filter option; adjusting table size; or the extra stuff from DT
            filter = 'none', fillContainer = FALSE,
            options = list(dom = 't',ordering=F))
  })

  ## any edit in table update s_df
  observeEvent(input$sample_id_df_cell_edit, {
    s_df <<- editData(s_df, input$sample_id_df_cell_edit, 'sample_id_df')
  })
  ## to update output table "calculate" button must be hit again
  output$sample_id_note <- renderText('Note: will have to recalulate if any changes to sample ids.')

  ## text to show under final volume input
  output$vol_warning <- renderText('Note: Final volume might change if the required volume from stock is larger than what is available.')

  ## file upload and read the file;
  ## currently on xlsx; will probably need to change this
  df <- reactive({
    req(input$c_file)
    inFile <- input$c_file
    if (is.null(inFile))
      return(NULL)
    cat('Reading file\n')
    suppressMessages(readxl::read_xlsx(inFile$datapath))
  })

  ## get the data out of the file; only a certain area
  plate <- eventReactive(input$calculate,{
    req(input$c_file)
    df <- df()
    # only want columns 1,2,3; rename
    t <- df[,1:3] %>% janitor::clean_names()
    # get 10 column start and stop; +- 2 because using strings to identify these
    ## can probably change this because I think it's always going to be the same
    row_start_10 <- which(grepl('dsDNA', t$x2)) + 1
    row_stop_10 <- which(grepl('Ratio 260', t$x2)) - 2

    # get 10 column from data
    df10 <- t[row_start_10:row_stop_10, ] %>%
      janitor::row_to_names(row_number = 1) %>%
      janitor::clean_names() %>% rename(r = na)
    df10
  })

  ## build table for plate
  output$plate <- renderDataTable({
    data.frame(r = plate()$r, '10' = NA, '11'=NA)
    })


  calc_df <- eventReactive(input$calculate,{
    ## requre the file uplaod and options
    req(input$c_file)
    req(input$start_vol)
    req(input$final_conc)
    req(input$final_vol)

    df10 <- plate()
    cat('Running calculations\n')

    df10 <- df10 %>%
      ## make long df
      pivot_longer(.,cols = c(x10, x11),
                   values_to = 'dna_concentration',
                   # force values to be numeric
                   values_transform = list(dna_concentration= as.numeric))

    ## function for calculation
    weight.calc <- function(dna_conc,
                            start_vol = input$start_vol,
                            final_conc= input$final_conc,
                            final_vol = input$final_vol){

      ## no dna concentration = no results
      if(is.na(dna_conc)){
        return(data.frame(dna_need = NA, total_dna = NA, water_need = NA,
                          final_vol = NA, final_conc= NA, note = NA))
      } else if(final_conc > dna_conc){
        ## can't dilute to a larger concentration obviously
        ## this outputs negative water; might change to NAs too
        return(data.frame(dna_need = NA, total_dna = NA, water_need = NA,
                          final_vol = NA, final_conc = final_conc,
                          note = 'dilution must be less than stock concentration'))
      } else {
        #   ## formula c1*v1 = c2*v2
        #   calc_vol <- final_vol
        #   dna_need <- (final_conc * calc_vol)/dna_conc
        #   water_need <- calc_vol - dna_need

        ## make vars so nothing changes
        calc_vol <- final_vol
        total_dna <- dna_conc*start_vol
        dna_need <- (final_conc * calc_vol)/total_dna * start_vol
        water_need <- calc_vol - dna_need
        #conc_check <- (dna_need * dna_conc)/(water_need + dna_need)

        ## if not enough dna need available then reduce final volume by .5 then 1/3
        if(dna_need > dna_conc){
          calc_vol <- final_vol * .5
          total_dna <- dna_conc*start_vol
          dna_need <- (final_conc * calc_vol)/total_dna * start_vol
          water_need <- calc_vol - dna_need
          #conc_check <- (dna_need * dna_conc)/(water_need + dna_need)

          if(dna_need > dna_conc){
            calc_vol <- final_vol * 1/3
            total_dna <- dna_conc * start_vol
            dna_need <- (final_conc * calc_vol)/total_dna * start_vol
            water_need <- calc_vol - dna_need
            #conc_check <- (dna_need * dna_conc)/(water_need + dna_need)

            # if still too large then there's not enough dna
            if(dna_need > dna_conc){
              return(data.frame(dna_need = NA, total_dna = NA, water_need = NA,
                                final_vol = NA, final_conc = NA, note = 'dilution must be less than stock concentration'))
            }
          }
        }
      }

        return(data.frame(dna_need = dna_need, total_dna = total_dna,
                                              water_need = water_need,
                                              final_vol = calc_vol, final_conc = final_conc, note = NA))
}

    #   ## can't dilute to a larger concentration obviously
    #   ## this outputs negative water; might change to NAs too
    #   if(final_conc > dna_conc){
    #     return(data.frame(dna_need = dna_need, total_dna = total_dna, water_need = water_need,
    #                       final_vol = calc_vol, final_conc = final_conc,
    #                       note = 'dilution must be less than start concentration'))
    #   }
    #   return(data.frame(dna_need = dna_need, total_dna = total_dna,
    #                     water_need = water_need,
    #                     final_vol = calc_vol, final_conc = final_conc, note = NA))
    # }
    f_start_vol <- eventReactive(input$start_vol,{
      input$start_vol
    })
    f_final_conc <- eventReactive(input$final_conc,{
      input$final_conc
    })
    f_final_vol <- eventReactive(input$final_vol,{
      input$final_vol
    })


    df10_c <- df10 %>%
      ## map function to each row of pivoted table
      mutate(purrr::pmap_dfr(list(dna_conc = df10$dna_concentration,
                                  final_conc = f_final_conc(),#input$final_conc,
                                  final_vol = f_final_vol(),#input$final_vol,
                                  start_vol = f_start_vol()),#input$start_vol),
                             weight.calc)) %>%
      ## force rounding to 2 decimal places
      mutate(across(where(is.numeric), round, 2)) %>%
      ## remove the x (numeric col names are a no go in R)
      mutate(name = gsub('x','',name)) %>%
      ## sort; don't really need this
      arrange(name, r) %>%
      ## remove any missing sections from plate
      filter(!is.na(dna_concentration)) %>%
      ## this will be for joining to sample ids
      mutate(samp_space = paste0(r, name))

    ## fix sample id df
    samples <- s_df %>%
      ## make this a column instead
      rownames_to_column('r') %>%
      ## pivot to long df
      pivot_longer(cols = c('x10','x11'),
                   names_to = 'name', values_to ='sample_id') %>%
      ## remove the x and create the var for joining
      mutate(name = gsub('x','',name),
             samp_space = paste0(r, name))

    ## join with the calculated table; order columns
    out_df <- df10_c %>%
      left_join(., samples, by = c('r','name','samp_space')) %>%
      select(sample_id, r, name,
             stock_dna_concentration = dna_concentration,
             total_dna,
             volume_from_stock = dna_need,
             water_volume = water_need,
             final_volume = final_vol,
             final_concentration = final_conc, note)

    cat('finished with calculations.\n')

    #df10_c
    out_df
  })

  ## download button only shows up after you hit calculate once
  observe({
    shinyjs::toggle("dl", condition = (input$calculate>0))
  })

  ## rendering the calculated table; not allowing edits of this
  output$contents <- DT::renderDT({
    DT::datatable(calc_df(), options= list(dom = 't',pageLength = 20),
                  editable = FALSE, class = 'cell-border stripe',
                  rownames = FALSE, selection = 'single')
  })#, editable = TRUE)


  ## download as csv
  output$download_data <- downloadHandler(
    filename = function() {
      paste("concentrations",Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(calc_df(), file, row.names = FALSE, col.names =TRUE)
    }
  )




})
