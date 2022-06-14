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

  sample_id_df <- data.frame(row.names =  LETTERS[1:8],
                          x10 = rep(as.character(NA),8),
                          x11 = rep(as.character(NA),8))
  s_df <- sample_id_df

  output$sample_id_df <- renderDT({
  datatable(s_df,
            editable = list(target='cell', disable = list(columns = c(0)),
                            numeric = 'none'),
            ## if target = 'column' must do ctrl + enter to save
            filter = 'none',
            caption = 'Sample IDs - double click to edit',
            fillContainer = FALSE,
            width = 100, height = 100, selection = 'single',
            class = 'cell-border stripe', rownames = TRUE,
            options = list(dom = 't',ordering=F))
  })

  observeEvent(input$sample_id_df_cell_edit, {
    #cat('yes\n')
    #print(input$sample_id_df_cell_edit)
    #s_df[input$sample_id_df_cell_edit$row, input$sample_id_df_cell_edit$col] <<- input$sample_id_df_cell_edit$value
    s_df <<- editData(s_df, input$sample_id_df_cell_edit, 'sample_id_df')
  })
  output$sample_id_note <- renderText('Note: will have to recalulate if any changes to sample ids.')

  ## text to show under final volume input
  output$vol_warning <- renderText('Note: Final volume might change if the dna volume is not large enough.')

  ## only run after file upload
  df <- reactive({
    req(input$c_file)
    inFile <- input$c_file
    if (is.null(inFile))
      return(NULL)
    cat('Reading file\n')
    suppressMessages(readxl::read_xlsx(inFile$datapath))
  })




  plate <- eventReactive(input$calculate > 0,{
    req(input$c_file)
    df <- df()
    # only want column 2 and 3; rename
    t <- df[,1:3] %>% janitor::clean_names()
    # get 10 column start and stop; +- 2 because using strings to identify these
    row_start_10 <- which(grepl('dsDNA', t$x2)) + 1
    row_stop_10 <- which(grepl('Ratio 260', t$x2)) - 2


    #plate_ids <- data.frame(p = LETTERS[1:8], x10 = NA, x11 = NA)
    #plate <- pivot_longer(plate_ids, cols = c(x10, x11))

    # get 10 column from data
    df10 <- t[row_start_10:row_stop_10, ] %>%
      janitor::row_to_names(row_number = 1) %>%
      janitor::clean_names() %>% rename(r = na)
    df10
  })

  output$plate <- renderDataTable({
    data.frame(r = plate()$r, '10' = NA, '11'=NA)
    })


  calc_df <- eventReactive(input$calculate > 0,{
    req(input$c_file)
    req(input$start_vol)
    req(input$final_conc)
    req(input$final_vol)

    df10 <- plate()
    cat('Running calculations\n')

    df10 <- df10 %>% pivot_longer(.,cols = c(x10, x11),values_to = 'dna_concentration',
                   # force values to be numeric
                   values_transform = list(dna_concentration= as.numeric))

    weight.calc <- function(dna_conc,
                            start_vol = input$start_vol,
                            final_conc= input$final_conc,
                            final_vol = input$final_vol){

      if(is.na(dna_conc)){
        return(data.frame(dna_need = NA, total_dna = NA, water_need = NA,
                          final_vol = NA, final_conc= NA, note = NA))
      } else {
        #   ## formula c1*v1 = c2*v2
        #   calc_vol <- final_vol
        #   dna_need <- (final_conc * calc_vol)/dna_conc
        #   water_need <- calc_vol - dna_need

        calc_vol <- final_vol
        total_dna <- dna_conc*start_vol
        dna_need <- (final_conc * calc_vol)/total_dna * start_vol
        water_need <- calc_vol - dna_need
        #conc_check <- (dna_need * dna_conc)/(water_need + dna_need)

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

            if(dna_need > dna_conc){
              return(data.frame(dna_need = NA, total_dna = NA, water_need = NA,
                                final_vol = NA, final_conc = NA, note = 'not enough dna'))
            }
          }
        }
      }
      if(final_conc > dna_conc){
        return(data.frame(dna_need = dna_need, total_dna = total_dna, water_need = water_need,
                          final_vol = calc_vol, final_conc = final_conc,
                          note = 'dilution must be less than start concentration'))
      }
      return(data.frame(dna_need = dna_need, total_dna = total_dna, water_need = water_need,
                        final_vol = calc_vol, final_conc = final_conc, note = NA))
    }

    df10_c <- df10 %>%
      mutate(purrr::pmap_dfr(list(dna_conc = df10$dna_concentration, final_conc = input$final_conc, final_vol = input$final_vol), weight.calc)) %>%
      mutate(across(where(is.numeric), round, 2)) %>%
      mutate(name = gsub('x','',name)) %>%
      arrange(name, r) %>%
      filter(!is.na(dna_concentration)) %>%
      mutate(samp_space = paste0(r, name))

    samples <- s_df %>%rownames_to_column('r') %>%
      pivot_longer(cols = c('x10','x11'),
                   names_to = 'name', values_to ='sample_id') %>%
      mutate(name = gsub('x','',name),
             samp_space = paste0(r, name))

    out_df <- df10_c %>%
      left_join(., samples, by = c('r','name','samp_space')) %>%
      select(sample_id, r, name, dna_concentration,
             dna_need, total_dna, water_need, final_vol,
             final_conc, note)

    cat('finished with calculations.\n')

    #df10_c
    out_df
  })

  observe({
    shinyjs::toggle("dl", condition = (input$calculate>0))
  })

  output$contents <- DT::renderDT({
    #s <- input$sample_id_df_rows_selected
    DT::datatable(calc_df(), options= list(dom = 't',pageLength = 20),
                  editable = FALSE, class = 'cell-border stripe',)
  })#, editable = TRUE)



  output$download_data <- downloadHandler(
    filename = function() {
      paste("concentrations",Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(calc_df(), file, row.names = FALSE, col.names =TRUE)
    }
  )




})
