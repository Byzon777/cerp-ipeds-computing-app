# CERP Shiny App - Version 2.0.0
# Computing Degrees Completed - IPEDS
# Originally by: Evelyn Yarzebinski
# Version 2.0.0 (July 2026, Nargiz Akhmetova): Parquet pipeline (Task 1),
# one data level at a time (Task 2), full CIP list always selectable (Task 3).

server <- (function(input, output){

#######################################
#### IPEDS: Reactive sidebar menus ####
#######################################

# IPEDS: select state (shown for Institution and State levels)
  output$ipeds_cip2_state_selector <- renderUI({
    selectInput(
      inputId = "ipeds_cip2_fipsInput",
      label = "Select or Type State/Territory:",
      choices = sort(as.character(unique(ipeds_offerings$fips))),
      selected = "")
  })

# IPEDS: institutions in the selected state (shown for Institution level)
  output$ipeds_cip2_name_selector <- renderUI({
    req(input$ipeds_cip2_fipsInput)
    available_name <- ipeds_offerings$inst_name[ipeds_offerings$fips == input$ipeds_cip2_fipsInput]

    selectInput(
      inputId = "ipeds_cip2_instnameInput",
      label = "Select or Type Institution Name:",
      choices = sort(unique(available_name)),
      selected = "")
  })

# IPEDS: degree types, scoped to the selected level
  output$ipeds_cip2_degree_selector <- renderUI({
    req(input$ipeds_levelInput)

    if (input$ipeds_levelInput == "Institution") {
      req(input$ipeds_cip2_instnameInput)
      available_deg <- unique(ipeds_offerings$award_fac[ipeds_offerings$inst_name == input$ipeds_cip2_instnameInput])
    } else if (input$ipeds_levelInput == "State") {
      req(input$ipeds_cip2_fipsInput)
      available_deg <- unique(ipeds_offerings$award_fac[ipeds_offerings$fips == input$ipeds_cip2_fipsInput])
    } else {
      available_deg <- award_facLevel
    }
    #keep a consistent Associate's -> Doctoral ordering
    available_deg <- award_facLevel[award_facLevel %in% available_deg]

    selectInput(
      inputId = "ipeds_cip2_degreeInput",
      label = "Select or Type Degree Type:",
      choices = available_deg,
      selected = if ("Bachelor's" %in% available_deg) "Bachelor's" else available_deg[1]
    )
  })

# IPEDS: 4 digit CIP family filter - narrows the 6 digit picker below it
  output$ipeds_cip2_cip4_selector <- renderUI({
    pickerInput(
      inputId = "ipeds_cip2_cip4Input",
      label = "Select or Type 4-digit CIP Family:",
      choices = ipeds_cip4choices,
      selected = unname(ipeds_cip4choices),
      options = list(`actions-box` = TRUE,
                     `selected-text-format` = 'count > 1',
                     `count-selected-text` = '{0} of {1} families selected'),
      multiple = TRUE
    )
  })

# IPEDS: 6 digit CIP codes - Task 3: full tracked list, independent of
# institution, limited to the 4 digit families selected above
  output$ipeds_cip2_cip_selector <- renderUI({
    req(input$ipeds_cip2_cip4Input)
    available_cip <- ipeds_allCipcodes[ipeds_cip4_of_all %in% input$ipeds_cip2_cip4Input]

    pickerInput(
      inputId = "ipeds_cip2_cipInput",
      label = "Select or Type 6-digit CIP Code (Computing):",
      choices = available_cip,
      selected = available_cip,
      options = list(`actions-box` = TRUE,
                     `selected-text-format` = 'count > 1',
                     `count-selected-text` = '{0} of {1} programs selected'),
      multiple = TRUE
    )
  })

#############################################
#### IPEDS: Reactive dataset making code ####
#############################################

# which gender x race rows the user wants displayed
selectedDemo = reactive({
  demo_grid %>%
    dplyr::filter(if (input$ipeds_cip2_genderInput == "all") gender == "all" else gender != "all",
                  if (input$ipeds_cip2_raceInput  == "all") race  == "all" else race  != "all")
})

# which CIP x degree x year cells match the sidebar (the complete national
# cell list lives in the national totals table)
selectedCombos = reactive({
  req(input$ipeds_cip2_degreeInput, input$ipeds_cip2_cipInput, input$ipeds_cip2_yearInput)
  ipeds_national %>%
    dplyr::distinct(cipTitle, award_fac, year) %>%
    dplyr::filter(award_fac %in% input$ipeds_cip2_degreeInput,
                  cipTitle %in% input$ipeds_cip2_cipInput,
                  year >= input$ipeds_cip2_yearInput[1],
                  year <= input$ipeds_cip2_yearInput[2])
})

# INSTITUTION level frame: full grid for the selection (zeros included), with
# state and national totals joined on, in the same wide shape v1 used.
# Task 3 note: CIPs the institution never reported simply appear as zeros.
ipeds_filtered_inst = reactive({
  req(input$ipeds_levelInput == "Institution",
      input$ipeds_cip2_instnameInput, input$ipeds_cip2_fipsInput)

  grid <- tidyr::crossing(selectedCombos(), selectedDemo()) %>%
    dplyr::mutate(inst_name = input$ipeds_cip2_instnameInput,
                  fips = input$ipeds_cip2_fipsInput)

  inst_small <- ipeds_inst_ds %>%
    dplyr::filter(inst_name %in% input$ipeds_cip2_instnameInput,
                  year >= input$ipeds_cip2_yearInput[1],
                  year <= input$ipeds_cip2_yearInput[2]) %>%
    dplyr::collect()

  state_small <- ipeds_state_ds %>%
    dplyr::filter(fips %in% input$ipeds_cip2_fipsInput,
                  year >= input$ipeds_cip2_yearInput[1],
                  year <= input$ipeds_cip2_yearInput[2]) %>%
    dplyr::collect()

  grid %>%
    dplyr::left_join(inst_small,
      by = c("fips","inst_name","cipTitle","award_fac","year","gender","race")) %>%
    dplyr::mutate(awards = dplyr::coalesce(awards, 0L)) %>%
    dplyr::left_join(state_small,
      by = c("fips","cipTitle","award_fac","gender","race","year")) %>%
    dplyr::mutate(totalStateAwards = dplyr::coalesce(totalStateAwards, 0L)) %>%
    dplyr::left_join(ipeds_national,
      by = c("cipTitle","award_fac","gender","race","year")) %>%
    dplyr::mutate(totalNationalAwards = dplyr::coalesce(totalNationalAwards, 0L),
                  cipTitle = gsub(",",";",cipTitle))
})

# STATE level frame: state totals for the selection, no institution needed
ipeds_filtered_state = reactive({
  req(input$ipeds_levelInput == "State", input$ipeds_cip2_fipsInput,
      input$ipeds_cip2_degreeInput, input$ipeds_cip2_cipInput)

  ipeds_state_ds %>%
    dplyr::filter(fips %in% input$ipeds_cip2_fipsInput,
                  award_fac %in% input$ipeds_cip2_degreeInput,
                  cipTitle %in% input$ipeds_cip2_cipInput,
                  year >= input$ipeds_cip2_yearInput[1],
                  year <= input$ipeds_cip2_yearInput[2]) %>%
    dplyr::collect() %>%
    dplyr::semi_join(selectedDemo(), by = c("gender","race")) %>%
    dplyr::mutate(cipTitle = gsub(",",";",cipTitle))
})

# NATIONAL level frame: national totals for the selection, no state needed
ipeds_filtered_national = reactive({
  req(input$ipeds_levelInput == "National",
      input$ipeds_cip2_degreeInput, input$ipeds_cip2_cipInput)

  ipeds_national %>%
    dplyr::filter(award_fac %in% input$ipeds_cip2_degreeInput,
                  cipTitle %in% input$ipeds_cip2_cipInput,
                  year >= input$ipeds_cip2_yearInput[1],
                  year <= input$ipeds_cip2_yearInput[2]) %>%
    dplyr::semi_join(selectedDemo(), by = c("gender","race")) %>%
    dplyr::mutate(cipTitle = gsub(",",";",cipTitle))
})

# the frame for whichever level is active (used by the table and its download)
activeLevelData = reactive({
  switch(input$ipeds_levelInput,
         "Institution" = ipeds_filtered_inst(),
         "State"       = ipeds_filtered_state(),
         "National"    = ipeds_filtered_national())
})

# level frame --> table data (activeLevelData is itself a reactive, so it can
# be passed directly in the same style the v1 functions used)
dataForFinalTable_ipeds = reactive({
  mainFilteredDataToTableData_ipeds(activeLevelData, input$ipeds_levelInput)
})

# level frames --> plot data
dataForFinalPlot_ipeds_inst     = reactive({ mainFilteredDataToPlotData_ipeds_inst(ipeds_filtered_inst) })
dataForFinalPlot_ipeds_state    = reactive({ mainFilteredDataToPlotData_ipeds_state(ipeds_filtered_state) })
dataForFinalPlot_ipeds_national = reactive({ mainFilteredDataToPlotData_ipeds_national(ipeds_filtered_national) })

###############################
#### IPEDS Render displays ####
###############################

# table (level aware)
output$ipeds_cip2_mytable = DT::renderDataTable({
  validate(need(nrow(activeLevelData()) > 0,
                "No reported data for this combination of selections. Try widening the year range or selecting more CIP codes."))
  ipedsDataHTMLTable(dataForFinalTable_ipeds, input$ipeds_levelInput)
})

# plots: only the active level's plot is present in the UI at any time
output$ipeds_cip2_myplot_inst = renderPlotly({
  validate(need(nrow(dataForFinalPlot_ipeds_inst()) > 0,
                "No reported data for this selection."))
  ipedsDataGGplot_inst(dataForFinalPlot_ipeds_inst)
})

output$ipeds_cip2_myplot_state = renderPlotly({
  validate(need(nrow(dataForFinalPlot_ipeds_state()) > 0,
                "No reported data for this selection."))
  ipedsDataGGplot_state(dataForFinalPlot_ipeds_state)
})

output$ipeds_cip2_myplot_national = renderPlotly({
  validate(need(nrow(dataForFinalPlot_ipeds_national()) > 0,
                "No reported data for this selection."))
  ipedsDataGGplot_national(dataForFinalPlot_ipeds_national)
})

#########################
#### IPEDS: Download ####
#########################

# shared pieces for download file names and headers
yearsLabel = function() {
  paste0(input$ipeds_cip2_yearInput[1],
         ifelse(input$ipeds_cip2_yearInput[1] == input$ipeds_cip2_yearInput[2], "",
                paste0("-", input$ipeds_cip2_yearInput[2])))
}

scopeLabel = function(level) {
  switch(level,
         "Institution" = input$ipeds_cip2_instnameInput,
         "State"       = input$ipeds_cip2_fipsInput,
         "National"    = "National")
}

downloadFileName = function(kind, level, ext) {
  gsub(" ","",paste0("IPEDS_", kind, "_", level, "_",
                     scopeLabel(level), "_",
                     yearsLabel(), "_",
                     input$ipeds_cip2_degreeInput, "_",
                     "DownloadedFromCRA_CERP_",
                     Sys.Date(), ".", ext))
}

downloadFileHeader = function(level) {
  scopeLine = switch(level,
    "Institution" = paste0("\n\nInstitution: ", input$ipeds_cip2_instnameInput),
    "State"       = paste0("\n\nState: ", input$ipeds_cip2_fipsInput),
    "National"    = "\n\nScope: National")
  paste0("The below data is copied from https://cra.org/cerp/data-visualization/.",
         "\nThe data is from the IPEDS dataset on Completions ('Awards/degrees conferred by program (6-digit CIP code); award level; race/ethnicity; and gender')",
         "\nThe data is filtered for the following selections:",
         scopeLine,
         "\nData level displayed: ", level,
         "\nYear(s): ", yearsLabel(),
         " [Year is July 1 of the prior year to June 30 of the selected year.]",
         "\nDegree Level: ", input$ipeds_cip2_degreeInput,
         "\n", ifelse(input$ipeds_cip2_raceInput != "all", input$ipeds_cip2_raceInput, "Aggregate Race/Ethnicity"),
         "\n", ifelse(input$ipeds_cip2_genderInput != "all", input$ipeds_cip2_genderInput, "Aggregate Gender"),
         "\n\nPrograms Included:\n",
         gsub(",",";",paste0(input$ipeds_cip2_cipInput, sep="", collapse="\n")),
         "\n[Please confirm the CIP code(s) your academic unit uses for reporting.]\n\n")
}

writeDownloadCsv = function(file, level, body) {
  suppressWarnings({
    write.table(downloadFileHeader(level), quote=F, sep=",", col.names=F, row.names=F, file)
    write.table(body, quote=F, sep=",", append=T, row.names=F, file)
    write.table(paste0(appCite), quote=F, sep=",", col.names=F, row.names=F, append=T, file)
  })
}

# table data download (level aware)
output$downloadTableData_ipeds <- downloadHandler(
  filename = function(){ downloadFileName("TableData", input$ipeds_levelInput, "csv") },
  content = function(file) {
    writeDownloadCsv(file, input$ipeds_levelInput, dataForFinalTable_ipeds())
  }
)

# plot data downloads (one per level; only the active level's button is shown)
output$downloadPlotData_ipeds_inst <- downloadHandler(
  filename = function(){ downloadFileName("PlotData", "Institution", "csv") },
  content = function(file) { writeDownloadCsv(file, "Institution", dataForFinalPlot_ipeds_inst()) }
)

output$downloadPlotData_ipeds_state <- downloadHandler(
  filename = function(){ downloadFileName("PlotData", "State", "csv") },
  content = function(file) { writeDownloadCsv(file, "State", dataForFinalPlot_ipeds_state()) }
)

output$downloadPlotData_ipeds_national <- downloadHandler(
  filename = function(){ downloadFileName("PlotData", "National", "csv") },
  content = function(file) { writeDownloadCsv(file, "National", dataForFinalPlot_ipeds_national()) }
)

# plot image downloads
output$downloadPlotImage_ipeds_inst <- downloadHandler(
  filename = function(){ downloadFileName("PlotImage", "Institution", "png") },
  content = function(file){
    req(nrow(dataForFinalPlot_ipeds_inst()) > 0)
    ggsave(file, plot = ipedsDataGGplot_inst(dataForFinalPlot_ipeds_inst),
           device = 'png', width = 10, units = "in")
  }
)

output$downloadPlotImage_ipeds_state <- downloadHandler(
  filename = function(){ downloadFileName("PlotImage", "State", "png") },
  content = function(file){
    req(nrow(dataForFinalPlot_ipeds_state()) > 0)
    ggsave(file, plot = ipedsDataGGplot_state(dataForFinalPlot_ipeds_state),
           device = 'png', width = 10, units = "in")
  }
)

output$downloadPlotImage_ipeds_national <- downloadHandler(
  filename = function(){ downloadFileName("PlotImage", "National", "png") },
  content = function(file){
    req(nrow(dataForFinalPlot_ipeds_national()) > 0)
    ggsave(file, plot = ipedsDataGGplot_national(dataForFinalPlot_ipeds_national),
           device = 'png', width = 10, units = "in")
  }
)

})
