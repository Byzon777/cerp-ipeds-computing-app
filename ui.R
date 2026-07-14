# CERP Shiny App - Version 2.0.0
# Computing Degrees Completed - IPEDS
# Originally by: Evelyn Yarzebinski
# Version 2.0.0 (July 2026, Nargiz Akhmetova): Parquet pipeline (Task 1),
# one data level at a time (Task 2), full CIP list always selectable (Task 3).

#####################
#### footer text ####
#####################

dataNote = "Note: Percentages may not add up to 100% within a column due to rounding. "
versionAuthor = paste0("Version ", versionNumber,". Originally written by Evelyn Yarzebinski. Send app-related questions to evelyn@cra.org.")
citationText = paste0("CERP Data Visualization Dashboards, Postsecondary Computing Degrees Awarded (Version ",versionNumber,"). Computing Research Association, Center for Evaluating the Research Pipeline. Accessed ",Sys.Date(),", https://cra.org/cerp/data-visualization/.")

#################
#### page UI ####
#################

ui = navbarPage("",
                theme = shinytheme("flatly"),
                header=singleton(tags$head(includeHTML(("google-analytics-cra.html")))), #calls Google analytics

###################
#### IPEDS TAB ####
###################

                tabPanel("Computing Degrees Awarded",
                         h2("IPEDS: Computing Degrees Awarded"),

                         sidebarLayout(
                           sidebarPanel(
                             h4("Select data"),

                             # Task 2: one data level at a time
                             radioButtons("ipeds_levelInput",
                                          label = "Select data level:",
                                          choices = c("Institution", "State", "National"),
                                          selected = "Institution"),

                             # state selector: needed for Institution and State levels
                             conditionalPanel(
                               condition = "input.ipeds_levelInput != 'National'",
                               htmlOutput("ipeds_cip2_state_selector")
                             ),

                             # institution selector: needed only for Institution level
                             conditionalPanel(
                               condition = "input.ipeds_levelInput == 'Institution'",
                               htmlOutput("ipeds_cip2_name_selector")
                             ),

                             htmlOutput("ipeds_cip2_degree_selector"),
                             htmlOutput("ipeds_cip2_cip4_selector"),
                             htmlOutput("ipeds_cip2_cip_selector"),

                             # Specify Data to Display
                             sliderInput("ipeds_cip2_yearInput",
                                         label = "Show data for:",
                                         min = as.numeric(min(ipeds_offerings$year)),
                                         max = as.numeric(max(ipeds_offerings$year)),
                                         c(max(ipeds_offerings$year)-3,max(ipeds_offerings$year)),
                                         sep=""),
                             h4("Customize the output"),
                             radioButtons("ipeds_cip2_genderInput",
                                          label = "Select view for student gender:",
                                          choices = list("Aggregate gender" = "all",
                                                         "Display gender"),
                                          selected = "Display gender"),
                             radioButtons("ipeds_cip2_raceInput",
                                          label = "Select view for student race/ethnicity:",
                                          choices = list("Aggregate race/ethnicity" = "all",
                                                         "Display race/ethnicity"),
                                          selected = "Display race/ethnicity")
                           ),

                           #show table and plot for the selected level
                           mainPanel(
                             tabsetPanel(type = "tabs",
                                         #table of data (aggregated across years)
                                         tabPanel("Table",
                                                  br(),
                                                  p("The table shows results for the data level selected in the sidebar. Switch the level to view institution, state, or national results."),
                                                  downloadButton('downloadTableData_ipeds','Download Table Data '),
                                                  br(),
                                                  DT::dataTableOutput("ipeds_cip2_mytable")
                                         ),
                                         #plot of data (disaggregated by year)
                                         tabPanel("Plot",
                                                  br(),
                                                  p("The plot shows results for the data level selected in the sidebar. The plot is interactive: hover over it for more detail. If the legend covers the plot, increase the width of your browser window. You may need to scroll in the legend to access the full list of populations."),
                                                  p("Double click on an entry in the legend to display only that entry. Double click again on that item to restore the plot to its original format. If you change the plot view, double click on the plot to reset it to the original view.
                                                  Single click on an entry in the legend to remove that entry from the plot. Single click again to add the entry back."),
                                                  br(),

                                                  conditionalPanel(
                                                    condition = "input.ipeds_levelInput == 'Institution'",
                                                    downloadButton('downloadPlotImage_ipeds_inst','Download Institution Plot Image '),
                                                    downloadButton('downloadPlotData_ipeds_inst','Download Institution Plot Data '),
                                                    br(), br(),
                                                    plotlyOutput("ipeds_cip2_myplot_inst")
                                                  ),

                                                  conditionalPanel(
                                                    condition = "input.ipeds_levelInput == 'State'",
                                                    downloadButton('downloadPlotImage_ipeds_state','Download State Plot Image '),
                                                    downloadButton('downloadPlotData_ipeds_state','Download State Plot Data '),
                                                    br(), br(),
                                                    plotlyOutput("ipeds_cip2_myplot_state")
                                                  ),

                                                  conditionalPanel(
                                                    condition = "input.ipeds_levelInput == 'National'",
                                                    downloadButton('downloadPlotImage_ipeds_national','Download National Plot Image '),
                                                    downloadButton('downloadPlotData_ipeds_national','Download National Plot Data '),
                                                    br(), br(),
                                                    plotlyOutput("ipeds_cip2_myplot_national")
                                                  )
                                         )
                             ),
                             br()
                           )
                         ),
                         hr(),
                         paste0(dataNote," ",versionAuthor),
                         br(),
                         br(),
                         paste0("Suggested Citation: ",citationText)
                ),

###################
#### ABOUT TAB ####
###################

      tabPanel("About",
               tags$b("Note: This app displays summaries of raw data collected by IPEDS. The raw data comes as-is from IPEDS. See below for more information."),
               br(),
               h3("How to use this app"),
               tags$ul(
                 tags$li("Choose the data level (Institution, State, or National) and the desired filtering criteria in the sidebar."),
                 tags$li("The CIP code list always shows every computing program CERP tracks. Use the 4-digit CIP Family filter to narrow the 6-digit list to broader program groups. If a selected institution did not report a selected program, its counts display as zero."),
                 tags$li("Review the resulting table or plot. Percentages are calculated as a given value divided by the sum of its column."),
                 tags$li("Download the table data, plot data, or plot image to your local device by clicking the relevant ‘Download’ button."),
                 tags$li("Hover over a table row or plot bar for additional context.")
               ),
               h3("Data sources used in this app"),
               h4("IPEDS (Integrated Postsecondary Education Data System)"),
               p("IPEDS is a database that tracks data on postsecondary institutions, which is maintained by the National Center for Education Statistics (NCES). It is updated through annual data collection efforts. The IPEDS dataset on Degrees Completed, found in 'Awards/degrees conferred by program (6-digit CIP code); award level; race/ethnicity; and gender', was accessed through the ",tags$a(href="https://nces.ed.gov/ipeds/","IPEDS website.", target="_blank"),
                    "IPEDS data is displayed in this app only if the selected institution reported graduating students for the particular Degree Type and Program CIP Code in the selected year range.",
                    tags$a(href="https://datavisualization.cra.org/datavizdocs/CIP_computing_degrees_webapp.pdf", "See a list of all included computing-related CIP codes here.", target="_blank")),
               p("All data comes as-is from IPEDS. Please refer to the",tags$a(href="https://surveys.nces.ed.gov/ipeds/public/glossary"," IPEDS glossary ", target="_blank"),"to see precise definitions for their data."),
               p("Pre-processing of IPEDS raw data filtered for:"),
               tags$ul(
                 tags$li("Institution is ‘Public’ or ‘Private not-for-profit’ and is ‘Four or more years’ or ‘At least two but less than four years’"),
                 tags$li("Institution awarded computing degrees under ",tags$a(href = "https://nces.ed.gov/ipeds/cipcode/browse.aspx?y=56", "CIP codes, which are federally-defined.", target="_blank"),tags$a(href="https://bpcnet.org/wp-content/uploads/2020/10/cipCodeList.pdf", "See a list of all included computing-related CIP codes here.", target="_blank")),
                 tags$li("Institution awarded computing degrees at the Associate, Bachelor’s, Master’s, and/or Doctoral level(s)."),
               ),
           h3("How to cite this app & App references"),
           tags$b("Suggested In-text Citation:"),
           p("“These data are from IPEDS datasets and are aggregated by a tool provided by the Computing Research Association via https://cra.org/cerp/data-visualization/.”"),
           tags$b("Suggested Reference:"),
           p(paste0("CERP Data Visualization Dashboards, Postsecondary Computing Degrees Awarded (Version ",versionNumber,"). Computing Research Association, Center for Evaluating the Research Pipeline. Accessed ",Sys.Date(),", https://cra.org/cerp/data-visualization/.")),
           tags$b("Data Sources, Acknowledgements & References:"),
           tags$ol(
             tags$li(paste0("U.S. Department of Education, National Center for Education Statistics, Integrated Postsecondary Education Data System (IPEDS), ",min(ipeds_offerings$year),"-",max(ipeds_offerings$year)," Completions. Retrieved from https://nces.ed.gov/ipeds/datacenter/DataFiles.aspx.")),
             tags$li("Zweben, S. H., & Bizot, E. B. (2016). Representation of women in postsecondary computing: Disciplinary, institutional, and individual characteristics. Computing in Science & Engineering, 18(2), 40-56.")
           ),
hr(),
versionAuthor,
br(),
br(),
paste0("Suggested Citation: ",citationText)

)
)
