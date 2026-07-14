# CERP Shiny App - Version 2.0.0
# Computing Degrees Completed - IPEDS
# Originally by: Evelyn Yarzebinski
# Version 2.0.0 (July 2026, Nargiz Akhmetova):
#   - Task 1: data now loads from four small Parquet files instead of the 984 MB CSV
#   - Task 2: users view one data level at a time (Institution / State / National)
#   - Task 3: the CIP code picker always offers the full tracked list
#   - fixed the misspelled "postion" argument in the plot functions (it was silently ignored)

### prep environment ----

# call libraries
library(shiny)
library(tidyverse)   # includes dplyr, tidyr, ggplot2
library(DT)
library(scales)
library(janitor)
library(shinythemes)
library(shinyWidgets)
library(plotly)
library(bslib)
library(arrow)

### App text ----

#set version number
versionNumber = "2.0.0"

# set text
provisionalText = "Some IPEDS data releases are provisional and may be updated by NCES in a final release."
appCite = paste0('\n\nCERP Data Visualization Dashboards (Version ',versionNumber,'). Computing Research Association. Center for Evaluating the Research Pipeline. Accessed ',
                 Sys.Date(),' at https://cra.org/cerp/data-visualization/.\n\n',provisionalText)


#### set up IPEDS data (Parquet pipeline) ----

# Small tables are held in memory. They are tiny and consulted constantly.
ipeds_offerings <- arrow::read_parquet("data/offerings.parquet")  # ~231k rows: inst x CIP x degree x year catalog
ipeds_national  <- arrow::read_parquet("data/national.parquet")   # ~84k rows: national totals

# Large tables are opened lazily. Nothing loads until a query runs,
# and each query returns only the rows matching the user's selection.
ipeds_inst_ds   <- arrow::open_dataset("data/inst.parquet")
ipeds_state_ds  <- arrow::open_dataset("data/state.parquet")

# All 30 gender by race combinations (3 gender values x 10 race values).
demo_grid <- dplyr::distinct(ipeds_national, gender, race)

# Full tracked CIP list (Task 3: offered to users regardless of institution)
ipeds_allCipcodes <- sort(unique(ipeds_offerings$cipTitle))
ipeds_uniqueCipcode = gsub(",",";",ipeds_allCipcodes)

#### CIP families (4 digit filter) ----
# A 6 digit CIP code like 11.0201 belongs to the 4 digit family 11.02.
# The pattern below handles codes stored without a leading zero (e.g. 9.0702 -> 9.07).
cipFamilyOf <- function(cipTitles) {
  code6 <- sub(" .*", "", cipTitles)
  sub("^([0-9]+\\.[0-9]{2}).*$", "\\1", code6)
}

# Display titles for the 4 digit series, per the NCES CIP 2020 taxonomy.
# If a new family ever appears in the data without an entry here, it still
# displays (with a generic label); add its official title to this list.
cip4_seriesTitles <- c(
  `9.07`  = "Radio, Television, and Digital Communication",
  `10.03` = "Graphic Communications",
  `11.01` = "Computer and Information Sciences, General",
  `11.02` = "Computer Programming",
  `11.03` = "Data Processing",
  `11.04` = "Information Science/Studies",
  `11.05` = "Computer Systems Analysis",
  `11.07` = "Computer Science",
  `11.08` = "Computer Software and Media Applications",
  `11.09` = "Computer Systems Networking and Telecommunications",
  `11.10` = "Computer/Information Technology Administration and Management",
  `11.99` = "Computer and Information Sciences and Support Services, Other",
  `14.09` = "Computer Engineering",
  `26.11` = "Biomathematics, Bioinformatics, and Computational Biology",
  `27.03` = "Applied Mathematics",
  `30.08` = "Mathematics and Computer Science",
  `30.16` = "Accounting and Computer Science",
  `30.30` = "Computational Science",
  `30.31` = "Human Computer Interaction",
  `30.48` = "Linguistics and Computer Science",
  `30.70` = "Data Science",
  `30.71` = "Data Analytics",
  `50.01` = "Visual and Performing Arts, General",
  `51.27` = "Medical Illustration and Informatics",
  `52.12` = "Management Information Systems and Services"
)

# family of each entry in ipeds_allCipcodes (parallel vector, used to filter
# the 6 digit picker by the selected families)
ipeds_cip4_of_all <- cipFamilyOf(ipeds_allCipcodes)

# named choices for the family picker: names are shown, values are returned
ipeds_allCip4codes <- sort(unique(ipeds_cip4_of_all))
ipeds_cip4choices <- setNames(
  ipeds_allCip4codes,
  paste0(ipeds_allCip4codes, " ",
         ifelse(is.na(cip4_seriesTitles[ipeds_allCip4codes]),
                "(unlabeled family; see 6 digit codes)",
                cip4_seriesTitles[ipeds_allCip4codes]))
)

# ipeds degree level factor grouping
award_facLevel = c("Associate's",
                   "Bachelor's",
                   "Master's",
                   "Doctoral")

### FUNCTIONS ----

#### main data functions ----

# Filtered level data --> table data (Task 2: one level at a time).
# `data` is a reactive returning the level specific frame; `level` is
# "Institution", "State", or "National". The frame must contain the matching
# value column: awards / totalStateAwards / totalNationalAwards.
mainFilteredDataToTableData_ipeds = function(data, level) {
  df <- data() %>%
    dplyr::group_by(award_fac, gender, race)

  df <- switch(level,
    "Institution" = dplyr::summarize(df, N = sum(awards), .groups = "drop"),
    "State"       = dplyr::summarize(df, N = sum(totalStateAwards), .groups = "drop"),
    "National"    = dplyr::summarize(df, N = sum(totalNationalAwards), .groups = "drop"))

  df %>%
    dplyr::group_by(award_fac) %>%
    #get the percent within the (single) selected degree level
    dplyr::mutate(Pct = ifelse(N == 0, 0, round(N/sum(N),4))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(award_fac = NULL) %>%
    dplyr::select(Gender = gender,
                  `Race/Ethnicity` = race,
                  `Awards (N)` = N,
                  `Awards (%)` = Pct) %>%
    janitor::adorn_totals("row")
}

# table data --> formatted HTML table. `level` feeds the hover tooltip text.
ipedsDataHTMLTable = function(data, level) {
  levelText = tolower(level)
  data() %>%
    datatable(rownames = F,
              extensions = "Buttons",
              options = exprToFunction(
                list(
                  columnDefs = list(list(targets = c(0), type = "num-fmt")),
                  pageLength = 25, #everything on one page; max 20 gender x race combos plus a totals row
                  scrollX = TRUE,
                  searching = FALSE,
                  dom = "t",
                  #custom tooltip via JavaScript; column index 3 is Awards (%)
                  rowCallback = JS(
                    "function(row, data) {",
                    paste0("var full_text = 'Students in this group earned ' + (data[3]*100).toFixed(2) + '% of computing degrees for this degree type + program + year(s) at the ", levelText, " level.'"),
                    "$('td', row).attr('title', full_text);",
                    "}")
                ) #end of list
              )) %>%
    formatPercentage('Awards (%)', 2)
}

#### main plot functions ----
# IPEDS filtered data --> plot data - inst
mainFilteredDataToPlotData_ipeds_inst = function(data){
  data() %>%
    dplyr::group_by(fips,
             inst_name,
             award_fac,
             cipTitle,
             year,
             gender,
             race
    ) %>%
    dplyr::summarize(instSum = sum(awards),
                     raceGender = paste0(race," ",gender)) %>%
    dplyr::group_by(year) %>%
    #get the percent for each grouping
    dplyr::mutate(instPct = ifelse(instSum == 0, 0,
                                   100*round(instSum/sum(instSum),4))) %>%
    ungroup() %>%
    dplyr::mutate(population = case_when(gender == "all" & race == "all"~"all",
                                gender != "all" & race == "all"~gender,
                                gender == "all" & race != "all"~race,
                                gender != "all" & race != "all"~paste0(race," ",gender),
                                TRUE~NA_character_),
           year = as.character(year),
           cipCode_distinct = n_distinct(cipTitle),
           cipCode = ifelse(cipCode_distinct==1,cipTitle,"multiple CIP Codes")) %>%
    dplyr::group_by(State = fips,
                    `Institution Name` = inst_name,
                    Year = year,
                    Population = population,
                    `Race/Ethnicity` = race,
                    Gender = gender,
                    `CIP Code` = cipCode) %>%
    dplyr::summarize(`Degrees Awarded-Institution (%)` = round(sum(instPct),2),
                     `Degrees Awarded-Institution (N)` = round(sum(instSum),2)
                     ) %>%
    ungroup()
  }

# IPEDS filtered data --> plot data - state
mainFilteredDataToPlotData_ipeds_state = function(data){
  data() %>%
    dplyr::group_by(fips,
             award_fac,
             cipTitle,
             year,
             gender,
             race
    ) %>%
    dplyr::summarize(stateSum = sum(totalStateAwards),
                     raceGender = paste0(race," ",gender)) %>%
    dplyr::group_by(year) %>%
    # get the percent for each grouping
    dplyr::mutate(statePct = ifelse(stateSum == 0, 0,
                                    100*round(stateSum/sum(stateSum),4))) %>%
    ungroup() %>%
    dplyr::mutate(population = case_when(gender == "all" & race == "all"~"all",
                                  gender != "all" & race == "all"~gender,
                                  gender == "all" & race != "all"~race,
                                  gender != "all" & race != "all"~paste0(race," ",gender),
                                  TRUE~NA_character_),
           year = as.character(year),
           cipCode_distinct = n_distinct(cipTitle),
           cipCode = ifelse(cipCode_distinct==1,cipTitle,"multiple CIP Codes")) %>%
    dplyr::group_by(State = fips,
                    Year = year,
                    Population = population,
                    `Race/Ethnicity` = race,
                    Gender = gender,
                    `CIP Code` = cipCode) %>%
    dplyr::summarize(`Degrees Awarded-State (%)` = round(sum(statePct),2),
                     `Degrees Awarded-State (N)` = round(sum(stateSum),2)) %>%
    ungroup()
}

# IPEDS filtered data --> plot data - national
mainFilteredDataToPlotData_ipeds_national = function(data){
  data() %>%
    dplyr::group_by(award_fac,
                    cipTitle,
                    year,
                    gender,
                    race) %>%
    dplyr::summarize(nationalSum = sum(totalNationalAwards),
                     raceGender = paste0(race," ",gender)) %>%
    dplyr::group_by(year) %>%
    #get the percent for national
    dplyr::mutate(nationalPct = ifelse(nationalSum == 0, 0,
                                       100*round(nationalSum/sum(nationalSum),4))) %>%
    ungroup() %>%
    mutate(population = case_when(gender == "all" & race == "all"~"all",
                                  gender != "all" & race == "all"~gender,
                                  gender == "all" & race != "all"~race,
                                  gender != "all" & race != "all"~paste0(race," ",gender),
                                  TRUE~NA_character_),
           year = as.character(year),
           cipCode_distinct = n_distinct(cipTitle),
           cipCode = ifelse(cipCode_distinct==1,cipTitle,"multiple CIP Codes")) %>%
    dplyr::group_by(Year = year,
                    Population = population,
                    `Race/Ethnicity` = race,
                    Gender = gender,
                    `CIP Code` = cipCode) %>%
    dplyr::summarize(`Degrees Awarded-National (%)` = round(sum(nationalPct),2),
                     `Degrees Awarded-National (N)` = round(sum(nationalSum),2)) %>%
    ungroup()
}

# IPEDS plot inst data --> static ggplot
# (v2.0.0 note: the old geom_bar(postion = "fill") argument was misspelled and
# therefore silently ignored; it has been removed. Rendering is unchanged.)
ipedsDataGGplot_inst = function(data){
  data() %>%
    ggplot(aes(x = Year,
               y = `Degrees Awarded-Institution (%)`,
               fill = Population)) +
    geom_bar(stat = "identity") +
    ggtitle("Institution Computing Degrees Awarded by Year\n") +
    labs(x = "Year",
         y = "Computing Degrees Awarded (%)",
         col = "Population") +
    scale_fill_manual(values = ifelse(data()$`Race/Ethnicity` != "all" & data()$Gender != "all", colors_allGenderAllRaceEthnicity,
                                      ifelse(data()$`Race/Ethnicity` == "all" & data()$Gender != "all", colors_gender,
                                             ifelse(data()$`Race/Ethnicity` != "all" & data()$Gender == "all", colors_race_ipeds,
                                                    ifelse(data()$`Race/Ethnicity` == "all" & data()$Gender == "all", colors_all,
                                                           "#999999")
                                             )
                                      )
    ))
}

# IPEDS plot state data --> static ggplot
ipedsDataGGplot_state = function(data){
  data() %>%
    ggplot(aes(x = Year,
               y = `Degrees Awarded-State (%)`,
               fill = Population
               )) +
    geom_bar(stat = "identity") +
    ggtitle("State Computing Degrees Awarded by Year\n") +
    labs(x = "Year",
         y = "Computing Degrees Awarded (%)",
         col = "Population") +
    scale_fill_manual(values = ifelse(data()$`Race/Ethnicity` != "all" & data()$Gender != "all", colors_allGenderAllRaceEthnicity,
                                                                     ifelse(data()$`Race/Ethnicity` == "all" & data()$Gender != "all", colors_gender,
                                                                            ifelse(data()$`Race/Ethnicity` != "all" & data()$Gender == "all", colors_race_ipeds,
                                                                                   ifelse(data()$`Race/Ethnicity` == "all" & data()$Gender == "all", colors_all,
                                                                                          "#999999")
                                                                            )
                                                                     )
                                                           ))
  }

# IPEDS plot natl data --> static ggplot
ipedsDataGGplot_national = function(data){
  data() %>%
    ggplot(aes(x = Year,
               y = `Degrees Awarded-National (%)`,
               fill = Population
               )) +
    geom_bar(stat = "identity") +
    ggtitle("National Computing Degrees Awarded by Year\n") +
    labs(x = "Year",
         y = "Computing Degrees Awarded (%)",
         col = "Population") +
    scale_fill_manual(values = ifelse(data()$`Race/Ethnicity` != "all" & data()$Gender != "all", colors_allGenderAllRaceEthnicity,
                                      ifelse(data()$`Race/Ethnicity` == "all" & data()$Gender != "all", colors_gender,
                                             ifelse(data()$`Race/Ethnicity` != "all" & data()$Gender == "all", colors_race_ipeds,
                                                    ifelse(data()$`Race/Ethnicity` == "all" & data()$Gender == "all", colors_all,
                                                           "#999999")
                                             )
                                      )
    ))
}

### Plot miscellanea ----

# padding and indenting values
m = list(
  l = 80,
  r = 300,
  b = 80,
  t = 100,
  pad = 0
)

colors_allGenderAllRaceEthnicity <- c("#3a85a8", #peacock blue
                                      "#fdc81d", #light orange
                                      "#a24157", #maroon
                                      "#5f6692", #blue-purple
                                      "#42977e", #blue green
                                      "#9c509b", #magenta
                                      "#eb7520", #bright orange
                                      "#fff82f", #bright yellow
                                      "#c5954a", #light brown
                                      "#bc8fa7", #lavender
                                      "#e41a1c", #red
                                      "#999999", #grey
                                      "#e1c630", #mustard
                                      "#4aaa54", #kelly green
                                      "#cc6a6f", #red brown
                                      "#7e6e85", #eggplant
                                      "#fb9709", #light orange
                                      "#629362" #pine green
                                      )

colors_gender <- c("#F7E650", #minion yellow
                   "#53BEF3" #capri blue
                   )

colors_race_ipeds <- c("#AADB1E", #CRA light warm green = American Indian or Alaskan Native*
                       "#A6093D", #CRA red = Asian*
                       "#6CACE4", #CRA light blue = Black or African American*
                       "#E35205", #CRA bold orange = Hispanic or Latino*
                       "#9F8FCA", #CRA light purple = Native Hawaiian or Other Pacific Islander*
                       "#FFD100", #CRA yellow = U.S. Nonresident
                       "#1B806D", #CRA dark green = Two or more races*
                       "#ED8B00", #CRA light orange, = Unknown
                       "#2731A1" #CRA dark blue = White*
                       )

colors_all <- c("#888888")
