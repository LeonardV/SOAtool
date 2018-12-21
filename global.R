# install.packages(c("shiny","shinydashboard", "plotly", "plotlyBars", "DT","htmltools","shinyjs","leaflet","dplyr",
#                    "magrittr","lubridate", "stringr","readxl","RColorBrewer","gtable",
#                    "gridExtra","zoo","openxlsx"))

library(shiny)            # shiny framework0
library(shinydashboard)   # framework to create dashboards
library(leaflet)
library(dplyr)
library(plotly)           # for plotly plots
#library(plotlyBars)
library(DT)               # for nice data tables
library(htmltools)        # required by sweetalter
library(shinyjs)          # javascript addon for shiny tools
library(magrittr)
library(lubridate)
library(colorspace)
library(stringr)
#library(readxl)
#library(openxlsx)
library(RColorBrewer)
library(gtable)
library(gridExtra)
#library(zoo)
library(data.table)
library(fst)

#library(profvis)

#md <- fread("data/md.csv")
#md <- readRDS("data/md.rds")
md <- read_fst("data/md.fst")

## Data preprocessing
#md <- md %>% mutate(datum_consult_mod   = as.numeric(format(Datum.consult, '%Y')))
#md <- md %>% mutate(datum_consult_year  = as.numeric(format(Datum.consult, '%Y')))
#md <- md %>% mutate(datum_consult_month = as.numeric(format(Datum.consult, '%m')))

#md$Geboortejaar <- ymd(md$Geboortejaar, truncated = 2)

# helper functions
# convert character to numeric value
# fn2 <- function(x) {
#   if (x == "Positief") {
#     1
#   } else if (x == "Negatief") {
#     0
#   } else if (x == "Niet uitgevoerd") {
#     9
#   } else {
#     as.numeric(NA)
#   }
# }

# fn3 <- function(x) {
#   if (any(x == "Positief")) {
#     1
#   } else if (all(x == "Niet uitgevoerd")) {
#     9
#   } else if (all(x == "Negatief")) {
#     0
#   } else {
#     # combination negatief and niet uitgevoerd
#     0
#   }
# }

# fn4 <- function(x) {
#   if (x == "Ja") {
#     1
#   } else if (x == "Nee") {
#     0
#   } else if (x == "Onbekend") {
#     as.numeric(NA)
#   } else {
#     as.numeric(NA)
#   }
# }

# fn5 <- function(x) {
#   if (x == "jaPositief") {
#     1
#   } else if (x == "jaNegatief") {
#     0
#   } else if (x %in% c("Onbekend", "Nee", "jaOnbekend")) {
#     as.numeric(NA)
#   } else {
#     as.numeric(NA)
#   }
# }

