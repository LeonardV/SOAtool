# sudo ssh lvanbrabant@172.28.242.5
# cd /srv/shiny-server/SOAtool
# scp server.R lvanbrabant@172.28.242.5:/srv/shiny-server/SOAtool/

# install.packages(c("shiny","shinydashboard","plotly","DT","htmltools","shinyjs","leaflet","dplyr","magrittr","lubridate","ggplot2",
#                    "stringr","readxl","RColorBrewer","gtable","gridExtra","grid","zoo","diget","openxlsx"))

setwd("C:/Users/l.vanbrabant/OneDrive - Het Servicecentrum/SOAtool/shinyApp")
require(shiny)

source('ui.R')
source('server.R')

#library(profvis)
#profvis({
runApp(list(ui = ui, server = server), launch.browser = TRUE)
#})


