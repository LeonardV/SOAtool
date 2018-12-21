source('global.R', local = TRUE)

####################################################################################
################################ User Interface#####################################
####################################################################################

#################################### header ########################################
#header <- dashboardHeader(title = tags$a(href='https://www.ggdwestbrabant.nl', 
#                                         tags$img(src = 'GGD_logo.png', height = '50px', 
#                                                  width = '200px')),
#                          tags$li(actionLink("openModal", label = "", icon = icon("info")),
#                                  class = "dropdown", style = 'margin-right:20px')
#)   

header <- dashboardHeader(title ="SOAtool - GGD v0.2", titleWidth = 250,
                          tags$li(actionLink("openModal", label = "", icon = icon("info")),
                                  class = "dropdown", style = 'margin-right:20px'),
                          tags$li(actionLink("openModal2", label = "", icon = icon("history")),
                                  class = "dropdown", style = 'margin-right:20px'))

#################################### sidebar ######################################
sidebar <- dashboardSidebar( 
  useShinyjs(),  
  #conditionalPanel("input.navbar == 'tab1_val'",
  div(id = 'tab1_sidebar',
       #sidebarMenuOutput("sidebar_menu1"),
       sidebarMenu(
        menuItem( 
          text    = "Filters",
          #tabName = "filter",
          icon    = icon("thumb-tack")),
    
          # needed for reset button
          div(id = "regio",
            selectizeInput(inputId  = "ggd",
                           label    = "GGD regio",
                           choices  = sort(unique(md$GGD)),
                           multiple = TRUE,
                           options  = list(plugins= list('remove_button'))),
            dateRangeInput(inputId   = "datum",
                           language  = "nl",
                           separator = " tot ",
                           label     = "Consultdatum",
                           start     = min(md$Datum.consult), 
                           end       = max(md$Datum.consult)
            )
          ),
        menuItem( 
          text = "Achtergrond kenmerken",
          tabName = "kenmerken",
          icon = icon("user"),
          
          div(id = "achtergrond",
            dateRangeInput(inputId   = "year",
                           language  = "nl",
                           separator = " tot ",
                           label     = "Geboortejaar",
                           start     = min(md$Geboortejaar, na.rm = TRUE), 
                           end       = max(md$Geboortejaar, na.rm = TRUE),
                           startview = "year"),
            selectizeInput(inputId  = "sekse",
                           label    = "Sekse",
                           choices  = unique(md$Sekse),
                           multiple = TRUE,
                           options  = list(plugins= list('remove_button'))),
            selectizeInput(inputId  = "voorkeur",
                           label    = "Seksuele voorkeur",
                           choices  = unique(md$Seksuele.voorkeur),
                           multiple = TRUE,
                           options  = list(plugins= list('remove_button'))),
            selectizeInput(inputId  = "opleiding",
                           label    = "Huidige of laatst genoten opleiding",
                           choices  = c("HBO/WO", "HAVO/VWO", "LBO/MAVO/VMBO/MBO", "Basisonderwijs", 
                                        "Anders", "Geen", "Onbekend"),
                           multiple = TRUE,
                           options  = list(plugins= list('remove_button'))),
            selectizeInput(inputId  = "patient",
                           label    = "Geboorteland client",
                           choices  = sort(unique(md$Geboorteland.patient)),
                           multiple = TRUE,
                           options  = list(plugins= list('remove_button'))),
            selectizeInput(inputId  = "woonplaats",
                           label    = "Woonplaats client",
                           choices  = sort(unique(md$woonplaats)),
                           multiple = TRUE,
                           options  = list(plugins= list('remove_button')))
          )
          # replace choices with a list
          # list(
          #   "Europe" = c("Germany", "Spain"),
          #   "North America" = c("Canada", "United States" = "USA")
          #   
          ),
        menuItem( 
          text    = "SOA diagnose",
          tabName = "diagnose",
          icon    = icon("search"),
          div(id = "soa",
            selectizeInput(inputId  = "gonorroe",
                           label    = "Gonorroe",
                           choices  = c("Positief", "Negatief", "Niet uitgevoerd"),
                           multiple = TRUE,
                           options  = list(plugins = list('remove_button'))),
            selectizeInput(inputId  = "chlamydia",
                           label    = "Chlamydia",
                           choices  = c("Positief", "Negatief", "Niet uitgevoerd"),
                           multiple = TRUE,
                           options  = list(plugins = list('remove_button'))),
            selectizeInput(inputId  = "serovartypering",
                           label    = "LGV",
                           choices  = c("Positief", "Negatief", "Niet te beoordelen", "Niet uitgevoerd"),
                           multiple = TRUE,
                           options  = list(plugins = list('remove_button'))),
            selectizeInput(inputId  = "syfilis",
                           label    = "Syfilis",
                           choices  = c("Positief", "Negatief", "Niet uitgevoerd"),
                           multiple = TRUE,
                           options  = list(plugins = list('remove_button'))),
            selectizeInput(inputId  = "hiv",
                           label    = "HIV",
                           choices  = c("Positief", "Negatief", "Niet uitgevoerd"),
                           multiple = TRUE,
                           options  = list(plugins = list('remove_button'))),
            selectizeInput(inputId  = "hepatitis.b",
                           label    = "Hepatitis B",
                           choices  = c("Positief", "Negatief", "Niet uitgevoerd"),
                           multiple = TRUE,
                           options  = list(plugins = list('remove_button'))),
            selectizeInput(inputId  = "acuminata",
                           label    = "Condylomata acuminata",
                           choices  = c("Positief", "Negatief", "Niet uitgevoerd"),
                           multiple = TRUE,
                           options  = list(plugins = list('remove_button'))),
            selectizeInput(inputId  = "hepatitis.c",
                           label    = "Hepatitis C",
                           choices  = c("Positief", "Negatief", "Niet uitgevoerd"),
                           multiple = TRUE,
                           options  = list(plugins = list('remove_button')))
          )
        ),
        menuItem( 
          text = "SOA testen",
          tabName = "testen",
          icon = icon("tint"),
          div(id = "tests",
            selectizeInput(inputId  = "testlocatie",
                           label    = "Testlocatie",
                           choices  = c("Regulier", "Thuistest pakket", "Man tot Man", "Onbekend"),
                           multiple = TRUE,
                           options  = list(plugins = list('remove_button'))),
            selectizeInput(inputId  = "geolocatie",
                           label    = "Testlocatie geografisch",
                           choices  = sort(unique(md$testlocatie)),
                           multiple = TRUE,
                           options  = list(plugins = list('remove_button'))),
            selectizeInput(inputId  = "gonorroeOnderzoek",
                           label    = "Uitgevoerd gonorroe onderzoek",
                           choices  = c("Anaal", "Cervicaal", "Oraal", "Urethraal", "Urine", "Vaginaal"),
                           multiple = TRUE,
                           options  = list(plugins = list('remove_button'))),
            selectizeInput(inputId  = "chlamydiaOnderzoek",
                           label    = "Uitgevoerd chlamydia onderzoek",
                           choices  = c("Anorectaal", "Cervicaal", "Oraal", "Urethraal", "Urine", "Vaginaal"),
                           multiple = TRUE,
                           options  = list(plugins = list('remove_button')))
          )
        ),
        menuItem( 
          text = "Risicogroepen",
          tabName = "risicogroepen",
          icon = icon("exclamation-triangle"),
          
          div(id = "risicogroepen",
            selectizeInput(inputId = "risico",
                           label   = "Risicogroepen",
                           choices = c("MSM", "Gewaarschuwd", "Klachten",
                                       "<25 jaar",
                                       "Afkomstig uit soa endemischgebied", "Partner risicogroep",
                                       "Prostituee", "Afgelopen jaar GO, CT, SYF gehad", 
                                       "Afgelopen jaar een positieve hiv test gehad"),
                           multiple = TRUE,
                           options  = list(plugins = list('remove_button'))),
            selectizeInput(inputId  = "prep",
                           label    = "PrEP gebruik",
                           choices  = c("Nee", "Ja, in de afgelopen 6 maanden", "Ja, langer dan 6 maanden geleden", "Onbekend"),
                           multiple = TRUE,
                           options  = list(plugins = list('remove_button')))
          )
        ),
        br(),
        actionButton("resetAll", "Reset alle filters")
      )
    ),
  ################################## tab 2 #####################################
    shinyjs::hidden(
    div(id = "tab2_sidebar",
      sidebarMenu(
        menuItem( 
          text = "Filters",
          icon = icon("thumb-tack")),
        
        div(id = "datum_tab2",
          dateRangeInput(inputId   = "datum_tab2",
                         language  = "nl",
                         separator = " tot ",
                         label     = "Consultdatum",
                         start     = min(md$Datum.consult), 
                         end       = max(md$Datum.consult))
          ),
          menuItem( 
            text = "Achtergrond kenmerken",
            tabName = "achtergrond_tab2",
            icon = icon("user"),
            div(id = "achtergrond_tab2",
                dateRangeInput(inputId   = "year_tab2",
                               language  = "nl",
                               separator = " tot ",
                               label     = "Geboortejaar",
                               start     = min(md$Geboortejaar, na.rm = TRUE), 
                               end       = max(md$Geboortejaar, na.rm = TRUE),
                               startview = "year"),
                selectizeInput(inputId  = "ggd_tab2",
                               label    = "GGD regio",
                               choices  = sort(unique(md$GGD)),
                               multiple = TRUE,
                               options  = list(plugins = list('remove_button'))),
                selectizeInput(inputId  = "sekse_tab2",
                               label    = "Sekse",
                               choices  = unique(md$Sekse),
                               multiple = TRUE,
                               options  = list(plugins= list('remove_button'))),
                selectizeInput(inputId  = "voorkeur_tab2",
                               label    = "Seksuele voorkeur",
                               choices  = unique(md$Seksuele.voorkeur),
                               multiple = TRUE,
                               options  = list(plugins= list('remove_button'))),
                selectizeInput(inputId  = "opleiding_tab2",
                               label    = "Huidige of laatst genoten opleiding",
                               choices  = c("HBO/WO", "HAVO/VWO", "LBO/MAVO/VMBO/MBO", "Basisonderwijs", 
                                            "Anders", "Geen", "Onbekend"),
                               multiple = TRUE,
                               options  = list(plugins= list('remove_button'))),
                selectizeInput(inputId  = "patient_tab2",
                               label    = "Geboorteland client",
                               choices  = sort(unique(md$Geboorteland.patient)),
                               multiple = TRUE,
                               options  = list(plugins= list('remove_button'))),
                selectizeInput(inputId  = "woonplaats_tab2",
                               label    = "Woonplaats client",
                               choices  = sort(unique(md$woonplaats)),
                               multiple = TRUE,
                               options  = list(plugins= list('remove_button')))
            )
          ),
          menuItem( 
            text = "SOA testen",
            tabName = "tests_tab2",
            icon = icon("tint"),
            div(id = "tests_tab2",
                selectizeInput(inputId  = "testlocatie_tab2",
                               label    = "Testlocatie",
                               choices  = c("Regulier", "Thuistest pakket", "Man tot Man", "Onbekend"),
                               multiple = TRUE,
                               options  = list(plugins = list('remove_button'))),
                selectizeInput(inputId  = "geolocatie_tab2",
                               label    = "Testlocatie geografisch",
                               choices  = sort(unique(md$testlocatie)),
                               multiple = TRUE,
                               options  = list(plugins = list('remove_button')))
            )
          ),
          menuItem( 
            text = "Risicogroepen",
            tabName = "risicogroepen_tab2",
            icon = icon("exclamation-triangle"),
            div(id = "risicogroepen_tab2",
                selectizeInput(inputId = "risico_tab2",
                               label   = "Risicogroepen",
                               choices = c("MSM", "Gewaarschuwd", "Klachten",
                                           "<25 jaar",
                                           "Afkomstig uit soa endemischgebied", "Partner risicogroep",
                                           "Prostituee", "Afgelopen jaar GO, CT, SYF gehad", 
                                           "Afgelopen jaar een positieve hiv test gehad"),
                               multiple = TRUE,
                               options  = list(plugins = list('remove_button'))),
                selectizeInput(inputId  = "prep_tab2",
                               label    = "PrEP gebruik",
                               choices  = c("Nee", "Ja, in de afgelopen 6 maanden", "Ja, langer dan 6 maanden geleden", "Onbekend"),
                               multiple = TRUE,
                               options  = list(plugins = list('remove_button')))
            )
          ),
          br(),
          HTML("<br>&nbsp; &nbsp; &nbsp; Groepen vergelijken:
               <br>&nbsp; &nbsp; &nbsp; Kies uit 1 van onderstaande filters: 
               <br><h6>&nbsp; &nbsp; &nbsp; &nbsp; (Meerdere opties per filter zijn mogelijk)</h6>"),
          menuItem( 
            text = "Geografisch",
            tabName = "regio_gtab2",
            icon = icon("globe"),
            div(id = "regio_gtab2",
              selectizeInput(inputId  = "ggd_gtab2",
                             label    = "GGD regio",
                             choices  = sort(unique(md$GGD)),
                             multiple = TRUE,
                             options  = list(plugins = list('remove_button'))),
              selectizeInput(inputId  = "geolocatie_gtab2",
                             label    = "Testlocatie geografisch",
                             choices  = sort(unique(md$testlocatie)),
                             multiple = TRUE,
                             options  = list(plugins = list('remove_button'))),
              selectizeInput(inputId  = "woonplaats_gtab2",
                             label    = "Woonplaats client",
                             choices  = sort(unique(md$woonplaats)),
                             multiple = TRUE,
                             options  = list(plugins= list('remove_button')))
            )
          ),
          menuItem( 
            text = "Achtergrond kenmerken",
            tabName = "achtergrond_gtab2",
            icon = icon("user"),
            div(id = "achtergrond_gtab2",
              selectizeInput(inputId  = "sekse_gtab2",
                             label    = "Sekse",
                             choices  = unique(md$Sekse),
                             multiple = TRUE,
                             options  = list(plugins= list('remove_button'))),
              selectizeInput(inputId  = "voorkeur_gtab2",
                             label    = "Seksuele voorkeur",
                             choices  = unique(md$Seksuele.voorkeur),
                             multiple = TRUE,
                             options  = list(plugins= list('remove_button'))),
              selectizeInput(inputId  = "opleiding_gtab2",
                             label    = "Huidige of laatst genoten opleiding",
                             choices  = c("HBO/WO", "HAVO/VWO", "LBO/MAVO/VMBO/MBO", "Basisonderwijs", 
                                          "Anders", "Geen", "Onbekend"),
                             multiple = TRUE,
                             options  = list(plugins= list('remove_button')))
              )
            ),
          menuItem( 
            text = "Risicogroepen",
            tabName = "risicogroepen_gtab2",
            icon = icon("exclamation-triangle"),
            div(id = "risicogroepen_gtab2",
              selectizeInput(inputId = "risico_gtab2",
                             label   = "Risicogroepen",
                             choices = c("MSM", "Gewaarschuwd", "Klachten",
                                         "<25 jaar",
                                         "Afkomstig uit soa endemischgebied", "Partner risicogroep",
                                         "Prostituee", "Afgelopen jaar GO, CT, SYF gehad", 
                                         "Afgelopen jaar een positieve hiv test gehad"),
                             multiple = TRUE,
                             options  = list(plugins = list('remove_button'))),
              
              selectizeInput(inputId  = "prep_gtab2",
                             label    = "PrEP gebruik",
                             choices  = c("Nee", "Ja, in de afgelopen 6 maanden", "Ja, langer dan 6 maanden geleden", "Onbekend"),
                             multiple = TRUE,
                             options  = list(plugins = list('remove_button')))
            )
        ),
        br(),
        actionButton("resetAll_tab2", "Reset alle filters")
      )
    )
  ),
  shinyjs::hidden(
    div(id = "tab3_sidebar",
        sidebarMenu(
          menuItem( 
            text = "Filters",
            icon = icon("thumb-tack")),
          
          div(id = "datum_tab3",
              dateRangeInput(inputId   = "datum_tab3",
                             language  = "nl",
                             separator = " tot ",
                             label     = "Consultdatum",
                             start     = min(md$Datum.consult), 
                             end       = max(md$Datum.consult))
          ),
          menuItem( 
            text = "Achtergrond kenmerken",
            tabName = "achtergrond_tab3",
            icon = icon("user"),
            div(id = "achtergrond_tab3",
              dateRangeInput(inputId   = "year_tab3",
                             language  = "nl",
                             separator = " tot ",
                             label     = "Geboortejaar",
                             start     = min(md$Geboortejaar, na.rm = TRUE), 
                             end       = max(md$Geboortejaar, na.rm = TRUE),
                             startview = "year"),
              selectizeInput(inputId  = "ggd_tab3",
                             label    = "GGD regio",
                             choices  = sort(unique(md$GGD)),
                             multiple = TRUE,
                             options  = list(plugins = list('remove_button'))),
              selectizeInput(inputId  = "sekse_tab3",
                             label    = "Sekse",
                             choices  = unique(md$Sekse),
                             multiple = TRUE,
                             options  = list(plugins= list('remove_button'))),
              selectizeInput(inputId  = "voorkeur_tab3",
                             label    = "Seksuele voorkeur",
                             choices  = unique(md$Seksuele.voorkeur),
                             multiple = TRUE,
                             options  = list(plugins= list('remove_button'))),
              selectizeInput(inputId  = "opleiding_tab3",
                             label    = "Huidige of laatst genoten opleiding",
                             choices  = c("HBO/WO", "HAVO/VWO", "LBO/MAVO/VMBO/MBO", "Basisonderwijs", 
                                          "Anders", "Geen", "Onbekend"),
                             multiple = TRUE,
                             options  = list(plugins= list('remove_button'))),
              selectizeInput(inputId  = "patient_tab3",
                             label    = "Geboorteland client",
                             choices  = sort(unique(md$Geboorteland.patient)),
                             multiple = TRUE,
                             options  = list(plugins= list('remove_button'))),
              selectizeInput(inputId  = "woonplaats_tab3",
                             label    = "Woonplaats client",
                             choices  = sort(unique(md$woonplaats)),
                             multiple = TRUE,
                             options  = list(plugins= list('remove_button')))
            )
          ),
          menuItem( 
            text    = "SOA diagnose",
            tabName = "soa_tab3",
            icon    = icon("search"),
            div(id = "soa_tab3",
              selectizeInput(inputId  = "gonorroe_tab3",
                             label    = "Gonorroe",
                             choices  = c("Positief", "Negatief", "Niet uitgevoerd"),
                             multiple = TRUE,
                             options  = list(plugins = list('remove_button'))),
              selectizeInput(inputId  = "chlamydia_tab3",
                             label    = "Chlamydia",
                             choices  = c("Positief", "Negatief", "Niet uitgevoerd"),
                             multiple = TRUE,
                             options  = list(plugins = list('remove_button'))),
              selectizeInput(inputId  = "serovartypering_tab3",
                             label    = "LGV",
                             choices  = c("Positief", "Negatief", "Niet te beoordelen", "Niet uitgevoerd"),
                             multiple = TRUE,
                             options  = list(plugins = list('remove_button'))),
              selectizeInput(inputId  = "syfilis_tab3",
                             label    = "Syfilis",
                             choices  = c("Positief", "Negatief", "Niet uitgevoerd"),
                             multiple = TRUE,
                             options  = list(plugins = list('remove_button'))),
              selectizeInput(inputId  = "hiv_tab3",
                             label    = "HIV",
                             choices  = c("Positief", "Negatief", "Niet uitgevoerd"),
                             multiple = TRUE,
                             options  = list(plugins = list('remove_button'))),
              selectizeInput(inputId  = "hepatitis.b_tab3",
                             label    = "Hepatitis B",
                             choices  = c("Positief", "Negatief", "Niet uitgevoerd"),
                             multiple = TRUE,
                             options  = list(plugins = list('remove_button'))),
              selectizeInput(inputId  = "acuminata_tab3",
                             label    = "Condylomata acuminata",
                             choices  = c("Positief", "Negatief", "Niet uitgevoerd"),
                             multiple = TRUE,
                             options  = list(plugins = list('remove_button'))),
              selectizeInput(inputId  = "hepatitis.c_tab3",
                             label    = "Hepatitis C",
                             choices  = c("Positief", "Negatief", "Niet uitgevoerd"),
                             multiple = TRUE,
                             options  = list(plugins = list('remove_button')))
            )
          ),
          menuItem( 
            text = "SOA testen",
            tabName = "tests_tab3",
            icon = icon("tint"),
            div(id = "tests_tab3",
              selectizeInput(inputId  = "testlocatie_tab3",
                             label    = "Testlocatie",
                             choices  = c("Regulier", "Thuistest pakket", "Man tot Man", "Onbekend"),
                             multiple = TRUE,
                             options  = list(plugins = list('remove_button'))),
              selectizeInput(inputId  = "geolocatie_tab3",
                             label    = "Testlocatie geografisch",
                             choices  = sort(unique(md$testlocatie)),
                             multiple = TRUE,
                             options  = list(plugins = list('remove_button')))
            )
          ),
          br(),
          HTML("<br>&nbsp; &nbsp; &nbsp; Groepen vergelijken:
               <br>&nbsp; &nbsp; &nbsp; Kies uit 1 van onderstaande filters: 
               <br><h6>&nbsp; &nbsp; &nbsp; &nbsp; (Meerdere opties per filter zijn mogelijk)</h6>"),
          menuItem( 
            text = "Geografisch",
            tabName = "regio_gtab3",
            icon = icon("globe"),
            div(id = "regio_gtab3",
              selectizeInput(inputId  = "ggd_gtab3",
                             label    = "GGD regio",
                             choices  = sort(unique(md$GGD)),
                             multiple = TRUE,
                             options  = list(plugins = list('remove_button'))),
              selectizeInput(inputId  = "geolocatie_gtab3",
                             label    = "Testlocatie geografisch",
                             choices  = sort(unique(md$testlocatie)),
                             multiple = TRUE,
                             options  = list(plugins = list('remove_button'))),
              selectizeInput(inputId  = "woonplaats_gtab3",
                             label    = "Woonplaats client",
                             choices  = sort(unique(md$woonplaats)),
                             multiple = TRUE,
                             options  = list(plugins= list('remove_button')))
            )
          ),
          menuItem( 
            text = "Achtergrond kenmerken",
            tabName = "achtergrond_gtab3",
            icon = icon("user"),
            div(id = "achtergrond_gtab3",
              selectizeInput(inputId  = "sekse_gtab3",
                             label    = "Sekse",
                             choices  = unique(md$Sekse),
                             multiple = TRUE,
                             options  = list(plugins= list('remove_button'))),
              selectizeInput(inputId  = "voorkeur_gtab3",
                             label    = "Seksuele voorkeur",
                             choices  = unique(md$Seksuele.voorkeur),
                             multiple = TRUE,
                             options  = list(plugins= list('remove_button'))),
              selectizeInput(inputId  = "opleiding_gtab3",
                             label    = "Huidige of laatst genoten opleiding",
                             choices  = c("HBO/WO", "HAVO/VWO", "LBO/MAVO/VMBO/MBO", 
                                          "Basisonderwijs", "Anders", "Geen", "Onbekend"),
                             multiple = TRUE,
                             options  = list(plugins= list('remove_button')))
            )
          ),
          menuItem( 
            text = "Risicogroepen",
            tabName = "tests_gtab3",
            icon = icon("tint"),
            div(id = "tests_gtab3",
              # selectizeInput(inputId = "soa_gtab3",
              #                label   = "SOA",
              #                choices = c("Gonorroe", "Chlamydia", "LGV", "Syfilis",
              #                            "HIV", "Hepatitis B", "Condylomata acuminata",
              #                            "Hepatitis C"),
              #                multiple = TRUE,
              #                options  = list(plugins = list('remove_button'))),
              selectizeInput(inputId  = "prep_gtab3",
                             label    = "PrEP gebruik",
                             choices  = c("Nee", "Ja, in de afgelopen 6 maanden", 
                                          "Ja, langer dan 6 maanden geleden", "Onbekend"),
                             multiple = TRUE,
                             options  = list(plugins = list('remove_button')))
            )
          ),
          br(),
          actionButton("resetAll_tab3", "Reset alle filters")
          )
    )
  )
)

##### body ####################################################################
body <- dashboardBody(

  tags$head(includeCSS("www/custom.css")),
  
  #useShinyjs(),
#  fluidRow(
    tabsetPanel(id = "navbar",
      tabPanel("Overzicht", id = "tab1", value = 'tab1_val',
#        fluidPage(
          fluidRow(
            br(),
            infoBoxOutput("value1"),
            infoBoxOutput("value2"),
            infoBoxOutput("value3")
          ),
          fluidRow(
            box(
              title        = "Achtergrondkenmerken - Geregistreerde cases",                              
              status       = "primary",
              solidHeader  = TRUE,
              width        = 12,
              column(width = 6,
                     column(12, offset = 0, style = 'padding:0px; margin-left:-20px; margin-top:10px; margin-bottom:10px;',
                            plotlyOutput("plot1a", height = 250))),
              column(width = 6,
                     column(12, offset = 0, style = 'padding:0px; margin-left:-20px; margin-top:10px; margin-bottom:10px;',
                            plotlyOutput("plot1b", height = 250))),
              column(width = 6,
                     column(12, offset = 0, style = 'padding:0px; margin-left:0px; margin-top:10px; margin-bottom:0px;',
                            plotlyOutput("plot1c", height = 400))),
              column(width = 6,
                     column(12, offset = 0, style = 'padding:0px; margin-left:0px; margin-top:10px; margin-bottom:0px;',
                            plotlyOutput("plot1d", height = 400)))
            )
          ),
          fluidRow(
            box(
              title        = "Trend vindpercentage - Positief geregistreerde cases",
              status       = "primary",
              solidHeader  = TRUE,
              width        = 12,
              column(width = 12,
                     column(12,
                            plotlyOutput("plot2", height = 600),
                            style = 'padding:0px; margin-left:-10px; margin-top:10px; margin-bottom:0px;'))
            )
          ),
          fluidRow(
            box(
              title        = "Kaart - Positief geregistreerde cases",
              status       = "primary",
              solidHeader  = TRUE,
              width        = 12,
              column(width = 12,
                     actionButton("recalc", "Update kaart"), actionButton("recalc_posNeg", "Update + Negatieve cases"),
                     p(),
                     leafletOutput("mymap", height = 600))
            )
          ),
          # fluidRow(
          #   box(
          #     title        = "Data viewer",
          #     status       = "primary",
          #     solidHeader  = TRUE,
          #     width        = 12,
          #     column(width = 12, 
          #            dataTableOutput("data_table"))
          #   )
          # ),
          fluidRow(
            box(
              title = "Deze tool is powered by", 
              status = "primary", 
              solidHeader = TRUE, 
              width = 12,
              column(width   = 4, 
                     align   = "left",
                     tags$img(src = "GGD WB_klein.png",  height='52px', width='137px')),
              column(width   = 4, 
                     align   = "center",
                     tags$img(src = "GGD HvB_klein.png", height='52px', width='137px')),
              column(width   = 4, 
                     align   = "right",
                     tags$img(src = "GGD BZO_klein.png", height='52px', width='137px'))
            )
          )
      ),
      tabPanel("Vergelijken vindpercentage", id = "tab2", value = 'tab2_val',
         fluidRow(
           br(),
           infoBoxOutput("value1_tab2"),
           infoBoxOutput("value2_tab2"),
           infoBoxOutput("value3_tab2")
         ),
         fluidRow(
           box(
             title        = "Trend vindpercentage - Positief geregistreerde cases",
             status       = "primary",
             solidHeader  = TRUE,
             width        = 12,
             column(width = 12,
                    column(12,
                           plotlyOutput("trendVindpercentage", height = 600),
                           style = 'padding:0px; margin-left:-10px; margin-top:10px; margin-bottom:0px;'))
           )
         )
      ),
      tabPanel("Vergelijken risicogroepen", id = "tab3", value = 'tab3_val',
       fluidRow(
         br(),
         infoBoxOutput("value1_tab3"),
         infoBoxOutput("value2_tab3"),
         infoBoxOutput("value3_tab3")
       ),
       fluidRow(
         box(
           title        = "Trend risicogroepen",
           status       = "primary",
           solidHeader  = TRUE,
           width        = 12,
           column(width = 12,
                  column(12,
                         plotlyOutput("trendRisicogroepen", height = 800),
                         style = 'padding:0px; margin-left:-10px; margin-top:10px; margin-bottom:0px;'))
         )
      )
    )
  )
)




#################################### ui generation ########################################
ui <- dashboardPage(title = "SOAtool",
                    header, sidebar, body)