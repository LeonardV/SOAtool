source('global.R', local = TRUE)

############################ server ##############################
server <- function(input, output, session) {
  
  observeEvent(input$navbar, {
    toggle("tab1_sidebar", condition = input$navbar == "tab1_val")
    toggle("tab2_sidebar", condition = input$navbar == "tab2_val")
    toggle("tab3_sidebar", condition = input$navbar == "tab3_val")
  })
  

  observeEvent(input$resetAll, {
    reset("regio")
    reset("achtergrond")
    reset("soa")
    reset("tests")
    reset("risicogroepen")
  })
  
  
  observeEvent(input$resetAll_tab2, {
    reset("datum_tab2")
    reset("achtergrond_tab2")
    reset("tests_tab2")
    reset("risicogroepen_tab2")
    
    reset("regio_gtab2")
    reset("achtergrond_gtab2")
    reset("risicogroepen_gtab2")

    enable(id = "ggd_gtab2")
    enable(id = "geolocatie_gtab2")
    enable(id = "sekse_gtab2")
    enable(id = "voorkeur_gtab2")
    enable(id = "opleiding_gtab2")
    enable(id = "woonplaats_gtab2")
    enable(id = "prep_gtab2")
    enable(id = "risico_gtab2")
  })
  
  observeEvent(input$resetAll_tab3, {
    reset("datum_tab3")
    reset("achtergrond_tab3")
    reset("soa_tab3")
    reset("tests_tab3")
    
    reset("regio_gtab3")
    reset("achtergrond_gtab3")
    reset("tests_gtab3")
    
    enable(id = "ggd_gtab3")
    enable(id = "geolocatie_gtab3")
    enable(id = "sekse_gtab3")
    enable(id = "voorkeur_gtab3")
    enable(id = "opleiding_gtab3")
    enable(id = "woonplaats_gtab3")
    enable(id = "prep_gtab3")
  })
  

  observeEvent(input$openModal, {
    showModal(
      modalDialog(title = "",
                  includeMarkdown("www/about.md"))
    )
  })
  
  observeEvent(input$openModal2, {
    showModal(
      modalDialog(title = "",
                  includeMarkdown("www/history.md"))
    )
  })
  
    dataInput <- reactive({
      dataInput <- 
        if (input$navbar == "tab1_val") {
          md %>%
            filter(Datum.consult <= max(input$datum)) %>% 
            filter(Datum.consult >= min(input$datum)) 
        } else if (input$navbar == "tab2_val") {
          md %>%
            filter(Datum.consult <= max(input$datum_tab2)) %>% 
            filter(Datum.consult >= min(input$datum_tab2)) 
        } else if (input$navbar == "tab3_val") {
          md %>%
            filter(Datum.consult <= max(input$datum_tab3)) %>% 
            filter(Datum.consult >= min(input$datum_tab3)) 
        }
    }) 
  

    df <- reactive({
      df2 <- dataInput()
      df2 <- 
        if (input$navbar == "tab1_val") {
          df2 %<>%  
            filter(Geboortejaar_mod >= min(as.numeric(format(input$year, '%Y')))) %>%
            filter(Geboortejaar_mod <= max(as.numeric(format(input$year, '%Y'))))
          if (!is.null(input$sekse)) {
            df2 %<>% filter( Sekse %in% input$sekse )
          }
          if (!is.null(input$voorkeur)) {
            df2 %<>% filter( Seksuele.voorkeur %in% input$voorkeur )
          }
          if (!is.null(input$patient)) {
            df2 %<>% filter( Geboorteland.patient %in% input$patient )
          }
          if (!is.null(input$opleiding)) {
            df2 %<>% filter( Huidige.of.laatst.genoten.opleiding %in% input$opleiding )
          }
          if (!is.null(input$woonplaats)) {
            df2 %<>% filter( woonplaats %in% input$woonplaats )
          }
          if (!is.null(input$ggd)) {
            df2 %<>% filter( GGD %in% input$ggd )
          }
          if (!is.null(input$geolocatie)) {
            df2 %<>% filter(testlocatie %in% input$geolocatie)
          }
          if (!is.null(input$testlocatie)) {
            df2 %<>% filter(Soa.consult %in% input$testlocatie)
          }
          if (!is.null(input$gonorroe)) {
            df2 %<>% filter( Diagnose.gonorroe %in% input$gonorroe ) 
          }
          if (!is.null(input$gonorroeOnderzoek)) {
            if ("Cervicaal" %in% input$gonorroeOnderzoek) {
              idx.NAAT.Cervicaal <- unique(grep(6,  df2$Uitgevoerd.gonorroe.onderzoek), grep("NAAT Cervicaal", df2$Uitgevoerd.gonorroe.onderzoek))
              df2$NAAT.Cervicaal <- 0L
              df2$NAAT.Cervicaal[idx.NAAT.Cervicaal] <- 1L
              df2 %<>% filter(NAAT.Cervicaal %in% 1L)
            }
            if ("Urethraal" %in% input$gonorroeOnderzoek) {
              idx.NAAT.Urethraal <- unique(grep(7,  df2$Uitgevoerd.gonorroe.onderzoek), grep("NAAT Urethraal", df2$Uitgevoerd.gonorroe.onderzoek))
              df2$NAAT.Urethraal <- 0L
              df2$NAAT.Urethraal[idx.NAAT.Urethraal] <- 1L
              df2 %<>% filter(NAAT.Urethraal %in% 1L)
            }
            if ("Anaal" %in% input$gonorroeOnderzoek) {
              idx.NAAT.Anaal <- unique(grep(8,  df2$Uitgevoerd.gonorroe.onderzoek), grep("NAAT Anaal", df2$Uitgevoerd.gonorroe.onderzoek))
              df2$NAAT.Anaal <- 0L
              df2$NAAT.Anaal[idx.NAAT.Anaal] <- 1L
              df2 %<>% filter(NAAT.Anaal %in% 1L)
            }
            if ("Oraal" %in% input$gonorroeOnderzoek) {
              idx.NAAT.Oraal <- unique(grep(10, df2$Uitgevoerd.gonorroe.onderzoek), grep("NAAT Oraal", df2$Uitgevoerd.gonorroe.onderzoek))
              df2$NAAT.Oraal <- 0L
              df2$NAAT.Oraal[idx.NAAT.Oraal] <- 1L
              df2 %<>% filter(NAAT.Oraal %in% 1L)
            }
            if ("Urine" %in% input$gonorroeOnderzoek) {
              idx.NAAT.Urine  <- unique(grep(11, df2$Uitgevoerd.gonorroe.onderzoek), grep("NAAT Urine", df2$Uitgevoerd.gonorroe.onderzoek))
              df2$NAAT.Urine <- 0L
              df2$NAAT.Urine[idx.NAAT.Urine] <- 1L
              df2 %<>% filter(NAAT.Urine %in% 1L)
            }
            if ("Vaginaal" %in% input$gonorroeOnderzoek) {
              idx.NAAT.Vaginaal <- unique(grep(12, df2$Uitgevoerd.gonorroe.onderzoek), grep("NAAT Vaginaal", df2$Uitgevoerd.gonorroe.onderzoek))
              # some men have had a vaginaal examination. This is pretty weird.
              idx.sekse.vrouw <- which(df2$Sekse == "Vrouw")
              df2$NAAT.Vagiaal <- 0L
              df2$NAAT.Vagiaal[intersect(idx.sekse.vrouw, idx.NAAT.Vaginaal)] <- 1L
              df2 %<>% filter(NAAT.Vagiaal %in% 1L)
            }
          }
          if (!is.null(input$chlamydia)) {
            df2 %<>% filter( Diagnose.chlamydia %in% input$chlamydia ) 
          }
          if (!is.null(input$chlamydiaOnderzoek)) {
            if ("Cervicaal" %in% input$chlamydiaOnderzoek) {
              idx.Cervicaal <- unique(grep(3,  df2$Uitgevoerd.chlamydia.onderzoek), grep("Cervicaal", df2$Uitgevoerd.chlamydia.onderzoek))
              df2$Cervicaal <- 0L
              df2$Cervicaal[idx.Cervicaal] <- 1L
              df2 %<>% filter(Cervicaal %in% 1L)
            }
            if ("Urethraal" %in% input$chlamydiaOnderzoek) {
              idx.Urethraal <- unique(grep(1,  df2$Uitgevoerd.chlamydia.onderzoek), grep("Urethraal", df2$Uitgevoerd.chlamydia.onderzoek))
              df2$Urethraal <- 0L
              df2$Urethraal[idx.Urethraal] <- 1L
              df2 %<>% filter(Urethraal %in% 1L)
            }
            if ("Anorectaal" %in% input$chlamydiaOnderzoek) {
              idx.Anorectaal <- unique(grep(2,  df2$Uitgevoerd.chlamydia.onderzoek), grep("Anorectaal", df2$Uitgevoerd.chlamydia.onderzoek))
              df2$Anorectaal <- 0L
              df2$Anorectaal[idx.Anorectaal] <- 1L
              df2 %<>% filter(Anorectaal %in% 1L)
            }
            if ("Oraal" %in% input$chlamydiaOnderzoek) {
              idx.Oraal <- unique(grep(4, df2$Uitgevoerd.chlamydia.onderzoek), grep("Oraal", df2$Uitgevoerd.chlamydia.onderzoek))
              df2$Oraal <- 0L
              df2$Oraal[idx.Oraal] <- 1L
              df2 %<>% filter(Oraal %in% 1L)
            }
            if ("Urine" %in% input$chlamydiaOnderzoek) {
              idx.Urine  <- unique(grep(5, df2$Uitgevoerd.chlamydia.onderzoek), grep("Urine", df2$Uitgevoerd.chlamydia.onderzoek))
              df2$Urine <- 0L
              df2$Urine[idx.Urine] <- 1L
              df2 %<>% filter(Urine %in% 1L)
            }
            if ("Vaginaal" %in% input$chlamydiaOnderzoek) {
              idx.Vaginaal <- unique(grep(6, df2$Uitgevoerd.chlamydia.onderzoek), grep("Vaginaal", df2$Uitgevoerd.chlamydia.onderzoek))
              # some men have had a vaginaal examination. This is pretty weird.
              idx.sekse.vrouw <- which(df2$Sekse == "Vrouw")
              df2$Vagiaal <- 0L
              df2$Vagiaal[intersect(idx.sekse.vrouw, idx.Vaginaal)] <- 1L
              df2 %<>% filter(Vagiaal %in% 1L)
            }
          }
          if (!is.null(input$serovartypering)) {
            df2 %<>% filter( Uitslag.serovartypering %in% input$serovartypering ) 
          }
          if (!is.null(input$syfilis)) {
            df2 %<>% filter( Diagnose.syfilis %in% input$syfilis ) 
          }
          if (!is.null(input$hiv)) {
            df2 %<>% filter( Diagnose.HIV %in% input$hiv ) 
          }
          if (!is.null(input$hepatitis.b)) {
            df2 %<>% filter( Diagnose.hepatitis.B %in% input$hepatitis.b) 
          }
          if (!is.null(input$acuminata)) {
            df2 %<>% filter( Diagnose.condylomata.acuminata %in% input$acuminata ) 
          }
          if (!is.null(input$hepatitis.c)) {
            df2 %<>% filter( Diagnose.hepatitis.C %in% input$hepatitis.c ) 
          }
          
          if (!is.null(input$risico)) {
            if ("MSM" %in% input$risico) {
              df2 %<>% filter( MSM %in% "Ja")
            }
            if ("Gewaarschuwd" %in% input$risico) {
              df2 %<>% filter( Gewaarschuwd.door.partner %in% "Ja")
            }
            if ("Klachten" %in% input$risico) {
              df2 %<>% filter( Soa.gerelateerde.klachten %in% "Ja")
            }
            if ("<25 jaar" %in% input$risico) {
              #cyear <- as.integer(format(Sys.Date(), "%Y"))
              df2 %<>% filter( (datum_consult_year - Geboortejaar_mod) < 25)
            }
            if ("Afkomstig uit soa endemischgebied" %in% input$risico) {
              df2 %<>% filter( Geboorteland.patient.endemisch %in% "Ja")
            }
            if ("Partner risicogroep" %in% input$risico) {
              df2 %<>% filter( (Partner.uit.doelgroep %in% "Ja") & (Welke.doelgroep %in% c("Partner endemisch", "MSM")))
            }
            if ("Prostituee" %in% input$risico) {
              df2 %<>% filter( Prostituee %in% "Ja")
            }
            if ("Afgelopen jaar GO, CT, SYF gehad" %in% input$risico) {
              df2 %<>% filter( In.de.afgelopen.jaar.GO_CT_SYF.gehad %in% "Ja")
            }
            if ("Afgelopen jaar een positieve hiv test gehad" %in% input$risico) {
              df2 %<>% filter( Eerder.HIV.test.met.uitslag %in% "jaPositief")
            }
          }
           if (!is.null(input$prep)) {
            df2 %<>% filter( ooit.prep.gebruikt %in% input$prep ) 
           }
            df2 %>% select(-datum_consult_mod)
        } 
############################ input filters tab 2 ###############################        
        else if (input$navbar == "tab2_val") {
          df2 %<>%  
            filter(Geboortejaar_mod >= min(as.numeric(format(input$year_tab2, '%Y')))) %>%
            filter(Geboortejaar_mod <= max(as.numeric(format(input$year_tab2, '%Y'))))
          
          if (!is.null(input$ggd_tab2)) {
            df2 %<>% filter( GGD %in% input$ggd_tab2)
          }
          if (!is.null(input$geolocatie_tab2)) {
            df2 %<>% filter(testlocatie %in% input$geolocatie_tab2)
          }
          if (!is.null(input$testlocatie_tab2)) {
            df2 %<>% filter(Soa.consult %in% input$testlocatie_tab2)
          }
          if (!is.null(input$sekse_tab2)) {
            df2 %<>% filter( Sekse %in% input$sekse_tab2 )
          }
          if (!is.null(input$voorkeur_tab2)) {
            df2 %<>% filter( Seksuele.voorkeur %in% input$voorkeur_tab2 )
          }
          if (!is.null(input$patient_tab2)) {
            df2 %<>% filter( Geboorteland.patient %in% input$patient_tab2 )
          }
          if (!is.null(input$opleiding_tab2)) {
            df2 %<>% filter( Huidige.of.laatst.genoten.opleiding %in% input$opleiding_tab2 )
          }
          if (!is.null(input$woonplaats_tab2)) {
            df2 %<>% filter( woonplaats %in% input$woonplaats_tab2 )
          }
          if (!is.null(input$prep_tab2)) {
            df2 %<>% filter( ooit.prep.gebruikt %in% input$prep_tab2 ) 
          }
          if (!is.null(input$risico_tab2)) {
            if ("MSM" %in% input$risico_tab2) {
              df2 %<>% filter( MSM %in% "Ja")
            }
            if ("Gewaarschuwd" %in% input$risico_tab2) {
              df2 %<>% filter( Gewaarschuwd.door.partner %in% "Ja")
            }
            if ("Klachten" %in% input$risico_tab2) {
              df2 %<>% filter( Soa.gerelateerde.klachten %in% "Ja")
            }
            if ("<25 jaar" %in% input$risico_tab2) {
              #cyear <- as.integer(format(Sys.Date(), "%Y"))
              df2 %<>% filter( (datum_consult_year - Geboortejaar_mod) < 25)
            }
            if ("Afkomstig uit soa endemischgebied" %in% input$risico_tab2) {
              df2 %<>% filter( Geboorteland.patient.endemisch %in% "Ja")
            }
            if ("Partner risicogroep" %in% input$risico_tab2) {
              df2 %<>% filter( (Partner.uit.doelgroep %in% "Ja") & (Welke.doelgroep %in% c("Partner endemisch", "MSM")))
            }
            if ("Prostituee" %in% input$risico_tab2) {
              df2 %<>% filter( Prostituee %in% "Ja")
            }
            if ("Afgelopen jaar GO, CT, SYF gehad" %in% input$risico_tab2) {
              df2 %<>% filter( In.de.afgelopen.jaar.GO_CT_SYF.gehad %in% "Ja")
            }
            if ("Afgelopen jaar een positieve hiv test gehad" %in% input$risico_tab2) {
              df2 %<>% filter( Eerder.HIV.test.met.uitslag %in% "jaPositief")
            }
          }
          
          df2 %>% select(-datum_consult_mod)
        }
      ############################ input filters tab 3 ###############################        
      else if (input$navbar == "tab3_val") {
        df2 %<>%  
          filter(Geboortejaar_mod >= min(as.numeric(format(input$year_tab3, '%Y')))) %>%
          filter(Geboortejaar_mod <= max(as.numeric(format(input$year_tab3, '%Y'))))
        
        if (!is.null(input$geolocatie_tab3)) {
          df2 %<>% filter(testlocatie %in% input$geolocatie_tab3)
        }
        if (!is.null(input$testlocatie_tab3)) {
          df2 %<>% filter(Soa.consult %in% input$testlocatie_tab3)
        }
        if (!is.null(input$gonorroe_tab3)) {
          df2 %<>% filter( Diagnose.gonorroe %in% input$gonorroe_tab3 ) 
        }
        if (!is.null(input$chlamydia_tab3)) {
          df2 %<>% filter( Diagnose.chlamydia %in% input$chlamydia_tab3 ) 
        }
        if (!is.null(input$serovartypering_tab3)) {
          df2 %<>% filter( Uitslag.serovartypering %in% input$serovartypering_tab3 ) 
        }
        if (!is.null(input$syfilis_tab3)) {
          df2 %<>% filter( Diagnose.syfilis %in% input$syfilis_tab3 ) 
        }
        if (!is.null(input$hiv_tab3)) {
          df2 %<>% filter( Diagnose.HIV %in% input$hiv_tab3 ) 
        }
        if (!is.null(input$hepatitis.b_tab3)) {
          df2 %<>% filter( Diagnose.hepatitis.B %in% input$hepatitis.b_tab3) 
        }
        if (!is.null(input$acuminata_tab3)) {
          df2 %<>% filter( Diagnose.condylomata.acuminata %in% input$acuminata_tab3 ) 
        }
        if (!is.null(input$hepatitis.c_tab3)) {
          df2 %<>% filter( Diagnose.hepatitis.C %in% input$hepatitis.c_tab3 ) 
        }
        if (!is.null(input$ggd_tab3)) {
          df2 %<>% filter( GGD %in% input$ggd_tab3 )
        }
        if (!is.null(input$geolocatie_tab3)) {
          df2 %<>% filter(testlocatie %in% input$geolocatie_tab3)
        }
        if (!is.null(input$sekse_tab3)) {
          df2 %<>% filter( Sekse %in% input$sekse_tab3 )
        }
        if (!is.null(input$voorkeur_tab3)) {
          df2 %<>% filter( Seksuele.voorkeur %in% input$voorkeur_tab3 )
        }
        if (!is.null(input$patient_tab3)) {
          df2 %<>% filter( Geboorteland.patient %in% input$patient_tab3 )
        }
        if (!is.null(input$opleiding_tab3)) {
          df2 %<>% filter( Huidige.of.laatst.genoten.opleiding %in% input$opleiding_tab3 )
        }
        if (!is.null(input$woonplaats_tab3)) {
          df2 %<>% filter( woonplaats %in% input$woonplaats_tab3 )
        }
        if (!is.null(input$prep_tab3)) {
          df2 %<>% filter( ooit.prep.gebruikt %in% input$prep_tab3 ) 
        }
        if (!is.null(input$risico_tab3)) {
          if ("MSM" %in% input$risico_tab3) {
            df2 %<>% filter( MSM %in% "Ja")
          }
          if ("Gewaarschuwd" %in% input$risico_tab3) {
            df2 %<>% filter( Gewaarschuwd.door.partner %in% "Ja")
          }
          if ("Klachten" %in% input$risico_tab3) {
            df2 %<>% filter( Soa.gerelateerde.klachten %in% "Ja")
          }
          if ("<25 jaar" %in% input$risico_tab3) {
            #cyear <- as.integer(format(Sys.Date(), "%Y"))
            df2 %<>% filter( (datum_consult_year - Geboortejaar_mod) < 25)
          }
          if ("Afkomstig uit soa endemischgebied" %in% input$risico_tab3) {
            df2 %<>% filter( Geboorteland.patient.endemisch %in% "Ja")
          }
          if ("Partner risicogroep" %in% input$risico_tab3) {
            df2 %<>% filter( (Partner.uit.doelgroep %in% "Ja") & (Welke.doelgroep %in% c("Partner endemisch", "MSM")))
          }
          if ("Prostituee" %in% input$risico_tab3) {
            df2 %<>% filter( Prostituee %in% "Ja")
          }
          if ("Afgelopen jaar GO, CT, SYF gehad" %in% input$risico_tab3) {
            df2 %<>% filter( In.de.afgelopen.jaar.GO_CT_SYF.gehad %in% "Ja")
          }
          if ("Afgelopen jaar een positieve hiv test gehad" %in% input$risico_tab3) {
            df2 %<>% filter( Eerder.HIV.test.met.uitslag %in% "jaPositief")
          }
        }
        
        df2 %>% select(-datum_consult_mod)
      }
  })
  

################################# data viewer###################################
    # output$data_table <- renderDataTable({
    #   df() %>% select(-c(MELDINGSNR, OSIRISNR, Postcode.patient.cijfers, Postcode.patient.opties, woonplaats, 
    #                      gemeente, latitude, longitude, Geboorteland.patient, Geboorteland.moeder, Geboorteland.vader))
    # }, class = 'cell-border stripe', options = list(scrollX = TRUE,
    #                                                 columnDefs = list(list(className = 'dt-center', targets = 5)),
    #                                                 pageLength = 5,
    #                                                 lengthMenu = c(5, 10, 15, 20, 50, 100)))
    
    ##################################  MAP #####################################
    
    points <- reactiveValues(data = NULL)
    
    observeEvent(input$recalc, {
      points$data <- df() %>% 
          select(c(STD.positief, longitude, latitude, woonplaats)) %>%
          filter(STD.positief == 1) %>%
        na.omit
    }, ignoreNULL = FALSE)

    observeEvent(input$recalc_posNeg, {
      points$data <- df() %>% 
        select(c(STD.positief, longitude, latitude, woonplaats)) %>%
        filter(STD.positief %in% c(0,1)) %>%
        na.omit
    })
    

    output$mymap <- renderLeaflet({
      req(nrow(points$data > 0))
      leaflet(options = leafletOptions(minZoom = 7)) %>%
        addProviderTiles(providers$CartoDB.Positron,
                         options = providerTileOptions(noWrap = TRUE, minZoom = 7, maxZoom = 18,
                                                       updateWhenZooming = FALSE, updateWhenIdle = FALSE)
        ) %>%
        addCircleMarkers(data = points$data, lng = ~longitude, lat = ~latitude, 
                         popup = ~woonplaats, clusterOptions = markerClusterOptions(spiderfyOnMaxZoom = FALSE)) 
    })
    
    ################################ Sample-size###################################
    #creating the valueBoxOutput content
    output$value1 <- output$value1_tab2 <- output$value1_tab3 <- renderInfoBox({
      infoBox(
        paste0('Aantal totaal = ', dim(md)[1]),
  #      icon = icon("stats", lib = 'glyphicon'),
        icon = icon("users"),
        color = "green")  
    })
    output$value2 <- renderInfoBox({
      infoBox(
        paste0('Aantal subset = ', dim(df())[1]),
        icon = icon("users"),
        color = "purple")  
    })
    output$value3 <- renderInfoBox({
      infoBox(
        "",#paste0('N missings = ', (sum(is.na(df())))),
        icon = icon("users"),
        color = "orange")
    })
    
    ############################### FIGURES #####################################
    #display.brewer.pal(8, "Set3")
    # brewer.pal(8, "Paired")
    
    
    plot1 <- function() {
      req(nrow(df()) > 0)
      ## Diagnose
      diagnosis <- c("Diagnose.chlamydia",
                     "Diagnose.gonorroe",
                     "Diagnose.HIV",
                     "Diagnose.syfilis",
                     "Diagnose.hepatitis.B",
                     "Diagnose.condylomata.acuminata",
                     "Diagnose.hepatitis.C",
                     "Totaal.vindpercentage")
      
      count.diag <- data.frame(row.names = diagnosis, Positief = 1:length(diagnosis), 
                               Negatief = 1:length(diagnosis))
      
      count.diag["Diagnose.chlamydia", ]["Positief"] <- sum(df()$Diagnose.chlamydia == "Positief")
      count.diag["Diagnose.chlamydia", ]["Negatief"] <- sum(df()$Diagnose.chlamydia == "Negatief")
      count.diag["Diagnose.gonorroe", ]["Positief"]  <- sum(df()$Diagnose.gonorroe == "Positief")
      count.diag["Diagnose.gonorroe", ]["Negatief"]  <- sum(df()$Diagnose.gonorroe == "Negatief")
      count.diag["Diagnose.HIV", ]["Positief"]       <- sum(df()$Diagnose.HIV == "Positief")
      count.diag["Diagnose.HIV", ]["Negatief"]       <- sum(df()$Diagnose.HIV == "Negatief")
      count.diag["Diagnose.syfilis", ]["Positief"]   <- sum(df()$Diagnose.syfilis == "Positief")
      count.diag["Diagnose.syfilis", ]["Negatief"]   <- sum(df()$Diagnose.syfilis == "Negatief")
      count.diag["Diagnose.hepatitis.B", ]["Positief"] <- sum(df()$Diagnose.hepatitis.B == "Positief")
      count.diag["Diagnose.hepatitis.B", ]["Negatief"] <- sum(df()$Diagnose.hepatitis.B == "Negatief")
      count.diag["Diagnose.condylomata.acuminata",]["Positief"] <- sum(df()$Diagnose.condylomata.acuminata == "Positief")
      count.diag["Diagnose.condylomata.acuminata",]["Negatief"] <- sum(df()$Diagnose.condylomata.acuminata == "Negatief")
      count.diag["Diagnose.hepatitis.C",]["Positief"] <- sum(df()$Diagnose.hepatitis.C == "Positief")
      count.diag["Diagnose.hepatitis.C",]["Negatief"] <- sum(df()$Diagnose.hepatitis.C == "Negatief")
      count.diag["Totaal.vindpercentage",]["Positief"] <- sum(df()$STD.positief == 1) # or you have a std or you don't. 
      count.diag["Totaal.vindpercentage",]["Negatief"] <- sum(df()$STD.positief == 0)
      
      
      names.arg <- c("Chlamydia", "Gonorroe", "HIV", "Syfilis", 
                    "Hepatitis B", "Condyloma", "Hepatitis C", "Totaal")
      
      count.diag.vec <- c(count.diag$Positief, count.diag$Negatief)
      
      df.extra <- data.frame(Diagnose   = as.factor(rep(c("Positief", "Negatief"), 
                                                    each = length(diagnosis))),
                             SOA        = as.factor(c(names.arg, names.arg)),
                             Frequentie = count.diag.vec)
      
      df.extra$Diagnose <- factor(df.extra$Diagnose, levels = c("Positief", "Negatief"))
      count.diag.per <- round(100 * df.extra$Frequentie / rowSums(count.diag), 2) 
      
      plot1 <- plot_ly(data = df.extra, y = ~count.diag.per, x = ~SOA, type = "bar", 
                       color = ~Diagnose, text =paste("Soa: ", df.extra$SOA,
                                                      "<br>Diagnose: ", df.extra$Diagnose,
                                                      "<br>Percentage: ", count.diag.per,"%",
                                                      "<br>Aantal: ", df.extra$Frequentie),
                       hoverinfo = 'text',
                       #hoverinfo = 'text',
                       colors = c("#1F78B4", "#A6CEE3")) %>%
        layout(yaxis = list(title = ""), #legend = list(orientation = 'h', y = -0.15),
               xaxis = list(title = ""), 
               barmode = "group",
               title = "Vindpercentage %",
               titlefont = list(
                 #family = "Agency FB",
                 size = 14)) %>%
        config(displaylogo = FALSE,
               collaborate = FALSE,
               modeBarButtonsToRemove = c(
                 'sendDataToCloud',
                 #'toImage',
                 'autoScale2d',
                 #'resetScale2d',
                 'hoverClosestCartesian', 
                 'hoverCompareCartesian')
        ) 
      
    }
    
    plot2 <- function() {
      req(nrow(df()) > 0)
      ## risicogroepen
      behaviour <- c("MSM",
                     "Gewaarschuwd.voor.soa.door.partner",
                     "Soa.gerelateerde.klachten",
                     "Jonger.dan.25.jaar",
                     "Afkomstig.uit.soa.endemisch.gebied",
                     "Partner.MSM.of.uit.soa.endemisch.gebied",
                     "Prostituee",
                     "Afgelopen.jaar.GO.CT.SYF.gehad",
                     "Heeft.al.een.positieve.hiv.test.gehad")

      # current year
      cyear <- as.integer(format(Sys.Date(), "%Y"))
      
      count.beha <- data.frame(row.names = behaviour, Ja = 1:length(behaviour), 
                               Nee = 1:length(behaviour), Onbekend = 1:length(behaviour))
      
      count.beha["MSM", ]["Ja"]  <- sum(df()$MSM == "Ja")
      count.beha["MSM", ]["Nee"] <- sum(df()$MSM == "Nee")
      count.beha["MSM", ]["Onbekend"] <- 0L
      count.beha["Gewaarschuwd.voor.soa.door.partner", ]["Ja"]        <- sum(df()$Gewaarschuwd.door.partner == "Ja")
      count.beha["Gewaarschuwd.voor.soa.door.partner", ]["Nee"]       <- sum(df()$Gewaarschuwd.door.partner == "Nee")
      count.beha["Gewaarschuwd.voor.soa.door.partner", ]["Onbekend"]  <- sum(df()$Gewaarschuwd.door.partner == "Onbekend")
      count.beha["Soa.gerelateerde.klachten", ]["Ja"]        <- sum(df()$Soa.gerelateerde.klachten == "Ja")
      count.beha["Soa.gerelateerde.klachten", ]["Nee"]       <- sum(df()$Soa.gerelateerde.klachten == "Nee")
      count.beha["Soa.gerelateerde.klachten", ]["Onbekend"]  <- sum(df()$Soa.gerelateerde.klachten == "Onbekend")
      count.beha["Jonger.dan.25.jaar", ]["Ja"]  <- sum((df()$datum_consult_year - df()$Geboortejaar_mod) < 25, na.rm = TRUE)
      count.beha["Jonger.dan.25.jaar", ]["Nee"] <- sum((df()$datum_consult_year - df()$Geboortejaar_mod) >= 25, na.rm = TRUE)
      count.beha["Jonger.dan.25.jaar", ]["Onbekend"] <- 0L
      count.beha["Afkomstig.uit.soa.endemisch.gebied", ]["Ja"]       <- sum(df()$Geboorteland.patient.endemisch == "Ja", na.rm = TRUE)
      count.beha["Afkomstig.uit.soa.endemisch.gebied", ]["Nee"]      <- sum(df()$Geboorteland.patient.endemisch == "Nee", na.rm = TRUE)
      count.beha["Afkomstig.uit.soa.endemisch.gebied", ]["Onbekend"] <- sum(df()$Geboorteland.patient.endemisch == "Onbekend", na.rm = TRUE)
      count.beha["Partner.MSM.of.uit.soa.endemisch.gebied", ]["Ja"]       <- sum(df()$Partner.uit.doelgroep == "Ja")
      count.beha["Partner.MSM.of.uit.soa.endemisch.gebied", ]["Nee"]      <- sum(df()$Partner.uit.doelgroep == "Nee")
      count.beha["Partner.MSM.of.uit.soa.endemisch.gebied", ]["Onbekend"] <- sum(df()$Partner.uit.doelgroep == "Onbekend")
      count.beha["Prostituee", ]["Ja"]       <- sum(df()$Prostituee == "Ja")
      count.beha["Prostituee", ]["Nee"]        <- sum(df()$Prostituee == "Nee")
      count.beha["Prostituee", ]["Onbekend"]  <- sum(df()$Prostituee == "Onbekend")
      count.beha["Afgelopen.jaar.GO.CT.SYF.gehad", ]["Ja"]       <- sum(df()$In.de.afgelopen.jaar.GO_CT_SYF.gehad == "Ja")
      count.beha["Afgelopen.jaar.GO.CT.SYF.gehad", ]["Nee"]      <- sum(df()$In.de.afgelopen.jaar.GO_CT_SYF.gehad == "Nee")
      count.beha["Afgelopen.jaar.GO.CT.SYF.gehad", ]["Onbekend"] <- sum(df()$In.de.afgelopen.jaar.GO_CT_SYF.gehad == "Onbekend")
      count.beha["Heeft.al.een.positieve.hiv.test.gehad", ]["Ja"]       <- sum(df()$Eerder.HIV.test.met.uitslag == "jaPositief")
      count.beha["Heeft.al.een.positieve.hiv.test.gehad", ]["Nee"]      <- sum(df()$Eerder.HIV.test.met.uitslag == "jaNegatief")
      count.beha["Heeft.al.een.positieve.hiv.test.gehad", ]["Onbekend"] <- sum(df()$Eerder.HIV.test.met.uitslag %in% c("Nee", "jaOnbekend", "Onbekend"))

      names.arg = c("MSM", "Gewaarschuwd", "Klachten", "<25 jaar",
                    "Soa endemisch gebied", "Partner risicogroep", "Prostituee",
                    "Afgelopen jaar SOA", "Bekende HIV+")
      
      count.beha.vec <- c(count.beha$Ja, count.beha$Nee, count.beha$Onbekend)
      
      df.extra <- data.frame(Antw = as.factor(rep(c("Ja", "Nee", "Onbekend"), each = length(behaviour))),
                             gedrag = as.factor(c(names.arg, names.arg, names.arg)),
                             Frequentie = count.beha.vec)
      df.extra$Antw <- factor(df.extra$Antw, levels = c("Ja", "Nee", "Onbekend"))
      
      # check, should add up to nrow (df())
      #df.extra %>% group_by(gedrag) %>% summarize(Sum = sum(Frequentie))
      count.beha.per <- round(100 * df.extra$Frequentie / dim(df())[1], 2) 

      plot2 <- plot_ly(data = df.extra, y = ~count.beha.per, x = ~gedrag, type = "bar", 
                       color = ~Antw, text = paste("Gedrag: ", df.extra$gedrag,
                                                   "<br>Antwoord: ", df.extra$Antw,
                                                   "<br>Percentage: ", count.beha.per,"%",
                                                   "<br>Aantal: ", df.extra$Frequentie),
                       hoverinfo = 'text',
                       #hoverinfo = 'text',
                       colors = c("#1F78B4", "#A6CEE3", "#33A02C")) %>%
        layout(yaxis = list(title = ""), #legend = list(orientation = 'h', y = -0.15),
               xaxis = list(title = ""), 
               barmode = "stack",
               title = "Risicogroepen %",
               titlefont = list(
                 #family = "Agency FB",
                 size = 14)) %>%
        config(displaylogo = FALSE,
               collaborate = FALSE,
               modeBarButtonsToRemove = c(
                 'sendDataToCloud',
                 #'toImage',
                 'autoScale2d',
                 #'resetScale2d',
                 'hoverClosestCartesian', 
                 'hoverCompareCartesian')
        ) 
      
    }
    
  
    plot3 <- function() {  
      req(nrow(df()) > 0)
      ## Sekse
      count.sexc <- data.frame(row.names = c("Man", "Vrouw", "Trans"), 
                               Heteroseksueel = 1:3, Homoseksueel = 1:3, Biseksueel = 1:3, Onbekend = 1:3)
      count.sexc["Man",]["Heteroseksueel"]   <- length(df()$Seksuele.voorkeur[df()$Sekse == "Man" & df()$Seksuele.voorkeur == "Heteroseksueel"])
      count.sexc["Man",]["Homoseksueel"]     <- length(df()$Seksuele.voorkeur[df()$Sekse == "Man" & df()$Seksuele.voorkeur == "Homoseksueel"])
      count.sexc["Man",]["Biseksueel"]       <- length(df()$Seksuele.voorkeur[df()$Sekse == "Man" & df()$Seksuele.voorkeur == "Biseksueel"])
      count.sexc["Man",]["Onbekend"]         <- length(df()$Seksuele.voorkeur[df()$Sekse == "Man" & df()$Seksuele.voorkeur == "Onbekend"])
      count.sexc["Vrouw",]["Heteroseksueel"] <- length(df()$Seksuele.voorkeur[df()$Sekse == "Vrouw" & df()$Seksuele.voorkeur == "Heteroseksueel"])
      count.sexc["Vrouw",]["Homoseksueel"]   <- length(df()$Seksuele.voorkeur[df()$Sekse == "Vrouw" & df()$Seksuele.voorkeur == "Homoseksueel"])
      count.sexc["Vrouw",]["Biseksueel"]     <- length(df()$Seksuele.voorkeur[df()$Sekse == "Vrouw" & df()$Seksuele.voorkeur == "Biseksueel"])
      count.sexc["Vrouw",]["Onbekend"]       <- length(df()$Seksuele.voorkeur[df()$Sekse == "Vrouw" & df()$Seksuele.voorkeur == "Onbekend"])
      count.sexc["Trans",]["Heteroseksueel"] <- length(df()$Seksuele.voorkeur[df()$Sekse == "Transgender" & df()$Seksuele.voorkeur == "Heteroseksueel"])
      count.sexc["Trans",]["Homoseksueel"]   <- length(df()$Seksuele.voorkeur[df()$Sekse == "Transgender" & df()$Seksuele.voorkeur == "Homoseksueel"])
      count.sexc["Trans",]["Biseksueel"]     <- length(df()$Seksuele.voorkeur[df()$Sekse == "Transgender" & df()$Seksuele.voorkeur == "Biseksueel"])
      count.sexc["Trans",]["Onbekend"]       <- length(df()$Seksuele.voorkeur[df()$Sekse == "Transgender" & df()$Seksuele.voorkeur == "Onbekend"])
      
      count.sexc.vec <- unlist(rbind(c(count.sexc[1, ], count.sexc[2, ], count.sexc[3, ]))) 
      
      names.arg <- c("Hetero  ", "Homo  ", " Bi", " Onbekend")
      df.extra <- data.frame(Sekse = as.factor(rep(c("Man", "Vrouw", "Trans"), each = 4)),
                             Voorkeur = as.factor(rep(names.arg, times = 3)),
                             Frequentie = count.sexc.vec)
      
      df.extra$Sekse <- factor(df.extra$Sekse, levels = c("Trans", "Man", "Vrouw"), labels = c("Trans", "Man", "Vrouw"))
      df.extra$Voorkeur <- factor(df.extra$Voorkeur, levels = names.arg, labels = names.arg)
      df.extra <- df.extra[order(df.extra$Voorkeur), ]
      
  
      count.sexc.per <- round(100 * df.extra$Frequentie / sum(df.extra$Frequentie), 2)
      #Frequentie.new <- paste0("Aantal = ", df.extra$Frequentie)
      
      plot3 <- plot_ly(data = df.extra, y = ~Sekse, x = ~count.sexc.per, type = "bar", 
                       color = ~Voorkeur, orientation = 'h', 
                       text = paste("Sekse: ", df.extra$Sekse,
                                    "<br>Voorkeur ", df.extra$Voorkeur,
                                    "<br>Percentage: ", count.sexc.per,"%",
                                    "<br>Aantal: ", df.extra$Frequentie),
                       hoverinfo = 'text', 
                       colors = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C")) %>%
        layout(yaxis = list(title = ""), legend = list(orientation = 'h', y = -0.15),
               xaxis = list(title = ""), 
               barmode = "stack",
               title = "Seksuele voorkeur %",
               titlefont = list(
                 #family = "Agency FB",
                 size = 14)) %>%
        config(displaylogo = FALSE,
               collaborate = FALSE,
               modeBarButtonsToRemove = c(
                 'sendDataToCloud',
                 #'toImage',
                 'autoScale2d',
                 #'resetScale2d',
                 'hoverClosestCartesian',
                 'hoverCompareCartesian')
               ) 
      plot3
    }
    
    
    plot4 <- function() {  
      req(nrow(df()) > 0)
      ### Education
      count.educ <- data.frame(row.names = c("onbekend", "hbo.wo", "havo.vwo.gymnasium", "lbo.mavo.vmbo.mbo",
                                             "basisonderwijs", "anders.nl", "geen"),
                               Frequentie = 1:7)
      count.educ["onbekend", ]           <- sum(df()$Huidige.of.laatst.genoten.opleiding == "Onbekend")
      count.educ["hbo.wo", ]             <- sum(df()$Huidige.of.laatst.genoten.opleiding == "HBO/WO")
      count.educ["havo.vwo.gymnasium", ] <- sum(df()$Huidige.of.laatst.genoten.opleiding == "HAVO/VWO")
      count.educ["lbo.mavo.vmbo.mbo", ]  <- sum(df()$Huidige.of.laatst.genoten.opleiding == "LBO/MAVO/VMBO/MBO")
      count.educ["basisonderwijs", ]     <- sum(df()$Huidige.of.laatst.genoten.opleiding == "Basisonderwijs")
      count.educ["anders.nl", ]          <- sum(df()$Huidige.of.laatst.genoten.opleiding == "Anders")
      count.educ["geen", ]               <- sum(df()$Huidige.of.laatst.genoten.opleiding == "Geen")
      
      
      names.arg <- c("Onbekend", "HBO / WO", "HAVO / VWO", "LBO / MAVO VMBO / MBO",
                    "Basis", "Anders", "Geen")
      
      df.extra <- data.frame(Opleiding  = names.arg,
                             Frequentie = count.educ$Frequentie)
      df.extra$Opleiding <- factor(df.extra$Opleiding, levels = c(names.arg))
     
      
      ##df.extra %>% group_by(gedrag) %>% summarize(Sum = sum(Frequentie))
      count.educ.per <- round(100 * df.extra$Frequentie / dim(df())[1], 2) 
      #Frequentie.new <- paste0("Aantal = ", df.extra$Frequentie)
      
      plot4 <- plot_ly(data = df.extra, y = ~count.educ.per, x = ~Opleiding, type = "bar",
                       text = paste("Opleiding: ", df.extra$Opleiding,
                                    "<br>Percentage: ", count.educ.per,"%",
                                    "<br>Aantal: ", df.extra$Frequentie),
                       hoverinfo = 'text') %>% #,
                       #hoverinfo = 'text',
                       #colors=c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C"))
        layout(yaxis = list(title = ""), #legend = list(orientation = 'h', y = -0.15),
               xaxis = list(title = ""),# tickfont = list(size = 10)),
               #barmode = "stack",
               title = "Opleidingsniveau %",
               titlefont = list(
                 #family = "Agency FB",
                 size = 14)) %>%
        config(displaylogo = FALSE,
               collaborate = FALSE,
               modeBarButtonsToRemove = c(
                 'sendDataToCloud',
                 #'toImage',
                 'autoScale2d',
                 #'resetScale2d',
                 'hoverClosestCartesian',
                 'hoverCompareCartesian')
        )
    }
  
  
  
    output$plot1d <- renderPlotly({
      p1 <- plot1()
      p1
    })
    
    output$plot1c <- renderPlotly({
      p2 <- plot2()
      p2
    })
    
    output$plot1a <- renderPlotly({
      p3 <- plot3()
      p3
    })
    
    output$plot1b <- renderPlotly({
      p4 <- plot4()
      p4
    })
    
    
  ##############
  
    output$plot2 <- renderPlotly({
      req(nrow(df()) > 0)
      df3 <- df() 
  
      vn <- c("Diagnose.gonorroe", "Diagnose.chlamydia", "Diagnose.syfilis", "Diagnose.HIV", 
              "Diagnose.hepatitis.B", "Diagnose.condylomata.acuminata", "Diagnose.hepatitis.C",
              "STD.positief") 

      df3 <- df3[, c("Datum.consult", vn, "datum_consult_year", "datum_consult_month")]
  
      df3 <- group_by(df3, datum_consult_year)
      df3.extra <- summarize(df3, 
                             sum(Diagnose.gonorroe == "Positief"), 
                             sum(Diagnose.chlamydia == "Positief"),
                             sum(Diagnose.syfilis == "Positief"), 
                             sum(Diagnose.HIV == "Positief"),
                             sum(Diagnose.hepatitis.B == "Positief"), 
                             sum(Diagnose.condylomata.acuminata == "Positief"),
                             sum(Diagnose.hepatitis.C == "Positief"))
            
      colnames(df3.extra) <- c("datum_consult_year", "Diagnose.gonorroe","Diagnose.chlamydia",
                               "Diagnose.syfilis", "Diagnose.HIV", "Diagnose.hepatitis.B",
                               "Diagnose.condylomata.acuminata", "Diagnose.hepatitis.C")
      
      # plot function
      plot.diag <- function(data, soa = "Chlamydia", color = "black") {
        
        diagnosis.per <- round(100 * data$Perc, 2)
        plot5 <- plot_ly(data = data, y = ~diagnosis.per, x = ~as.character(datum_consult), 
                         type = "bar",
                         color = ~soa, text = paste("Soa: ", soa,
                                                    "<br>Jaar: ", as.character(data$datum_consult),
                                                    "<br>Percentage: ", diagnosis.per,"%",
                                                    "<br>Aantal (positief): ", data$diagnosis,
                                                    "<br>Totaal (positief + negatief): ", data$Tot),
                         hoverinfo = 'text', 
                         colors = color) %>%
          layout(yaxis = list(title = ""), 
                 legend = list(orientation = 'h', y = -0.15,
                               xanchor = "center",  # use center of legend as anchor
                               x = 0.5),
                 xaxis = list(title = "")) %>%
          config(displaylogo = FALSE,
                 collaborate = FALSE,
                 modeBarButtonsToRemove = c(
                   'sendDataToCloud',
                   #'toImage',
                   'autoScale2d',
                   #'resetScale2d',
                   'hoverClosestCartesian',
                   'hoverCompareCartesian')
          )
        
        plot5
      }  
      
      df.extra <- summarize(df3, 
                            sum(Diagnose.gonorroe %in% c("Negatief", "Positief")), 
                            sum(Diagnose.chlamydia %in% c("Negatief", "Positief")),
                            sum(Diagnose.syfilis %in% c("Negatief", "Positief")), 
                            sum(Diagnose.HIV %in% c("Negatief", "Positief")),
                            sum(Diagnose.hepatitis.B %in% c("Negatief", "Positief")), 
                            sum(Diagnose.condylomata.acuminata %in% c("Negatief", "Positief")),
                            sum(Diagnose.hepatitis.C %in% c("Negatief", "Positief")))
  
      colnames(df.extra) <- c("datum_consult_year", "Test.gonorroe","Test.chlamydia",
                              "Test.syfilis", "Test.HIV", "Test.hepatitis.B",
                              "Test.condylomata.acuminata", "Test.hepatitis.C")
      ###
      #font style
      f <- list(
        #  family = "Courier New, monospace",
        size = 12,
        color = "black")

      a <- list(text = "Chlamydia", font = f, xref = "paper", yref = "paper",
                yanchor = "bottom", xanchor = "center", align = "center",
                x = 0.5, y = 1, showarrow = FALSE)

      
      color <- brewer.pal(8, "Dark2")
      cnames <- c("datum_consult", "diagnosis", "Perc", "Tot")
      
      Perc <- df3.extra / df.extra   
      Perc[is.na(Perc)] <- 0
      
      df4 <- cbind(df3.extra[ , c(1, 3)], Perc[ , 3], df.extra[, 3])
        colnames(df4) <- cnames
      p1 <- plot.diag(df4, soa = "Chlamydia", color = color[1]) %>% layout(annotations = a) 
      df4 <- cbind(df3.extra[ , c(1, 7)], Perc[ , 7], df.extra[, 7])
        colnames(df4) <- cnames
      a$text <- "Condylomata Acuminata"
      p2 <- plot.diag(df4, soa = "Condylomata Acuminata", color = color[2]) %>% layout(annotations = a) 
      df4 <- cbind(df3.extra[ , c(1, 2)], Perc[ , 2], df.extra[, 2])
        colnames(df4) <- cnames
      a$text <- "Gonorroe"
      p3 <- plot.diag(df4, soa = "Gonorroe", color = color[3]) %>% layout(annotations = a) 
      df4 <- cbind(df3.extra[ , c(1, 6)], Perc[ , 6], df.extra[, 6])
        colnames(df4) <- cnames
      a$text <- "Hepatitis B"
      p4 <- plot.diag(df4, soa = "Hepatitis B", color = color[4]) %>% layout(annotations = a) 
      df4 <- cbind(df3.extra[ , c(1, 8)], Perc[ , 8], df.extra[, 8])
        colnames(df4) <- cnames
      a$text <- "Hepatitis C"
      p5 <- plot.diag(df4, soa = "Hepatitis C", color = color[5]) %>% layout(annotations = a) 
      df4 <- cbind(df3.extra[ , c(1, 5)], Perc[ , 5], df.extra[, 5])
        colnames(df4) <- cnames
      a$text <- "HIV"
      p6 <- plot.diag(df4, soa = "HIV", color = color[6]) %>% layout(annotations = a) 
      df4 <- cbind(df3.extra[ , c(1, 4)], Perc[ , 4], df.extra[, 4])
        colnames(df4) <- cnames
      a$text <- "Syfilis"
      p7 <- plot.diag(df4, soa = "Syfilis", color = color[7]) %>% layout(annotations = a) 
      

#      std.pos <- apply(df3[, vn], 1, function(y) fn3(y))

      std.pos <- df3$STD.positief


# end for loop ----------------------------------------------------------------      
      
      
      df.extra2 <- data.frame(df3, std.pos)
      df.extra2 <- group_by(df.extra2, datum_consult_year)
      
      df.post <- summarize(df.extra2, sum(std.pos == 1), sum(std.pos %in% c(0, 1)))
      df4 <- cbind(df3.extra[, 1], df.post[ , 2], df.post[ , 2] / df.post[ , 3], df.post[, 3])
        colnames(df4) <- cnames
      a$text <- "Totaal"
      p8 <- plot.diag(df4, soa = "Totaal", color = color[8]) %>% layout(annotations = a) 
      
  
      subplot(p1,p2,p3,p4,p5,p6,p7,p8, nrows = 2, 
              shareX = TRUE, shareY = FALSE, margin = c(0,0.02,0,0.1))
      
    })
    
############################### output tab2 ####################################

  output$trendVindpercentage <- renderPlotly({
    req(nrow(df()) > 0)
    df3 <- df() 
    
    # check if more than 2 filters are selected. This might happen if you
    # are too fast.
    input.list <- list(ggd = input$ggd_gtab2, geo = input$geolocatie_gtab2, 
                       sekse = input$sekse_gtab2, voorkeur = input$voorkeur_gtab2, 
                       opleiding = input$opleiding_gtab2, woonplaats = input$woonplaats_gtab2,
                       prep = input$prep_gtab2, risico = input$risico_gtab2)
    
    input.sum <- sum(unlist(lapply(input.list, function(x) !is.null(x))))
    
    if (input.sum == 1) {
        vn <- c("Diagnose.gonorroe", "Diagnose.chlamydia", "Diagnose.syfilis", "Diagnose.HIV", 
                "Diagnose.hepatitis.B", "Diagnose.condylomata.acuminata", "Diagnose.hepatitis.C",
                "STD.positief") 

        if (!is.null(input$ggd_gtab2)) {
          df3 <- df3 %<>% select("GGD", "Datum.consult", vn, "datum_consult_year", "datum_consult_month") %>%
            filter(GGD %in% input$ggd_gtab2) %>%
              group_by(GGD, datum_consult_year)
        
          observe({
            disable(id = "geolocatie_gtab2")
            disable(id = "sekse_gtab2")
            disable(id = "voorkeur_gtab2")
            disable(id = "opleiding_gtab2")
            disable(id = "prep_gtab2")
            disable(id = "woonplaats_gtab2")
            disable(id = "risico_gtab2")
          })
        } 
        
        if (!is.null(input$geolocatie_gtab2)) {
          df3 <- df3 %<>% select("testlocatie", "Datum.consult", vn, "datum_consult_year", "datum_consult_month") %>%
            filter(testlocatie %in% input$geolocatie_gtab2) %>%
              group_by(testlocatie, datum_consult_year)
          
          observe({
            disable(id = "ggd_gtab2")
            disable(id = "sekse_gtab2")
            disable(id = "voorkeur_gtab2")
            disable(id = "opleiding_gtab2")
            disable(id = "prep_gtab2")
            disable(id = "woonplaats_gtab2")
            disable(id = "risico_gtab2")
          })
        } 
        
        if (!is.null(input$sekse_gtab2)) {
          df3 <- df3 %<>% select("Sekse", "Datum.consult", vn, "datum_consult_year", "datum_consult_month") %>%
            filter(Sekse %in% input$sekse_gtab2) %>%
              group_by(Sekse, datum_consult_year)
          
          observe({
            disable(id = "ggd_gtab2")
            disable(id = "geolocatie_gtab2")
            disable(id = "voorkeur_gtab2")
            disable(id = "opleiding_gtab2")
            disable(id = "prep_gtab2")
            disable(id = "woonplaats_gtab2")
            disable(id = "risico_gtab2")
          })
        } 
        
        if (!is.null(input$voorkeur_gtab2)) {
          df3 <- df3 %<>% select("Seksuele.voorkeur", "Datum.consult", vn, "datum_consult_year", "datum_consult_month") %>%
            filter(Seksuele.voorkeur %in% input$voorkeur_gtab2) %>%
              group_by(Seksuele.voorkeur, datum_consult_year)
          
          observe({
            disable(id = "ggd_gtab2")
            disable(id = "geolocatie_gtab2")
            disable(id = "sekse_gtab2")
            disable(id = "opleiding_gtab2")
            disable(id = "prep_gtab2")
            disable(id = "woonplaats_gtab2")
            disable(id = "risico_gtab2")
          })
        } 
        
        if (!is.null(input$opleiding_gtab2)) {
          df3 <- df3 %<>% select("Huidige.of.laatst.genoten.opleiding", "Datum.consult", vn, "datum_consult_year", "datum_consult_month") %>%
            filter(Huidige.of.laatst.genoten.opleiding %in% input$opleiding_gtab2) %>%
              group_by(Huidige.of.laatst.genoten.opleiding, datum_consult_year)

          observe({
            disable(id = "ggd_gtab2")
            disable(id = "geolocatie_gtab2")
            disable(id = "sekse_gtab2")
            disable(id = "voorkeur_gtab2")
            disable(id = "prep_gtab2")
            disable(id = "woonplaats_gtab2")
            disable(id = "risico_gtab2")
          })
        } 

        if (!is.null(input$prep_gtab2)) {
          df3 <- df3 %<>% select("ooit.prep.gebruikt", "Datum.consult", vn, "datum_consult_year", "datum_consult_month") %>%
            filter(ooit.prep.gebruikt %in% input$prep_gtab2) %>%
            group_by(ooit.prep.gebruikt, datum_consult_year)
          
          observe({
            disable(id = "ggd_gtab2")
            disable(id = "geolocatie_gtab2")
            disable(id = "sekse_gtab2")
            disable(id = "voorkeur_gtab2")
            disable(id = "opleiding_gtab2")
            disable(id = "woonplaats_gtab2")
            disable(id = "risico_gtab2")
          })
        } 
        
        if (!is.null(input$woonplaats_gtab2)) {
          df3 <- df3 %<>% select("woonplaats", "Datum.consult", vn, "datum_consult_year", "datum_consult_month") %>%
            filter(woonplaats %in% input$woonplaats_gtab2) %>%
            group_by(woonplaats, datum_consult_year)
          
          observe({
            disable(id = "ggd_gtab2")
            disable(id = "geolocatie_gtab2")
            disable(id = "sekse_gtab2")
            disable(id = "voorkeur_gtab2")
            disable(id = "opleiding_gtab2")
            disable(id = "prep_gtab2")            
            disable(id = "risico_gtab2")
          })
        } 

        if (!is.null(input$risico_gtab2)) {
          disable(id = "risico_tab2")
          observe({
            disable(id = "ggd_gtab2")
            disable(id = "geolocatie_gtab2")
            disable(id = "sekse_gtab2")
            disable(id = "voorkeur_gtab2")
            disable(id = "opleiding_gtab2")
            disable(id = "prep_gtab2")            
            disable(id = "woonplaats_gtab2")
          })
          
          
          risico.df <- c()
          if ("MSM" %in% input$risico_gtab2) {
            msm.df <- df3 %>% select("MSM", "Datum.consult", vn, 
                                     "datum_consult_year", "datum_consult_month") %>%
              filter(MSM == "Ja") 
            names(msm.df)[1] <- "Group"
            msm.df[, 1] <- "MSM"
            risico.df <- rbind(risico.df, msm.df)
          }
          if ("Gewaarschuwd" %in% input$risico_gtab2) {
            gewaarschuwd.df <- df3 %>% select("Gewaarschuwd.door.partner", "Datum.consult", vn, 
                                     "datum_consult_year", "datum_consult_month") %>%
              filter(Gewaarschuwd.door.partner == "Ja") 
            names(gewaarschuwd.df)[1] <- "Group"
            gewaarschuwd.df[, 1] <- "Gewaarschuwd"
            risico.df <- rbind(risico.df, gewaarschuwd.df)
          }
          if ("Klachten" %in% input$risico_gtab2) {
            klachten.df <- df3 %>% select("Soa.gerelateerde.klachten", "Datum.consult", vn, 
                                              "datum_consult_year", "datum_consult_month") %>%
              filter(Soa.gerelateerde.klachten == "Ja") 
            names(klachten.df)[1] <- "Group"
            klachten.df[, 1] <- "Klachten"
            risico.df <- rbind(risico.df, klachten.df)
          }
          if ("<25 jaar" %in% input$risico_gtab2) {
            df3$Jonger.dan.25 <- df3$datum_consult_year - df3$Geboortejaar_mod
            jaar.df <- df3 %>% select("Jonger.dan.25", "Datum.consult", vn, 
                                      "datum_consult_year", "datum_consult_month") %>%
              filter(Jonger.dan.25 < 25) 
            names(jaar.df)[1] <- "Group"
            jaar.df[, 1] <- "25< jaar"
            risico.df <- rbind(risico.df, jaar.df)
          }
          if ("Afkomstig uit soa endemischgebied" %in% input$risico_gtab2) {
            endemisch.df <- df3 %>% select("Geboorteland.patient.endemisch", "Datum.consult", vn, 
                                          "datum_consult_year", "datum_consult_month") %>%
              filter(Geboorteland.patient.endemisch == "Ja") 
            names(endemisch.df)[1] <- "Group"
            endemisch.df[, 1] <- "Endemisch"
            risico.df <- rbind(risico.df, endemisch.df)
          }
          if ("Partner risicogroep" %in% input$risico_gtab2) {
            partner.df <- df3 %>% select("Partner.uit.doelgroep", "Datum.consult", vn, 
                                           "datum_consult_year", "datum_consult_month") %>%
              filter(Partner.uit.doelgroep == "Ja") 
            names(partner.df)[1] <- "Group"
            partner.df[, 1] <- "Partner"
            risico.df <- rbind(risico.df, partner.df)
          }
          if ("Prostituee" %in% input$risico_gtab2) {
            prostituee.df <- df3 %>% select("Prostituee", "Datum.consult", vn, 
                                         "datum_consult_year", "datum_consult_month") %>%
              filter(Prostituee == "Ja") 
            names(prostituee.df)[1] <- "Group"
            prostituee.df[, 1] <- "Prostituee"
            risico.df <- rbind(risico.df, prostituee.df)
          }
          if ("Afgelopen jaar GO, CT, SYF gehad" %in% input$risico_gtab2) {
            afgelopen.df <- df3 %>% select("In.de.afgelopen.jaar.GO_CT_SYF.gehad", "Datum.consult", vn, 
                                            "datum_consult_year", "datum_consult_month") %>%
              filter(In.de.afgelopen.jaar.GO_CT_SYF.gehad == "Ja") 
            names(afgelopen.df)[1] <- "Group"
            afgelopen.df[, 1] <- "Afgelopen"
            risico.df <- rbind(risico.df, afgelopen.df)
          }
          if ("Afgelopen jaar een positieve hiv test gehad" %in% input$risico_gtab2) {
            hiv.df <- df3 %>% select("Eerder.HIV.test.met.uitslag", "Datum.consult", vn, 
                                           "datum_consult_year", "datum_consult_month") %>%
              filter(Eerder.HIV.test.met.uitslag == "jaPositief") 
            names(hiv.df)[1] <- "Group"
            hiv.df[, 1] <- "HIV"
            risico.df <- rbind(risico.df, hiv.df)
          }
          
          df3 <- risico.df %>% group_by(Group, datum_consult_year)
        } 
        
        output$value2_tab2 <- renderInfoBox({
          infoBox(
            paste0('Aantal subset = ', dim(df3)[1]),
            icon = icon("users"),
            color = "purple")  
        })
        
          
      df3.extra <- summarize(df3, 
                             sum(Diagnose.gonorroe == "Positief"), 
                             sum(Diagnose.chlamydia == "Positief"),
                             sum(Diagnose.syfilis == "Positief"), 
                             sum(Diagnose.HIV == "Positief"),
                             sum(Diagnose.hepatitis.B == "Positief"), 
                             sum(Diagnose.condylomata.acuminata == "Positief"),
                             sum(Diagnose.hepatitis.C == "Positief"))
      
      colnames(df3.extra) <- colnames(df3)[1:9]
      
      
      # plot function
      plot.diag <- function(data, soa = "Chlamydia", color = "black") {
        
        if (nrow(data) == 0) {
          return(plotly_empty())
        }
        
        diagnosis.per <- round(100 * data$Perc, 2)
        #diagnosis.text <- paste0("Aantal = ", data$diagnosis)
        
        vn.legend <- data[ , 1]
        vn.legend[vn.legend == "GGD Brabant Zuid-Oost"] <- "BZO"
        vn.legend[vn.legend == "GGD Hart voor Brabant"] <- "HvB"
        vn.legend[vn.legend == "GGD West Brabant"] <- "WB"
      
      
        plot5 <- plot_ly(data = data, y = ~diagnosis.per, x = ~datum_consult, 
                       type = "scatter", linetype = ~data[, 1], mode = "lines+markers",
                       symbol = ~data[, 1], marker = list(size = 9),
                       color = soa, text = paste(data[, 1],
                                                  "<br>Jaar: ", data$datum_consult,
                                                  "<br>Soa: ", soa,
                                                  "<br>Percentage: ", diagnosis.per, "%",
                                                  "<br>Aantal (positief): ", data$diagnosis,
                                                  "<br>Totaal (positief + negatief): ", data$Tot),
                       hoverinfo = 'text',
                       colors = color,
                       name = vn.legend, legendgroup = soa) %>% group_by(data[, 1]) %>%  
        
        layout(yaxis = list(title = "", rangemode = "tozero"), 
               legend = list(orientation = 'h', y = -0.15,
                             xanchor = "center", x = 0.5), 
               xaxis = list(title = "")) %>%
            
        config(displaylogo = FALSE,
               collaborate = FALSE,
               modeBarButtonsToRemove = c(
                 'sendDataToCloud',
                 'autoScale2d',
                 'hoverClosestCartesian',
                 'hoverCompareCartesian')
        )
      
      plot5
    }  
      
      
      df.extra <- summarize(df3, 
                            sum(Diagnose.gonorroe %in% c("Negatief", "Positief")), 
                            sum(Diagnose.chlamydia %in% c("Negatief", "Positief")),
                            sum(Diagnose.syfilis %in% c("Negatief", "Positief")), 
                            sum(Diagnose.HIV %in% c("Negatief", "Positief")),
                            sum(Diagnose.hepatitis.B %in% c("Negatief", "Positief")), 
                            sum(Diagnose.condylomata.acuminata %in% c("Negatief", "Positief")),
                            sum(Diagnose.hepatitis.C %in% c("Negatief", "Positief")))
      

      colnames(df.extra) <- colnames(df3)[1:9]
      ###
      # font style
      f <- list(
      #  family = "Courier New, monospace",
        size = 12,
        color = "black")
      
      a <- list(text = "Chlamydia", font = f, xref = "paper", yref = "paper",
                yanchor = "bottom", xanchor = "center", align = "center",
                x = 0.5, y = 1, showarrow = FALSE)
      
      color <- brewer.pal(8, "Dark2")
      
      df3.extra$Datum.consult <- as.character(df3.extra$Datum.consult)  
      vn.df4 <- c(colnames(df3.extra)[1], "datum_consult", "diagnosis", "Perc", "Tot")
    
      Perc <- df3.extra[ , 3:9] / df.extra[ , 3:9] 
      Perc[is.na(Perc)] <- 0
      
      df4 <- data.frame(df3.extra[ , c(1, 2, 4)], Perc[ , 2, drop = FALSE], df.extra[, 4])
        colnames(df4) <- vn.df4
      p1 <- plot.diag(df4, soa = "Chlamydia", color = color[1]) %>% layout(annotations = a) 
      df4 <- data.frame(df3.extra[ , c(1, 2, 8)], Perc[ , 6], df.extra[, 8])
        colnames(df4) <- vn.df4
      a$text <- "Condylomata Acuminata"
      p2 <- plot.diag(df4, soa = "Condylomata Acuminata", color = color[2]) %>% layout(annotations = a)
      df4 <- data.frame(df3.extra[ , c(1, 2, 3)], Perc[ , 1], df.extra[, 3])
        colnames(df4) <- vn.df4
      a$text <- "Gonorroe"
      p3 <- plot.diag(df4, soa = "Gonorroe", color = color[3]) %>% layout(annotations = a)
      df4 <- data.frame(df3.extra[ , c(1, 2, 7)], Perc[ , 5], df.extra[, 7])
        colnames(df4) <- vn.df4
      a$text <- "Hepatitis B"
      p4 <- plot.diag(df4, soa = "Hepatitis B", color = color[4]) %>% layout(annotations = a)
      df4 <- data.frame(df3.extra[ , c(1, 2, 9)], Perc[ , 7], df.extra[, 9])
        colnames(df4) <- vn.df4
      a$text <- "Hepatitis C"
      p5 <- plot.diag(df4, soa = "Hepatitis C", color = color[5]) %>% layout(annotations = a)
      df4 <- data.frame(df3.extra[ , c(1, 2, 6)], Perc[ , 4], df.extra[, 6])  
        colnames(df4) <- vn.df4
      a$text <- "HIV"
      p6 <- plot.diag(df4, soa = "HIV", color = color[6]) %>% layout(annotations = a)
      df4 <- data.frame(df3.extra[ , c(1, 2, 5)], Perc[ , 3], df.extra[, 5])
        colnames(df4) <- vn.df4
      a$text <- "Syfilis"
      p7 <- plot.diag(df4, soa = "Syfilis", color = color[7]) %>% layout(annotations = a)
  

#      std.pos <- apply(df3[, vn], 1, function(y) fn3(y))
      std.pos <- df3$STD.positief
      df.extra2 <- data.frame(df3, std.pos)
      
      df.extra2 <- 
        df.extra2 %>% 
        group_by(df.extra2[, 1], datum_consult_year)
      
      df.post <- summarize(df.extra2, 
                           sum(std.pos == 1), 
                           sum(std.pos %in% c(0, 1)))
      
      df4 <- data.frame(df3.extra[, 1:2], df.post[ , 3], df.post[ , 3] / df.post[ , 4], df.post[, 4])
        colnames(df4) <- vn.df4
      a$text <- "Totaal"
      p8 <- plot.diag(df4, soa = "Totaal", color = color[8]) %>% layout(annotations = a)
      
      
      subplot(p1,p2,p3,p4,p5,p6,p7,p8, nrows = 2, 
              shareX = TRUE, shareY = FALSE, margin = c(0,0.02,0,0.1))
      
    } else {
      output$value2_tab2 <- renderInfoBox({
        infoBox(
          paste0('Aantal subset = ', dim(df3)[1]),
          icon = icon("users"),
          color = "purple")  
      })
      
      enable(id = "ggd_gtab2")
      enable(id = "geolocatie_gtab2")
      enable(id = "sekse_gtab2")
      enable(id = "voorkeur_gtab2")
      enable(id = "opleiding_gtab2")
      enable(id = "woonplaats_gtab2")
      enable(id = "prep_gtab2")  
      enable(id = "risico_gtab2")
      
      
      return(NULL)
    }
  
    
  })
    
    ############################### output tab3 ####################################
  
    output$trendRisicogroepen <- renderPlotly({
      req(nrow(df()) > 0)
      
      df3 <- df() 
      
      # check if more than 2 filters are selected. This might happen if you
      # are too fast.
      input.list <- list(ggd = input$ggd_gtab3, geo = input$geolocatie_gtab3, 
                         sekse = input$sekse_gtab3, voorkeur = input$voorkeur_gtab3, 
                         opleiding = input$opleiding_gtab3, woonplaats = input$woonplaats_gtab3,
                         prep = input$prep_gtab3, soa = input$soa_gtab3)

      input.idx <- unlist(lapply(input.list, function(x) !is.null(x)))
      input.sum <- sum(input.idx)
      
      if (input.sum == 1) {
        vn <- c("MSM", 
                "Gewaarschuwd.door.partner",
                "Soa.gerelateerde.klachten",
                "Geboorteland.patient.endemisch",
                "Partner.uit.doelgroep",
                "Prostituee",
                "In.de.afgelopen.jaar.GO_CT_SYF.gehad",
                "Eerder.HIV.test.met.uitslag")
        vn <- c(vn, "Jonger.dan.25")
        df3$Jonger.dan.25 <- df3$datum_consult_year - df3$Geboortejaar_mod
      
        df.extra <- df3
        
        if (!is.null(input$ggd_gtab3)) {
          observe({
            disable(id = "geolocatie_gtab3")
            disable(id = "sekse_gtab3")
            disable(id = "voorkeur_gtab3")
            disable(id = "opleiding_gtab3")
            disable(id = "prep_gtab3")
            disable(id = "woonplaats_gtab3")
            disable(id = "soa_gtab3")
          })
          
          input.vn <- "GGD"
          df3 %<>% select("GGD", "Datum.consult", vn, "datum_consult_year", "datum_consult_month") %>%
            filter(GGD %in% input$ggd_gtab3) %>%
            group_by(GGD, datum_consult_year)
        } 
        
        if (!is.null(input$geolocatie_gtab3)) {
          observe({
            disable(id = "ggd_gtab3")
            disable(id = "sekse_gtab3")
            disable(id = "voorkeur_gtab3")
            disable(id = "opleiding_gtab3")
            disable(id = "prep_gtab3")
            disable(id = "woonplaats_gtab3")
            disable(id = "soa_gtab3")
          })
          
          input.vn <- "testlocatie"
          df3 %<>% select("testlocatie", "Datum.consult", vn, "datum_consult_year", "datum_consult_month") %>%
            filter(testlocatie %in% input$geolocatie_gtab3) %>%
            group_by(testlocatie, datum_consult_year)
        } 
        
        if (!is.null(input$sekse_gtab3)) {
          observe({
            disable(id = "ggd_gtab3")
            disable(id = "geolocatie_gtab3")
            disable(id = "voorkeur_gtab3")
            disable(id = "opleiding_gtab3")
            disable(id = "prep_gtab3")
            disable(id = "woonplaats_gtab3")
            disable(id = "soa_gtab3")
          })
          
          input.vn <- "Sekse"
          df3 %<>% select("Sekse", "Datum.consult", vn, "datum_consult_year", "datum_consult_month") %>%
            filter(Sekse %in% input$sekse_gtab3) %>%
            group_by(Sekse, datum_consult_year)
        } 
        
        if (!is.null(input$voorkeur_gtab3)) {
          observe({
            disable(id = "ggd_gtab3")
            disable(id = "geolocatie_gtab3")
            disable(id = "sekse_gtab3")
            disable(id = "opleiding_gtab3")
            disable(id = "prep_gtab3")
            disable(id = "woonplaats_gtab3")
            disable(id = "soa_gtab3")
          })
          
          input.vn <- "Seksuele.voorkeur"
          df3 %<>% select("Seksuele.voorkeur", "Datum.consult", vn, "datum_consult_year", "datum_consult_month") %>%
            filter(Seksuele.voorkeur %in% input$voorkeur_gtab3) %>%
            group_by(Seksuele.voorkeur, datum_consult_year)
        } 
        
        if (!is.null(input$opleiding_gtab3)) {
          observe({
            disable(id = "ggd_gtab3")
            disable(id = "geolocatie_gtab3")
            disable(id = "sekse_gtab3")
            disable(id = "voorkeur_gtab3")
            disable(id = "prep_gtab3")
            disable(id = "woonplaats_gtab3")
            disable(id = "soa_gtab3")
          })
          
          input.vn <- "Huidige.of.laatst.genoten.opleiding"
          df3 %<>% select("Huidige.of.laatst.genoten.opleiding", "Datum.consult", vn, "datum_consult_year", "datum_consult_month") %>%
            filter(Huidige.of.laatst.genoten.opleiding %in% input$opleiding_gtab3) %>%
            group_by(Huidige.of.laatst.genoten.opleiding, datum_consult_year)
        } 
        
        if (!is.null(input$prep_gtab3)) {
          observe({
            disable(id = "ggd_gtab3")
            disable(id = "geolocatie_gtab3")
            disable(id = "sekse_gtab3")
            disable(id = "voorkeur_gtab3")
            disable(id = "opleiding_gtab3")
            disable(id = "woonplaats_gtab3")
            disable(id = "soa_gtab3")
          })
          
          input.vn <- "ooit.prep.gebruikt"
          df3 %<>% select("ooit.prep.gebruikt", "Datum.consult", vn, "datum_consult_year", "datum_consult_month") %>%
            filter(ooit.prep.gebruikt %in% input$prep_gtab3) %>%
            group_by(ooit.prep.gebruikt, datum_consult_year)
        } 
        
        if (!is.null(input$woonplaats_gtab3)) {
          observe({
            disable(id = "ggd_gtab3")
            disable(id = "geolocatie_gtab3")
            disable(id = "sekse_gtab3")
            disable(id = "voorkeur_gtab3")
            disable(id = "opleiding_gtab3")
            disable(id = "prep_gtab3")            
            disable(id = "soa_gtab3")
          })
          
          input.vn <- "woonplaats"
          df3 %<>% select("woonplaats", "Datum.consult", vn, "datum_consult_year", "datum_consult_month") %>%
            filter(woonplaats %in% input$woonplaats_gtab3) %>%
            group_by(woonplaats, datum_consult_year)
        } 
        
        
        # if (!is.null(input$soa_gtab3)) {
        #   disable(id = "soa_tab3")
        #   observe({
        #     disable(id = "ggd_gtab3")
        #     disable(id = "geolocatie_gtab3")
        #     disable(id = "sekse_gtab3")
        #     disable(id = "voorkeur_gtab3")
        #     disable(id = "opleiding_gtab3")
        #     disable(id = "prep_gtab3")
        #     disable(id = "woonplaats_gtab3")
        #   })
        #   
        #   soa.df <- c()
        #   if ("Gonorroe" %in% input$soa_gtab3) {
        #     gonorroe.df <- df3 %>% select("Diagnose.gonorroe", "Datum.consult", vn, 
        #                                    "datum_consult_year", "datum_consult_month") %>%
        #     filter(Diagnose.gonorroe == "Positief") 
        #     names(gonorroe.df)[1] <- "Diagnose"
        #     gonorroe.df[, 1] <- "Gonorroe"
        #     soa.df <- rbind(soa.df, gonorroe.df)
        #   }
        #   if ("Chlamydia" %in% input$soa_gtab3) {
        #     chlamydia.df <- df3 %>% select("Diagnose.chlamydia", "Datum.consult", vn, 
        #                                    "datum_consult_year", "datum_consult_month") %>%
        #     filter(Diagnose.chlamydia == "Positief") 
        #     names(chlamydia.df)[1] <- "Diagnose"
        #     chlamydia.df[, 1] <- "Chlymadia"
        #     soa.df <- rbind(soa.df, chlamydia.df)
        #   }
        #   if ("LGV" %in% input$soa_gtab3) {
        #     disable(id = "sekse_tab3")
        #     lgv.df <- df3 %>% select("Uitslag.serovartypering", "Datum.consult", vn, 
        #                              "datum_consult_year", "datum_consult_month") %>%
        #     filter(Uitslag.serovartypering == "Positief")
        #     names(lgv.df)[1] <- "Diagnose"
        #     lgv.df[, 1] <- "LGV"
        #     soa.df <- rbind(soa.df, lgv.df)
        #   }
        #   if ("Syfilis" %in% input$soa_gtab3) {
        #     syfilis.df <- df3 %>% select("Diagnose.syfilis", "Datum.consult", vn, 
        #                                  "datum_consult_year", "datum_consult_month") %>%
        #     filter(Diagnose.syfilis == "Positief")
        #     names(syfilis.df)[1] <- "Diagnose"
        #     syfilis.df[, 1] <- "Syfilis" 
        #     soa.df <- rbind(soa.df, syfilis.df)
        #   }
        #   if ("HIV" %in% input$soa_gtab3) {
        #     hiv.df <- df3 %>% select("Diagnose.HIV", "Datum.consult", vn, 
        #                              "datum_consult_year", "datum_consult_month") %>%
        #     filter(Diagnose.HIV == "Positief")
        #     names(hiv.df)[1] <- "Diagnose"
        #     hiv.df[, 1] <- "HIV"
        #     soa.df <- rbind(soa.df, hiv.df)
        #   }
        #   if ("Hepatitis B" %in% input$soa_gtab3) {
        #     hepB.df <- df3 %>% select("Diagnose.hepatitis.B", "Datum.consult", vn, 
        #                               "datum_consult_year", "datum_consult_month") %>%
        #     filter(Diagnose.hepatitis.B == "Positief")
        #     names(hepB.df)[1] <- "Diagnose"
        #     hepB.df[, 1] <- "Hepatitis.B"
        #     soa.df <- rbind(soa.df, hepB.df)
        #   }
        #   if ("Condylomata acuminata" %in% input$soa_gtab3) {
        #     condy.df <- df3 %>% select("Diagnose.condylomata.acuminata", "Datum.consult", vn, 
        #                               "datum_consult_year", "datum_consult_month") %>%
        #     filter(Diagnose.condylomata.acuminata == "Positief")
        #     names(condy.df)[1] <- "Diagnose"
        #     condy.df[, 1] <- "Condylomata acuminata"
        #     soa.df <- rbind(soa.df, condy.df)
        #   }
        #   if ("Hepatitis C" %in% input$soa_gtab3) {
        #     hepC.df <- df3 %>% select("Diagnose.hepatitis.C", "Datum.consult", vn, 
        #                               "datum_consult_year", "datum_consult_month") %>%
        #     filter(Diagnose.hepatitis.C == "Positief")
        #     names(hepC.df)[1] <- "Diagnose"
        #     hepC.df[, 1] <- "Hepatitis.C"
        #     soa.df <- rbind(soa.df, hepC.df)
        #   }
        #   df3 <- soa.df %>% group_by(Diagnose, datum_consult_year)
        # }
        
        
        
        output$value2_tab3 <- renderInfoBox({
          infoBox(
            paste0('Aantal subset = ', dim(df3)[1]),
            icon = icon("users"),
            color = "purple")  
        })
        
        
        df3.extra <- summarize(df3, 
                               sum(MSM == "Ja", na.rm = TRUE), 
                               sum(Gewaarschuwd.door.partner == "Ja", na.rm = TRUE),
                               sum(Soa.gerelateerde.klachten == "Ja", na.rm = TRUE), 
                               sum(Geboorteland.patient.endemisch == "Ja", na.rm = TRUE),
                               sum(Partner.uit.doelgroep == "Ja", na.rm = TRUE), 
                               sum(Prostituee == "Ja", na.rm = TRUE),
                               sum(In.de.afgelopen.jaar.GO_CT_SYF.gehad == "Ja", na.rm = TRUE),
                               sum(Eerder.HIV.test.met.uitslag == "jaPositief", na.rm = TRUE),
                               sum(Jonger.dan.25 < 25, na.rm = TRUE))
        
        colnames(df3.extra) <- colnames(df3)[1:11]
        
        df.extra %<>%
          select(input.vn, "datum_consult_year", vn) %>%
          group_by_at(c(1,2)) %>%
          summarize(sum(MSM == "Ja", na.rm = TRUE), 
                    sum(Gewaarschuwd.door.partner == "Ja", na.rm = TRUE),
                    sum(Soa.gerelateerde.klachten == "Ja", na.rm = TRUE), 
                    sum(Geboorteland.patient.endemisch == "Ja", na.rm = TRUE),
                    sum(Partner.uit.doelgroep == "Ja", na.rm = TRUE), 
                    sum(Prostituee == "Ja", na.rm = TRUE),
                    sum(In.de.afgelopen.jaar.GO_CT_SYF.gehad == "Ja", na.rm = TRUE),
                    sum(Eerder.HIV.test.met.uitslag == "jaPositief", na.rm = TRUE),
                    sum(Jonger.dan.25 < 25, na.rm = TRUE)) %>%
          set_colnames(c(input.vn, "Datum.consult", vn)) %>%
          ungroup() %>%
          group_by(Datum.consult) %>%
          summarise_at(vars(-c(1, 2)), funs(sum))
          
        # df.extra %<>% select(input.vn, "Datum.consult", vn, "datum_consult_year", "datum_consult_month") %>%
        #   group_by_at(c(1, 12)) %>%
        #   summarize(sum(MSM %in% c("Nee", "Ja")), 
        #             sum(Gewaarschuwd.door.partner %in% c("Nee", "Ja")),
        #             sum(Soa.gerelateerde.klachten %in% c("Nee", "Ja")),
        #             sum(Geboorteland.patient.endemisch %in% c("Nee", "Ja")),
        #             sum(Partner.uit.doelgroep %in% c("Nee", "Ja")), 
        #             sum(Prostituee %in% c("Nee", "Ja")),
        #             sum(In.de.afgelopen.jaar.GO_CT_SYF.gehad %in% c("Nee", "Ja")),
        #             sum(Eerder.HIV.test.met.uitslag %in% c("jaNegatief", "jaPositief")),
        #             length(Jonger.dan.25)) %>%
        #   set_colnames(c(input.vn, "Datum.consult", vn)) %>%
        #   ungroup() %>%
        #   select(Datum.consult, vn) %>%
        #   group_by(Datum.consult) %>%
        #   summarize_all(funs(sum))
        
        
        
        # plot function
        plot.risico <- function(data, risico = "MSM", color = "black") {
          
          if (nrow(data) == 0) {
            return(plotly_empty())
          }
          
          behavior.per <- round(100 * data$Perc, 2)
          
          vn.legend <- data[ ,1]
          vn.legend[vn.legend == "GGD Brabant Zuid-Oost "] <- "BZO"
          vn.legend[vn.legend == "GGD Hart voor Brabant"] <- "HvB"
          vn.legend[vn.legend == "GGD West Brabant"] <- "WB"
          
          
          plot6 <- plot_ly(data = data, y = ~behavior.per, x = ~Datum_consult, 
                           type = "scatter", linetype = ~data[, 1], mode = "lines+markers", 
                           symbol = ~data[, 1], marker = list(size = 9),
                           color = risico, text = paste(data[, 1],
                                                      "<br>Jaar: ", data$Datum_consult,
                                                      "<br>Risicogroep: ", risico,
                                                      "<br>Percentage: ", behavior.per, "%",
                                                      "<br>Aantal: ", data$risico),#,
                                                      #"<br>Totaal (ja + nee): ", data$Tot),
                           hoverinfo = 'text',
                           colors = color,
                           name = vn.legend, legendgroup = risico) %>% group_by(data[, 1]) %>%  
            
            layout(yaxis = list(title = "", rangemode = "tozero"), 
                   legend = list(orientation = 'h', y = -0.15,
                                 xanchor = "center", x = 0.5), 
                   xaxis = list(title = "")) %>%
            
            config(displaylogo = FALSE,
                   collaborate = FALSE,
                   modeBarButtonsToRemove = c(
                     'sendDataToCloud',
                     'autoScale2d',
                     'hoverClosestCartesian',
                     'hoverCompareCartesian')
            )
          
          plot6
        }  
        

        ###
        # font style
        f <- list(
          #  family = "Courier New, monospace",
          size = 12,
          color = "black")
        
        a <- list(text = "MSM", font = f, xref = "paper", yref = "paper",
                  yanchor = "bottom", xanchor = "center", align = "center",
                  x = 0.5, y = 1, showarrow = FALSE)
        
        color <- brewer.pal(12, "Paired")
        
        df3.extra$Datum.consult <- as.character(df3.extra$Datum.consult)  
        vn.df4 <- c(colnames(df3.extra)[1], "Datum_consult", "risico", "Perc")#, "Tot")
        
        
        # not all variables are registered from the start in 2012, 
        # e.g., opleidingsniveau LBO..., exists from 2014 onwards.
        df.match.idx <- match(df3.extra$Datum.consult, df.extra$Datum.consult)
        len <- table(df3.extra[1])
        Perc <- list()
        for (l in 1:length(len)) {
          
          if (l > 1) {
            df3.row.idx <- c(cumsum(len)[l - 1] + 1):cumsum(len)[l]
          } else {
            df3.row.idx <- 1:cumsum(len)[1]
          }
          
          df.row.idx <- df.match.idx[df3.row.idx]  #[1:len[l]]
          
          Perc[[l]] <- df3.extra[df3.row.idx, 3:11] / df.extra[df.row.idx, 2:10]
        }
        
          
        Perc <- do.call(rbind, Perc) 
        Perc[is.na(Perc)] <- 0
        
        df4 <- data.frame(df3.extra[ , c(1, 2, 3)], Perc[ , 1, drop = FALSE])#, df.extra[len.i, 2])
        colnames(df4) <- vn.df4
        p1 <- plot.risico(df4, risico = "MSM", color = color[4]) %>% layout(annotations = a) 
        
        df4 <- data.frame(df3.extra[ , c(1, 2, 4)], Perc[ , 2])#, df.extra[len.i, 3])
        colnames(df4) <- vn.df4
        a$text <- "Gewaarschuwd door partner"
        p2 <- plot.risico(df4, risico = "Gewaarschuwd", color = color[5]) %>% layout(annotations = a)
        
        df4 <- data.frame(df3.extra[ , c(1, 2, 5)], Perc[ , 3])#, df.extra[len.i, 4])
        colnames(df4) <- vn.df4
        a$text <- "Soa gerelateerde klachten"
        p3 <- plot.risico(df4, risico = "Klachten", color = color[6]) %>% layout(annotations = a)
        
        df4 <- data.frame(df3.extra[ , c(1, 2, 11)], Perc[ , 9])#, df.extra[len.i, 10])
        colnames(df4) <- vn.df4
        a$text <- "Jonger dan 25 jaar"
        p4 <- plot.risico(df4, risico = "< 25 jaar", color = color[7]) %>% layout(annotations = a)
        
        df4 <- data.frame(df3.extra[ , c(1, 2, 6)], Perc[ , 4])#, df.extra[len.i, 5])
        colnames(df4) <- vn.df4
        a$text <- "Client uit Soa endemisch gebied"
        p5 <- plot.risico(df4, risico = "Soa endemisch gebied", color = color[8]) %>% layout(annotations = a)
        
        df4 <- data.frame(df3.extra[ , c(1, 2, 7)], Perc[ , 5])#, df.extra[len.i, 6])  
        colnames(df4) <- vn.df4
        a$text <- "Partner uit Soa endemisch gebied"
        p6 <- plot.risico(df4, risico = "Partner risicogroep", color = color[9]) %>% layout(annotations = a)
        
        df4 <- data.frame(df3.extra[ , c(1, 2, 8)], Perc[ , 6])#, df.extra[len.i, 7])
        colnames(df4) <- vn.df4
        a$text <- "Prostituee"
        p7 <- plot.risico(df4, risico = "Prostituee", color = color[10]) %>% layout(annotations = a)
        
        df4 <- data.frame(df3.extra[ , c(1, 2, 9)], Perc[ , 7])#, df.extra[Datum.consult.idx, 8])
        colnames(df4) <- vn.df4
        a$text <- "Het afgelopen een SOA gehad"
        p8 <- plot.risico(df4, risico = "Afgelopen jaar SOA", color = color[2]) %>% layout(annotations = a)
        
        df4 <- data.frame(df3.extra[ , c(1, 2, 10)], Perc[ , 8])#, df.extra[len.i, 9])
        colnames(df4) <- vn.df4
        a$text <- "Bekende HIV+"
        p9 <- plot.risico(df4, risico = "HIV+", color = color[12]) %>% layout(annotations = a)
        
        subplot(p1,p2,p3,p4,p5,p6,p7,p8,p9, nrows = 3, 
                shareX = TRUE, shareY = FALSE, margin = c(0,0.02,0,0.1))
        
      } else {
        output$value2_tab3 <- renderInfoBox({
          infoBox(
            paste0('Aantal subset = ', dim(df3)[1]),
            icon = icon("users"),
            color = "purple")  
        })
        
        enable(id = "ggd_gtab3")
        enable(id = "geolocatie_gtab3")
        enable(id = "sekse_gtab3")
        enable(id = "voorkeur_gtab3")
        enable(id = "opleiding_gtab3")
        enable(id = "woonplaats_gtab3")
        enable(id = "prep_gtab3")      
        enable(id = "soa_gtab3")      
        enable(id = "soa_tab3")
        return(NULL)
      }
      
      
    })
}