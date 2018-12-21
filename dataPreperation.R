library(data.table)
# Set working directory
setwd("C:/Users/l.vanbrabant/OneDrive - Het Servicecentrum/SOAtool")

################################################################################
# Data column rename
library(openxlsx)
#setwd(path.codebook)
cb <- loadWorkbook("./codebook/Codeboek.xlsx")
name <- readWorkbook(cb, sheet = "Sheet2")
#vn <- c("MELDINGSNR", "OSIRISNR", "EIGENAAR", toupper(name$vraa_cd))
vn <- c("MELDINGSNR", "OSIRISNR", "EIGENAAR", toupper(name$vraa_cd))


#setwd(path.rawdata)
df.2012 <- read.csv2('./original data/Bra2012.csv', header = TRUE, 
                     stringsAsFactors = FALSE)
df.2013 <- read.csv2('./original data/Bra2013.csv', header = TRUE, 
                     stringsAsFactors = FALSE)
df.2014.wb <- read.csv2('./original data/WBra2014.csv', header = TRUE, 
                        stringsAsFactors = FALSE)
df.2014.hvb_bzo <- read.csv2('./original data/HVB en BZO 2014.csv', header = TRUE, 
                             stringsAsFactors = FALSE)
df.2014 <- rbind(df.2014.wb, df.2014.hvb_bzo)
df.2015 <- read.csv2('./original data/Bra2015.csv', header = TRUE, 
                     stringsAsFactors = FALSE)
df.2016 <- read.csv2('./original data/Bra2016.csv', header = TRUE, 
                     stringsAsFactors = FALSE)
df.2017.wb <- read.csv2('./original data/WBra2017.csv', header = TRUE, 
                        stringsAsFactors = FALSE)
df.2017.hvb_bzo <- read.csv2('./original data/HVB en BZO 2017.csv', header = TRUE, 
                             stringsAsFactors = FALSE)
df.2017 <- rbind(df.2017.wb, df.2017.hvb_bzo)
df.2018.wb <- read.csv2('./original data/2018/WBra2018_tem24082018.csv', header = TRUE, 
                        stringsAsFactors = FALSE)
df.2018.hvb_bzo <- read.csv2('./original data/HVB en BZO 2018.csv', header = TRUE, 
                             stringsAsFactors = FALSE)
df.2018 <- rbind(df.2018.wb, df.2018.hvb_bzo)


## some variables do not exist in previous years
## or have a different name
df.2012$SOAOPLEIDING    <- as.numeric(NA)
df.2012$SOAOPL_ANDERS   <- as.numeric(NA)
df.2012$SOAHULPVRAAG    <- as.numeric(NA)
df.2012$SOAGEWAARSOA    <- as.numeric(NA)
df.2012$SOAPREP         <- as.numeric(NA)

## mtm is SOACONSULT == 2 in 2006 and 2017. For 2012 to 2015 mtm is SOAMTM
df.2012$SOACONSULT      <- ifelse(df.2012$SOAMTM == 1, "MTM", "")
df.2012$SOATEST         <- as.numeric(NA)
df.2012$SOAGEWELD       <- as.numeric(NA)
df.2012$SOAPARTNERDLGRP <- as.numeric(NA)

df.2013$SOAOPLEIDING    <- as.numeric(NA)
df.2013$SOAOPL_ANDERS   <- as.numeric(NA)
df.2013$SOAHULPVRAAG    <- as.numeric(NA)
df.2013$SOAGEWAARSOA    <- as.numeric(NA)
df.2013$SOACONSULT      <- as.numeric(NA)
df.2013$SOACONSULT      <- ifelse(df.2013$SOAMTM == 1, "MTM", "")
df.2013$SOATEST         <- as.numeric(NA)
df.2013$SOAGEWELD       <- as.numeric(NA)
df.2013$SOAPARTNERDLGRP <- as.numeric(NA)
df.2013$SOAPREP         <- as.numeric(NA)

df.2014$SOAGEWAARSOA    <- as.numeric(NA)
df.2014$SOACONSULT      <- as.numeric(NA)
df.2014$SOACONSULT      <- ifelse(df.2014$SOAMTM == "Ja", "MTM", "")
df.2014$SOATEST         <- as.numeric(NA)
df.2014$SOAGEWELD       <- as.numeric(NA)
df.2014$SOAPARTNERDLGRP <- as.numeric(NA)
df.2014$SOAPREP         <- as.numeric(NA)

df.2015$SOACONSULT      <- as.numeric(NA)
df.2015$SOACONSULT      <- ifelse(df.2015$SOAMTM == 1, "MTM", "")
df.2015$SOATEST         <- as.numeric(NA)
df.2015$SOAGEWELD       <- as.numeric(NA)
df.2015$SOAPARTNERDLGRP <- as.numeric(NA)
df.2015$SOAPREP         <- as.numeric(NA)

df.2016$SOAMTM  <- as.numeric(NA)       
df.2016$SOAPREP <- as.numeric(NA)

df.2017$SOAMTM          <- as.numeric(NA)
df.2017$SOAPARTNERDLGRP <- df.2017$SOAPARTNERDLGRP2
df.2017$SOAPREP         <- as.numeric(NA)

df.2018$SOAMTM          <- as.numeric(NA)
df.2018$SOAPARTNERDLGRP <- df.2018$SOAPARTNERDLGRP2
df.2018$SOAPART         <- as.numeric(NA)
# Condoom is nu uitgesplits naar soort seksuele handeling, oraal, anaal, vaginaal.
df.2018$SOACONDOOM      <- as.numeric(NA)
# Ulceraties is nu uitgesplitst naar lymphpgranuloma venereum anaal/urehtral en oraal
df.2018$SOA19U          <- df.2018$SOA19U2



## rename test location
loc.2012.idx <- unique(df.2012$MELDLOCATIE)
df.2012$MELDLOCATIE[df.2012$MELDLOCATIE == loc.2012.idx[1]] <- "Eindhoven"
df.2012$MELDLOCATIE[df.2012$MELDLOCATIE == loc.2012.idx[2]] <- "Helmond"
df.2012$MELDLOCATIE[df.2012$MELDLOCATIE == loc.2012.idx[3]] <- "Tilburg"
df.2012$MELDLOCATIE[df.2012$MELDLOCATIE == loc.2012.idx[4]] <- "Den Bosch"
df.2012$MELDLOCATIE[df.2012$MELDLOCATIE == loc.2012.idx[5]] <- "Project"
df.2012$MELDLOCATIE[df.2012$MELDLOCATIE == loc.2012.idx[6]] <- "Breda"
df.2012$MELDLOCATIE[df.2012$MELDLOCATIE == loc.2012.idx[7]] <- "Bergen op Zoom"

loc.2013.idx <- unique(df.2013$MELDLOCATIE)
df.2013$MELDLOCATIE[df.2013$MELDLOCATIE == loc.2013.idx[1]] <- "Eindhoven"
df.2013$MELDLOCATIE[df.2013$MELDLOCATIE == loc.2013.idx[2]] <- "Helmond"
df.2013$MELDLOCATIE[df.2013$MELDLOCATIE == loc.2013.idx[3]] <- "Tilburg"
df.2013$MELDLOCATIE[df.2013$MELDLOCATIE == loc.2013.idx[4]] <- "Den Bosch"
df.2013$MELDLOCATIE[df.2013$MELDLOCATIE == loc.2013.idx[5]] <- "Project"
df.2013$MELDLOCATIE[df.2013$MELDLOCATIE == loc.2013.idx[6]] <- "Oss"
df.2013$MELDLOCATIE[df.2013$MELDLOCATIE == loc.2013.idx[7]] <- "Breda"
df.2013$MELDLOCATIE[df.2013$MELDLOCATIE == loc.2013.idx[8]] <- "Project"
df.2013$MELDLOCATIE[df.2013$MELDLOCATIE == loc.2013.idx[9]] <- "Roosendaal"
df.2013$MELDLOCATIE[df.2013$MELDLOCATIE == loc.2013.idx[10]] <- "Bergen op Zoom"

loc.2014.idx <- unique(df.2014$MELDLOCATIE)
df.2014$MELDLOCATIE[df.2014$MELDLOCATIE == loc.2014.idx[1]] <- "Breda"
df.2014$MELDLOCATIE[df.2014$MELDLOCATIE == loc.2014.idx[2]] <- "Roosendaal"
df.2014$MELDLOCATIE[df.2014$MELDLOCATIE == loc.2014.idx[3]] <- "Bergen op Zoom"
df.2014$MELDLOCATIE[df.2014$MELDLOCATIE == loc.2014.idx[4]] <- "Breda"
df.2014$MELDLOCATIE[df.2014$MELDLOCATIE == loc.2014.idx[5]] <- "Roosendaal"
df.2014$MELDLOCATIE[df.2014$MELDLOCATIE == loc.2014.idx[6]] <- "Bergen op Zoom"
df.2014$MELDLOCATIE[df.2014$MELDLOCATIE == loc.2014.idx[7]] <- "" #"Zutphen" n = 1
df.2014$MELDLOCATIE[df.2014$MELDLOCATIE == loc.2014.idx[8]] <- "Breda"
df.2014$MELDLOCATIE[df.2014$MELDLOCATIE == loc.2014.idx[9]] <- "" #"Harderwijk" n = 1
df.2014$MELDLOCATIE[df.2014$MELDLOCATIE == loc.2014.idx[10]] <- "" #"Heerlen" n = 2
df.2014$MELDLOCATIE[df.2014$MELDLOCATIE == loc.2014.idx[11]] <- "Den Bosch"
df.2014$MELDLOCATIE[df.2014$MELDLOCATIE == loc.2014.idx[12]] <- "" #"Nijmegen" n = 1
df.2014$MELDLOCATIE[df.2014$MELDLOCATIE == loc.2014.idx[13]] <- "" #"Dordrecht" n = 1
df.2014$MELDLOCATIE[df.2014$MELDLOCATIE == loc.2014.idx[14]] <- "Halsteren"
df.2014$MELDLOCATIE[df.2014$MELDLOCATIE == loc.2014.idx[15]] <- "Tilburg"
df.2014$MELDLOCATIE[df.2014$MELDLOCATIE == loc.2014.idx[16]] <- "Den Bosch"
df.2014$MELDLOCATIE[df.2014$MELDLOCATIE == loc.2014.idx[17]] <- "Eindhoven"
df.2014$MELDLOCATIE[df.2014$MELDLOCATIE == loc.2014.idx[18]] <- "Tilburg"
df.2014$MELDLOCATIE[df.2014$MELDLOCATIE == loc.2014.idx[19]] <- "Helmond"
df.2014$MELDLOCATIE[df.2014$MELDLOCATIE == loc.2014.idx[20]] <- "Centrum Jeugd en Gezin"
df.2014$MELDLOCATIE[df.2014$MELDLOCATIE == loc.2014.idx[21]] <- "Eindhoven"
df.2014$MELDLOCATIE[df.2014$MELDLOCATIE == loc.2014.idx[22]] <- "Helmond"
df.2014$MELDLOCATIE[df.2014$MELDLOCATIE == loc.2014.idx[23]] <- "Den Bosch"
df.2014$MELDLOCATIE[df.2014$MELDLOCATIE == loc.2014.idx[24]] <- "" #Groningen
df.2014$MELDLOCATIE[df.2014$MELDLOCATIE == loc.2014.idx[25]] <- "" #Enschede
df.2014$MELDLOCATIE[df.2014$MELDLOCATIE == loc.2014.idx[26]] <- "" #Goes
df.2014$MELDLOCATIE[df.2014$MELDLOCATIE == loc.2014.idx[27]] <- "Valkenswaard"


loc.2017.idx <- unique(df.2017$MELDLOCATIE)
df.2017$MELDLOCATIE[df.2017$MELDLOCATIE == loc.2017.idx[10]] <- "Valkenswaard"
df.2017$MELDLOCATIE[df.2017$MELDLOCATIE == loc.2017.idx[11]] <- "Den Bosch"

loc.2018.idx <- unique(df.2018$MELDLOCATIE)
df.2018$MELDLOCATIE[df.2018$MELDLOCATIE == loc.2018.idx[4]] <- "Breda"
df.2018$MELDLOCATIE[df.2018$MELDLOCATIE == loc.2018.idx[9]] <- "Valkenswaard"



## check is variables names exist in current data set.
#vn1.idx <- match(vn, names(df.2018))
#vn[which(is.na(vn1.idx))]


## create dataframe based on vn-variable
df <- rbind(df.2012[, vn], df.2013[, vn], df.2014[, vn], df.2015[, vn], 
            df.2016[, vn], df.2017[, vn], df.2018[, vn]) 


# rename columns
colnames(df) <- make.names(c("MELDINGSNR", "OSIRISNR", "GGD", name$vraa_nm))

#table(df$testlocatie)
#df.2016$MELDLOCATIE[df.2016$SOACONSULT == 1]
#as.data.frame(unique(df$MELDLOCATIE))

# replace NA with ""
df[is.na(df)] <- "" 


# load landenlijst
landenLijst <- readWorkbook(cb, sheet = "LANDENLIJST")

# consult data
df$Datum.consult <- as.Date(df$Datum.consult, format = '%d-%m-%Y')


#table(df$Soa.consult)
df$Soa.consult[df$Soa.consult == 1] <- "Regulier"
df$Soa.consult[df$Soa.consult %in% c(2, "MTM")] <- "Man tot Man"
df$Soa.consult[df$Soa.consult == 3] <- "Thuistest pakket"
df$Soa.consult[df$Soa.consult == ""] <- "Onbekend"


# Soa
#table(df$In.de.afgelopen.jaar.GO_CT_SYF.gehad)
df$In.de.afgelopen.jaar.GO_CT_SYF.gehad[df$In.de.afgelopen.jaar.GO_CT_SYF.gehad %in% c(1, "nee")] <- "Nee"
df$In.de.afgelopen.jaar.GO_CT_SYF.gehad[df$In.de.afgelopen.jaar.GO_CT_SYF.gehad %in% c(2, "ja")] <- "Ja"
# weet niet en onbekend zijn samengevoegd
df$In.de.afgelopen.jaar.GO_CT_SYF.gehad[df$In.de.afgelopen.jaar.GO_CT_SYF.gehad %in% c(3, 8, "onbekend", "weet niet", "")] <- "Onbekend"

# gonorroe
#table(df$Diagnose.gonorroe)
df$Diagnose.gonorroe[df$Diagnose.gonorroe %in% c(5, "Negatief")] <- "Negatief"
df$Diagnose.gonorroe[df$Diagnose.gonorroe == ""] <- "Niet uitgevoerd"
idx1 <- grep(1, df$Diagnose.gonorroe)
idx2 <- grep(2, df$Diagnose.gonorroe)
idx3 <- grep(3, df$Diagnose.gonorroe)
idx4 <- grep(6, df$Diagnose.gonorroe)
idx5 <- grep(7, df$Diagnose.gonorroe)
df$Diagnose.gonorroe[idx1] <- "Positief"
df$Diagnose.gonorroe[idx2] <- "Positief"
df$Diagnose.gonorroe[idx3] <- "Positief"
df$Diagnose.gonorroe[idx4] <- "Positief"
df$Diagnose.gonorroe[idx5] <- "Positief"
df$Diagnose.gonorroe[!c(df$Diagnose.gonorroe %in% c("Positief", "Negatief", "Niet uitgevoerd"))] <- "Positief"

# chlamydia
#table(df$Diagnose.chlamydia)
df$Diagnose.chlamydia[df$Diagnose.chlamydia %in% c(5, "Negatief")] <- "Negatief"
df$Diagnose.chlamydia[df$Diagnose.chlamydia == ""] <- "Niet uitgevoerd"
idx1 <- grep(1, df$Diagnose.chlamydia)
idx2 <- grep(2, df$Diagnose.chlamydia)
idx3 <- grep(3, df$Diagnose.chlamydia)
idx4 <- grep(6, df$Diagnose.chlamydia)
idx5 <- grep(7, df$Diagnose.chlamydia)
df$Diagnose.chlamydia[idx1] <- "Positief"
df$Diagnose.chlamydia[idx2] <- "Positief"
df$Diagnose.chlamydia[idx3] <- "Positief"
df$Diagnose.chlamydia[idx4] <- "Positief"
df$Diagnose.chlamydia[idx5] <- "Positief"
df$Diagnose.chlamydia[!c(df$Diagnose.chlamydia %in% c("Positief", "Negatief", "Niet uitgevoerd"))] <- "Positief"

# syfilis
# "reeds eerder" = negative
#table(df$Diagnose.syfilis)
df$Diagnose.syfilis[df$Diagnose.syfilis %in% c(1,2,3,4,5, "Lues I", "Lues latens recens", "Lues latens tarda",
                                               "Lues, stadium onbekend")] <- "Positief"
df$Diagnose.syfilis[df$Diagnose.syfilis %in% c(6, 7, "Negatief", "(Reeds eerder) behandelde lues")] <- "Negatief"
df$Diagnose.syfilis[df$Diagnose.syfilis == ""] <- "Niet uitgevoerd"

# HIV
#table(df$Diagnose.HIV)
df$Diagnose.HIV[df$Diagnose.HIV %in% c(1, "HIV positief")] <- "Positief"
df$Diagnose.HIV[df$Diagnose.HIV %in% c(2, "HIV Negatief")] <- "Negatief"
df$Diagnose.HIV[df$Diagnose.HIV == ""] <- "Niet uitgevoerd"

# Prep use
#table(df$ooit.prep.gebruikt)
df$ooit.prep.gebruikt[df$ooit.prep.gebruikt == 0] <- "Nee"
df$ooit.prep.gebruikt[df$ooit.prep.gebruikt == 1] <- "Ja, in de afgelopen 6 maanden"
df$ooit.prep.gebruikt[df$ooit.prep.gebruikt == 2] <- "Ja, langer dan 6 maanden geleden"
df$ooit.prep.gebruikt[df$ooit.prep.gebruikt == ""] <- "Onbekend"
  
# hepatitis B
# 3 = Hepatitis B, doorgemaakt, 5 = Hepatitis B, infectieus
#table(df$Diagnose.hepatitis.B)
df$Diagnose.hepatitis.B[df$Diagnose.hepatitis.B %in% c(3,5, "Hepatitis B, doorgemaakt", "Hepatitis B, infectieus")] <- "Positief"
df$Diagnose.hepatitis.B[df$Diagnose.hepatitis.B %in% c(4, "Negatief")] <- "Negatief"
df$Diagnose.hepatitis.B[df$Diagnose.hepatitis.B == ""] <- "Niet uitgevoerd"

# hepatitis C
#table(df$Diagnose.hepatitis.C)
df$Diagnose.hepatitis.C[df$Diagnose.hepatitis.C %in% c(1, "Positief")] <- "Positief"
df$Diagnose.hepatitis.C[df$Diagnose.hepatitis.C %in% c(2, "Negatief")] <- "Negatief"
df$Diagnose.hepatitis.C[df$Diagnose.hepatitis.C == ""] <- "Niet uitgevoerd"
                                                

# condylomata acuminata (genitale wratten)
# "reeds eerder" = negatief
#table(df$Diagnose.condylomata.acuminata)
df$Diagnose.condylomata.acuminata[df$Diagnose.condylomata.acuminata %in% c(1, "Ja, nieuw")] <- "Positief"
df$Diagnose.condylomata.acuminata[df$Diagnose.condylomata.acuminata %in% c(2, 3, "Ja, reeds eerder geconstateerd", "Nee")] <- "Negatief"
df$Diagnose.condylomata.acuminata[df$Diagnose.condylomata.acuminata == ""] <- "Niet uitgevoerd"

# Note there is no 0 here!
#table(df$Ulceraties)
df$Ulceraties[df$Ulceraties != ""] <- "Positief"
df$Ulceraties[df$Ulceraties == ""] <- "Niet uitgevoerd"

## This information is already gathered by Diagnose condylomata acuminata
df$Uitgevoerd.onderzoek.condylomata.acuminata[df$Uitgevoerd.onderzoek.condylomata.acuminata %in% c("N", "Nee")] <- "Nee"
df$Uitgevoerd.onderzoek.condylomata.acuminata[df$Uitgevoerd.onderzoek.condylomata.acuminata %in% c("J", "Ja")] <- "Ja"
df$Uitgevoerd.onderzoek.condylomata.acuminata[df$Uitgevoerd.onderzoek.condylomata.acuminata %in% c("")] <- "Onbekend"


## This information is already gathered by Uitslag.serovartypering
df$Serovartypering.gedaan.tbv.diagnostiek.LGV <- NULL

#table(df$Uitslag.serovartypering)
df$Uitslag.serovartypering[df$Uitslag.serovartypering %in% c(1, "Positief")] <- "Positief"
df$Uitslag.serovartypering[df$Uitslag.serovartypering %in% c(0, "Negatief")] <- "Negatief"
df$Uitslag.serovartypering[df$Uitslag.serovartypering %in% c(2, "LGV-PCR niet te beoordelen")] <- "Niet te beoordelen"
df$Uitslag.serovartypering[df$Uitslag.serovartypering %in% c("")] <- "Niet uitgevoerd"


# This level of detail is not necessary. Also, the 2 cases of hepatitis C are infectius. 
df$Infectieus <- NULL

# 3 = onbekend
#table(df$Was.dit.met.een.vaste.of.losse.partner)
#df$Was.dit.met.een.vaste.of.losse.partner[df$Was.dit.met.een.vaste.of.losse.partner %in% c(1, "Vaste partner")] <- "Vast"
#df$Was.dit.met.een.vaste.of.losse.partner[df$Was.dit.met.een.vaste.of.losse.partner %in% c(2, "Losse partner")] <- "Los"
#df$Was.dit.met.een.vaste.of.losse.partner[df$Was.dit.met.een.vaste.of.losse.partner %in% c(3, "Onbekend", "")] <- "Onbekend"

#  > 999 removed
#table(df$Met.hoeveel.personen.heeft.u.seksueel.contact.gehad.in.de.afgelopen.6.maanden)
#df$Met.hoeveel.personen.heeft.u.seksueel.contact.gehad.in.de.afgelopen.6.maanden <- as.numeric(df$Met.hoeveel.personen.heeft.u.seksueel.contact.gehad.in.de.afgelopen.6.maanden)
#df$Met.hoeveel.personen.heeft.u.seksueel.contact.gehad.in.de.afgelopen.6.maanden[df$Met.hoeveel.personen.heeft.u.seksueel.contact.gehad.in.de.afgelopen.6.maanden >= 999] <- as.numeric(NA)

# current year
#cyear <- as.integer(format(df$Datum.consult, "%Y"))
#age <- cyear - df$Geboortejaar
#age.young.idx <- which(age > 90)
#df$Geboortejaar[age.young.idx] <- as.numeric(NA)

#if (any(cyear - as.numeric(df$Jaar.eerdere.hiv.test[!na.idx]) > 100)) {
#  warning("Sommige personen zijn ouder dan 100 jaar. Dit is mogelijk een invoerfout.")
#  cat(which(cyear - as.numeric(df$Jaar.eerdere.hiv.test) > 100))
#}

#table(df$Slachtoffer.van.seksueel.geweld.of.verkrachting)
#df$Slachtoffer.van.seksueel.geweld.of.verkrachting[df$Slachtoffer.van.seksueel.geweld.of.verkrachting == 1] <- "Ja"
#df$Slachtoffer.van.seksueel.geweld.of.verkrachting[df$Slachtoffer.van.seksueel.geweld.of.verkrachting == 0] <- "Nee"
#df$Slachtoffer.van.seksueel.geweld.of.verkrachting[df$Slachtoffer.van.seksueel.geweld.of.verkrachting %in% c(9, "")] <- "Onbekend"

#table(df$Eerder.HIV.test.met.uitslag)
df$Eerder.HIV.test.met.uitslag[df$Eerder.HIV.test.met.uitslag %in% c(2, "ja, positief")] <- "jaPositief"
df$Eerder.HIV.test.met.uitslag[df$Eerder.HIV.test.met.uitslag %in% c(3, "ja, negatief")] <- "jaNegatief"
df$Eerder.HIV.test.met.uitslag[df$Eerder.HIV.test.met.uitslag %in% c(4, "ja, uitslag onbekend")] <- "jaOnbekend"
df$Eerder.HIV.test.met.uitslag[df$Eerder.HIV.test.met.uitslag %in% c(1, "nee")] <- "Nee"
df$Eerder.HIV.test.met.uitslag[df$Eerder.HIV.test.met.uitslag %in% c(8, "onbekend")] <- "Onbekend"
df$Eerder.HIV.test.met.uitslag[df$Eerder.HIV.test.met.uitslag == ""] <- "Onbekend"

#table(df$Heeft.u.condooms.gebruikt.bij.het.laatste.seksuele.contact)
#df$Heeft.u.condooms.gebruikt.bij.het.laatste.seksuele.contact[df$Heeft.u.condooms.gebruikt.bij.het.laatste.seksuele.contact %in% c(1, "Ja")] <- "Ja"
#df$Heeft.u.condooms.gebruikt.bij.het.laatste.seksuele.contact[df$Heeft.u.condooms.gebruikt.bij.het.laatste.seksuele.contact %in% c(0, "Nee")] <- "Nee"
#df$Heeft.u.condooms.gebruikt.bij.het.laatste.seksuele.contact[df$Heeft.u.condooms.gebruikt.bij.het.laatste.seksuele.contact %in% c(9, "Onbekend", "")] <- "Onbekend"

# Note that all the below might be the same, since it's the same number of NA's
#table(df$Gewaarschuwd.door.partner)
df$Gewaarschuwd.door.partner[df$Gewaarschuwd.door.partner %in% c(1, "Ja")] <- "Ja"
df$Gewaarschuwd.door.partner[df$Gewaarschuwd.door.partner %in% c(0, "Nee")] <- "Nee"
df$Gewaarschuwd.door.partner[df$Gewaarschuwd.door.partner %in% c(9, "Onbekend", "")] <- "Onbekend"

#table(df$Soa.gerelateerde.klachten)
df$Soa.gerelateerde.klachten[df$Soa.gerelateerde.klachten %in% c(1, "Ja")] <- "Ja"
df$Soa.gerelateerde.klachten[df$Soa.gerelateerde.klachten %in% c(0, "Nee")] <- "Nee"
df$Soa.gerelateerde.klachten[df$Soa.gerelateerde.klachten %in% c(9, "Onbekend", "")] <- "Onbekend"

#table(df$Partner.uit.doelgroep)
df$Partner.uit.doelgroep[df$Partner.uit.doelgroep %in% c(1, "Ja")] <- "Ja"
df$Partner.uit.doelgroep[df$Partner.uit.doelgroep %in% c(0, "Nee")] <- "Nee"
df$Partner.uit.doelgroep[df$Partner.uit.doelgroep %in% c(9, "Onbekend", "")] <- "Onbekend"

#table(df$Welke.doelgroep)
df$Welke.doelgroep[df$Welke.doelgroep == 1] <- "Partner endemisch"
df$Welke.doelgroep[df$Welke.doelgroep == 2] <- "Ouders endemisch"
df$Welke.doelgroep[df$Welke.doelgroep == 3] <- "Onbekend"
df$Welke.doelgroep[df$Welke.doelgroep == ""] <- "Onbekend"
df$Welke.doelgroep[df$Welke.doelgroep == 4] <- "MSM"

#table(df$Huidige.of.laatst.genoten.opleiding)
df$Huidige.of.laatst.genoten.opleiding[df$Huidige.of.laatst.genoten.opleiding %in% c(1, "geen")] <- "Geen"
df$Huidige.of.laatst.genoten.opleiding[df$Huidige.of.laatst.genoten.opleiding %in% c(2, "basisonderwijs")]     <- "Basisonderwijs"
df$Huidige.of.laatst.genoten.opleiding[df$Huidige.of.laatst.genoten.opleiding %in% c(3, "lbo/mavo/vmbo/mbo")]  <- "LBO/MAVO/VMBO/MBO"
df$Huidige.of.laatst.genoten.opleiding[df$Huidige.of.laatst.genoten.opleiding %in% c(4, "havo/vwo/gymnasium")] <- "HAVO/VWO"
df$Huidige.of.laatst.genoten.opleiding[df$Huidige.of.laatst.genoten.opleiding %in% c(5, "hbo/wo")] <- "HBO/WO"
df$Huidige.of.laatst.genoten.opleiding[df$Huidige.of.laatst.genoten.opleiding %in% c(6, "anders, nl")] <- "Anders"
df$Huidige.of.laatst.genoten.opleiding[df$Huidige.of.laatst.genoten.opleiding %in% c(9, "onbekend", "")] <- "Onbekend"

df$Prostituant <- NULL


# country of origin patient
code.idx <- grep("[[:digit:]]", df$Geboorteland.patient)
#char.idx <- grep("[A-Za-z]", df$Geboorteland.patient)
landcode.idx <- match(df$Geboorteland.patient[code.idx], landenLijst$landcode)
df$Geboorteland.patient[code.idx] <- landenLijst$landennaam[landcode.idx]
df$Geboorteland.patient[is.na(df$Geboorteland.patient[code.idx])] <- "Onbekend"
landcode2.idx <- match(df$Geboorteland.patient, landenLijst$landennaam)
df$Geboorteland.patient.endemisch <- landenLijst$endemisch[landcode2.idx]
df$Geboorteland.patient.endemisch[df$Geboorteland.patient.endemisch == "ja"] <- "Ja"
df$Geboorteland.patient.endemisch[df$Geboorteland.patient.endemisch == "nee"] <- "Nee"
#sum(table(df$Geboorteland.patient.endemisch))

# library(dplyr)
# test <- df
# test$year <- as.integer(format(test$Datum.consult, "%Y"))
# test <- test %>% group_by(GGD, year) %>%
#   summarize(sum(is.na(Geboorteland.patient.endemisch)))
# tail(test)

################################################################################
## gender
#table(df$Sekse)
df$Sekse[df$Sekse == 1] <- "Man"
df$Sekse[df$Sekse == 2] <- "Vrouw"
df$Sekse[df$Sekse == 3] <- "Transgender"

## prostituee
#table(df$Prostituee)
df$Prostituee[df$Prostituee == 1]          <- "Nee"
df$Prostituee[df$Prostituee == "nee"]      <- "Nee"
df$Prostituee[df$Prostituee == 3]          <- "Ja"
df$Prostituee[df$Prostituee == "ja"]       <- "Ja"
df$Prostituee[df$Prostituee == 8]          <- "Onbekend"
df$Prostituee[df$Prostituee == "onbekend"] <- "Onbekend"
df$Prostituee[df$Prostituee == ""]         <- "Onbekend"

## sexual preference
#table(df$Seksuele.voorkeur)
df$Seksuele.voorkeur[df$Seksuele.voorkeur == 1]                <- "Heteroseksueel"
df$Seksuele.voorkeur[df$Seksuele.voorkeur == "heteroseksueel"] <- "Heteroseksueel"
df$Seksuele.voorkeur[df$Seksuele.voorkeur == 2]                <- "Homoseksueel"
df$Seksuele.voorkeur[df$Seksuele.voorkeur == "homoseksueel"]   <- "Homoseksueel"
df$Seksuele.voorkeur[df$Seksuele.voorkeur == 3]                <- "Biseksueel"
df$Seksuele.voorkeur[df$Seksuele.voorkeur == "biseksueel"]     <- "Biseksueel"
df$Seksuele.voorkeur[df$Seksuele.voorkeur == 8]                <- "Onbekend"
df$Seksuele.voorkeur[df$Seksuele.voorkeur == ""]               <- "Onbekend"
df$Seksuele.voorkeur[df$Seksuele.voorkeur == "onbekend"]       <- "Onbekend"


## msm (males sex with males): homo + bi
sekse.man.idx <- which(df$Sekse == "Man")
voorkeur.idx  <- which(df$Seksuele.voorkeur == "Homoseksueel" | df$Seksuele.voorkeur == "Biseksueel")
df$MSM <- "Nee"
df$MSM[intersect(sekse.man.idx, voorkeur.idx)] <- "Ja"

## add city, municipaliy, province, latitude and longitude to dataframe
df$Postcode.patient.cijfers[df$Postcode.patient.cijfers == 9999] <- ""
pc4_coordinates <- readWorkbook(cb, sheet = "PC4")
library(dplyr)
pc4_coordinates <- pc4_coordinates %>% mutate(postcode = as.numeric(postcode))

md <- df %>% mutate(Postcode.patient.cijfers = as.numeric(Postcode.patient.cijfers))
md <- left_join(md, pc4_coordinates, by = c("Postcode.patient.cijfers" = "postcode"))


vn <- c("Diagnose.gonorroe", "Diagnose.chlamydia", "Diagnose.syfilis",
        "Diagnose.HIV", "Diagnose.hepatitis.B", "Diagnose.condylomata.acuminata",
        "Diagnose.hepatitis.C")

fn1 <- function(x) {
  if (any(x == "Positief")) {
    x <- 1
  } else if (all(x == "Niet uitgevoerd")) {
    x <- 9
  } else if (all(x == "Negatief")) {
    x <- 0
  } else {
    # combination negatief and niet uitgevoerd
    x <- 0
  }
}

vn.idx <- match(vn, colnames(md))
md$STD.positief <- apply(md[, vn.idx], 1, function(y) fn1(y))


## Data preprocessing
md <- md %>% mutate(datum_consult_mod   = as.numeric(format(Datum.consult, '%Y')))
md <- md %>% mutate(datum_consult_year  = as.numeric(format(Datum.consult, '%Y')))
md <- md %>% mutate(datum_consult_month = as.numeric(format(Datum.consult, '%m')))

# geboorte jaar
md$Geboortejaar <- lubridate::ymd(md$Geboortejaar, truncated = 2)
md$Geboortejaar_mod <- as.numeric(format(md$Geboortejaar, '%Y'))

########
# remove non-used columns
md$Swinger <- NULL
md$MELDINGSNR <- NULL
md$OSIRISNR <- NULL
md$Geboorteland.moeder <- NULL
md$Geboorteland.vader <- NULL
md$provincie <- NULL
md$gemeente <- NULL
md$anders..namelijk <- NULL
md$Hulpvraag <- NULL
md$Andere.aandoening <- NULL
md$Andere.indicatie <- NULL
md$Jaar.eerdere.hiv.test <- NULL
md$Maand.eerdere.hiv.test <- NULL
md$Heeft.u.condooms.gebruikt.bij.het.laatste.seksuele.contact <- NULL
md$Was.dit.met.een.vaste.of.losse.partner <- NULL
md$Overige.huid.aandoeningen <- NULL
md$Slachtoffer.van.seksueel.geweld.of.verkrachting <- NULL
md$Uitgevoerd.laboratorium.onderzoek <- NULL
md$Ulceraties <- NULL
md$Met.hoeveel.personen.heeft.u.seksueel.contact.gehad.in.de.afgelopen.6.maanden <- NULL
md$Voor.welke.soa.is.de.client.gewaarschuwd <- NULL
md$Acute.of.chronische.infectie <- NULL

########
saveRDS(md, "./SOAapp/data/md.rds")
fwrite(md, "./SOAapp/data/md.csv")
fst::write_fst(md, "./SOAapp/data/md.fst")

