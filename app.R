
# Climveturi App
# 19.10.2020, 21.10



# Clear workspace
rm(list = ls())

### INITIALISE -----------------------------------------------------------------

#### Working paths ####
wd <- getwd()

# Component paths. Commented because this does not exist ahaha
#csspath <- file.path(wd, "app_style.css")

# App version
app_v <- "0001 (19.10.2020)"

# Install libraries
#install.packages("shiny")
#install.package("ggplot2")

# Import libraries
require(shiny)
require(ggplot2)
require(dplyr)
require(lubridate)
require(reshape2)



setwd("C:/Users/e1007642/Documents/ClimVeturi")

### DATA WRANGLING -------------------------------------------------------------

# Output: list with as many dataframes with all possible combinations of data
# plus reference dataframes for all areas (162 + 27)

nyky <- read.table("data/suomiq__control_run__nyky_nyky_climveturi_0320_2.dat", sep = "", dec =".", header = FALSE,
                        colClasses = c(rep("character", 115), rep("NULL", 745)))

nyky <- as.data.frame(sapply(nyky, as.numeric))

colnames(nyky) = c("TimeStamp","1", "2","3","4.9","4.8","4.7","4.6","4.5","4.4","4.3","4.2","4.1","5","6","7","8","9","10","11","12","13",
                        "14.9","14.8","14.7","14.6","14.5","14.4","14.3","14.2","14.1","15","16","17","18","19","20","21","22","23","24","25",
                        "26","27","28","29","30","31","32","33","34","35.9","35.8","35.7","35.6","35.5","35.4","35.3","35.2","35.1","36","37",
                        "38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59.9","59.8",
                        "59.7","59.6","59.5","59.4","59.3","59.2","59.1","60","61","62","63","64","65.9","65.8","65.7","65.6","65.5","65.4",
                        "65.3","65.2","65.1","66","67","68","69","70","71","72","73","74")


# 2. Valitaan tarkasteltavat sarakkeet (Nooran viesti 21.9 + 19.10) --> MUOKATTAVISSA, lisää vain vesistön numeron

nyky <- nyky[c("TimeStamp", "4.1", "14.1", "14.6", "14.7", "21", "28", "35.1", "35.7", "35.9", "37", "41", "42", "44", "49",
                    "53", "54", "57", "59.1", "59.9","61", "64","65.1","65.6", "65.8","67","68", "71")]

# Nimetään vesistöalueet tulevien ongelmien välttämiseksi (jos muutettu edellistä, lisätään myös nimi tähän litaniaan oikealle kohdalle)
colnames(nyky) <- c("TimeStamp", "Vuoksi", "Kymijoki", "Saarijärven reitti", "Rautalammin reitti","Vantaanjoki",
                    "Aurajoki","Kokemäenjoki","Längelmäveden-Hauhon reitti", 
                    "Loimijoki","Lapväärtinjoki", "Laihianjoki",
                    "Kyrönjoki", "Lapuanjoki","Perhonjoki", 
                    "Kalajoki", "Pyhäjoki", "Siikajoki","Ala-Oulujoki", "Ontojärvi-Lentua","Iijoki", "Simojoki",
                    "Ala-Kemijoki","Ylä-Ounasjoki", "Kitinen", "Tornionjoki-Muonionjoki","Teno", "Paatsjoki")

nyky$TimeStamp <- ymd(nyky$TimeStamp)
nyky <- nyky[!(format(nyky$TimeStamp, "%m") == "02" & format(nyky$TimeStamp, "%d")==29), , drop = FALSE]
nyky$D_M <- format(nyky$TimeStamp, "%m-%d")


# list of daily means for each place for reference time period
ref_list <- list()

for (nimi in colnames(nyky)) {
  data <- nyky %>%
    select(D_M, nimi) %>%
    group_by(D_M) %>%
    summarize_all(list(mean = mean, max = max, min = min)) %>%
    data.frame()
  
  name <- paste(nimi, "ref", sep ="_")
  
  if (name == "TimeStamp_ref" | name == "D_M_ref") {
    next
  }
  data$D_M <- as.Date(data$D_M, format="%m-%d") 
  ref_list[[name]] <- data
  
  
}

# ------------------

# 2010-39

lammin_39 <- read.table("data/suomiq_198110_201039_IPCC5_MIROC-ESM-CHEM_rcp45_climveturi.dat", sep = "", dec =".", header = FALSE,
                        colClasses = c(rep("character", 115), rep("NULL", 745)))
kylma_39 <- read.table("data/suomiq_198110_201039_IPCC5_CESM1-CAM5_rcp26_climveturi.dat", sep = "", dec =".", header = FALSE,
                       colClasses = c(rep("character", 115), rep("NULL", 745)))
ka_39 <- read.table("data/suomiq_198110_201039_IPCC5_ka_rcp45_climveturi.dat", sep = "", dec =".", header = FALSE,
                    colClasses = c(rep("character", 115), rep("NULL", 745)))

list39 <- list()
list39[["lammin_39"]] <- lammin_39
list39[["kylma_39"]] <- kylma_39
list39[["ka_39"]] <- ka_39

# Select locations, rename and create datetimes
i <- 1
for (e in list39) {
  a <- e
  a <- as.data.frame(sapply(e, as.numeric))
  
  colnames(a) = c("TimeStamp","1", "2","3","4.9","4.8","4.7","4.6","4.5","4.4","4.3","4.2","4.1","5","6","7","8","9","10","11","12","13",
                  "14.9","14.8","14.7","14.6","14.5","14.4","14.3","14.2","14.1","15","16","17","18","19","20","21","22","23","24","25",
                  "26","27","28","29","30","31","32","33","34","35.9","35.8","35.7","35.6","35.5","35.4","35.3","35.2","35.1","36","37",
                  "38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59.9","59.8",
                  "59.7","59.6","59.5","59.4","59.3","59.2","59.1","60","61","62","63","64","65.9","65.8","65.7","65.6","65.5","65.4",
                  "65.3","65.2","65.1","66","67","68","69","70","71","72","73","74")
  
  a <- a[c("TimeStamp", "4.1", "14.1", "14.6", "14.7", "21", "28", "35.1", "35.7", "35.9", "37", "41", "42", "44", "49",
           "53", "54", "57", "59.1", "59.9","61", "64","65.1","65.6", "65.8","67","68", "71")]
  
  colnames(a) <- c("TimeStamp", "Vuoksi", "Kymijoki", "Saarijärven reitti", "Rautalammin reitti","Vantaanjoki",
                   "Aurajoki","Kokemäenjoki","Längelmäveden-Hauhon reitti", 
                   "Loimijoki","Lapväärtinjoki", "Laihianjoki",
                   "Kyrönjoki", "Lapuanjoki","Perhonjoki", 
                   "Kalajoki", "Pyhäjoki", "Siikajoki","Ala-Oulujoki", "Ontojärvi-Lentua","Iijoki", "Simojoki",
                   "Ala-Kemijoki","Ylä-Ounasjoki", "Kitinen", "Tornionjoki-Muonionjoki","Teno", "Paatsjoki")
  
  a$TimeStamp <- ymd(a$TimeStamp)
  a <- a[!(format(a$TimeStamp, "%m") == "02" & format(a$TimeStamp, "%d")==29), , drop = FALSE]
  a$D_M <- format(a$TimeStamp, "%m-%d")
  
  # update dataframe in the list
  list39[[i]] <- a
  
  i <- i + 1
  
}

# -------------

# 2040-69

# read data
lammin_69 <- read.table("data/suomiq_198110_204069_IPCC5_MIROC-ESM-CHEM_rcp45_climveturi.dat", sep = "", dec =".", header = FALSE,
                        colClasses = c(rep("character", 115), rep("NULL", 745)))
kylma_69 <- read.table("data/suomiq_198110_204069_IPCC5_CESM1-CAM5_rcp26_climveturi.dat", sep = "", dec =".", header = FALSE,
                       colClasses = c(rep("character", 115), rep("NULL", 745)))
ka_69 <- read.table("data/suomiq_198110_204069_IPCC5_ka_rcp45_climveturi.dat", sep = "", dec =".", header = FALSE,
                    colClasses = c(rep("character", 115), rep("NULL", 745)))

list69 <- list()
list69[["lammin_69"]] <- lammin_69
list69[["kylma_69"]] <- kylma_69
list69[["ka_69"]] <- ka_69

# Select locations, rename and create datetimes
i <- 1
for (e in list69) {
  a <- as.data.frame(sapply(e, as.numeric))
  
  colnames(a) = c("TimeStamp","1", "2","3","4.9","4.8","4.7","4.6","4.5","4.4","4.3","4.2","4.1","5","6","7","8","9","10","11","12","13",
                  "14.9","14.8","14.7","14.6","14.5","14.4","14.3","14.2","14.1","15","16","17","18","19","20","21","22","23","24","25",
                  "26","27","28","29","30","31","32","33","34","35.9","35.8","35.7","35.6","35.5","35.4","35.3","35.2","35.1","36","37",
                  "38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59.9","59.8",
                  "59.7","59.6","59.5","59.4","59.3","59.2","59.1","60","61","62","63","64","65.9","65.8","65.7","65.6","65.5","65.4",
                  "65.3","65.2","65.1","66","67","68","69","70","71","72","73","74")
  
  
  a <- a[c("TimeStamp", "4.1", "14.1", "14.6", "14.7", "21", "28", "35.1", "35.7", "35.9", "37", "41", "42", "44", "49",
           "53", "54", "57", "59.1", "59.9","61", "64","65.1","65.6", "65.8","67","68", "71")]
  
  # Nimetään vesistöalueet välttämiseksi (jos muutettu edellistä, lisätään myös nimi tähän litaniaan oikealle kohdalle)
  colnames(a) <- c("TimeStamp", "Vuoksi", "Kymijoki", "Saarijärven reitti", "Rautalammin reitti","Vantaanjoki",
                   "Aurajoki","Kokemäenjoki","Längelmäveden-Hauhon reitti", 
                   "Loimijoki","Lapväärtinjoki", "Laihianjoki",
                   "Kyrönjoki", "Lapuanjoki","Perhonjoki", 
                   "Kalajoki", "Pyhäjoki", "Siikajoki","Ala-Oulujoki", "Ontojärvi-Lentua","Iijoki", "Simojoki",
                   "Ala-Kemijoki","Ylä-Ounasjoki", "Kitinen", "Tornionjoki-Muonionjoki","Teno", "Paatsjoki")
  
  
  a$TimeStamp <- ymd(a$TimeStamp)
  a <- a[!(format(a$TimeStamp, "%m") == "02" & format(a$TimeStamp, "%d")==29), , drop = FALSE]
  a$D_M <- format(a$TimeStamp, "%m-%d")
  
  # Updating dataframes in the list
  list69[[i]] <- a
  
  i <- i + 1
  
}

# ---------
# 1 = 2010-2039, 2 = 2040-69

# empty list
scen_list <- list()

# could be done in a more effiient loop but no time
## 2010-39
# lammin 39
for (nimi in colnames(list39[["lammin_39"]])) {
  data <- list39[["lammin_39"]] %>%
    select(D_M, nimi) %>%
    group_by(D_M) %>%
    summarize_all(list(mean = mean, max = max, min = min)) %>%
    data.frame()
  name <- paste(nimi, "1","1", sep ="_")
  if (name == "TimeStamp_1_1" | name == "D_M_1_1") {
    next
  }
  data$D_M <- as.Date(data$D_M, format="%m-%d") 
  scen_list[[name]] <- data
  
}
# kylma 39
for (nimi in colnames(list39[["kylma_39"]])) {
  data <- list39[["kylma_39"]] %>%
    select(D_M, nimi) %>%
    group_by(D_M) %>%
    summarize_all(list(mean = mean, max = max, min = min)) %>%
    data.frame()
  name <- paste(nimi, "1","2", sep ="_")
  if (name == "TimeStamp_1_2" | name == "D_M_1_2") {
    next
  }
  data$D_M <- as.Date(data$D_M, format="%m-%d") 
  scen_list[[name]] <- data
  
}

for (nimi in colnames(list39[["ka_39"]])) {
  data <- list39[["ka_39"]] %>%
    select(D_M, nimi) %>%
    group_by(D_M) %>%
    summarize_all(list(mean = mean, max = max, min = min)) %>%
    data.frame()
  name <- paste(nimi, "1","3", sep ="_")
  if (name == "TimeStamp_1_3" | name == "D_M_1_3") {
    next
  }
  data$D_M <- as.Date(data$D_M, format="%m-%d") 
  scen_list[[name]] <- data
  
}

## 2040-69 = 2
# lammin 69
for (nimi in colnames(list69[["lammin_69"]])) {
  data <- list69[["lammin_69"]] %>%
    select(D_M, nimi) %>%
    group_by(D_M) %>%
    summarize_all(list(mean = mean, max = max, min = min)) %>%
    data.frame()
  name <- paste(nimi, "2","1", sep ="_")
  if (name == "TimeStamp_2_1" | name == "D_M_2_1") {
    next
  }
  data$D_M <- as.Date(data$D_M, format="%m-%d") 
  scen_list[[name]] <- data
  
}
# kylma 69
for (nimi in colnames(list69[["kylma_69"]])) {
  data <- list69[["kylma_69"]] %>%
    select(D_M, nimi) %>%
    group_by(D_M) %>%
    summarize_all(list(mean = mean, max = max, min = min)) %>%
    data.frame()
  name <- paste(nimi, "2","2", sep ="_")
  if (name == "TimeStamp_2_2" | name == "D_M_2_2") {
    next
  }
  data$D_M <- as.Date(data$D_M, format="%m-%d") 
  scen_list[[name]] <- data
  
}

#ka 69
for (nimi in colnames(list69[["ka_69"]])) {
  data <- list69[["ka_69"]] %>%
    select(D_M, nimi) %>%
    group_by(D_M) %>%
    summarize_all(list(mean = mean, max = max, min = min)) %>%
    data.frame()
  name <- paste(nimi, "2","3", sep ="_")
  if (name == "TimeStamp_2_3" | name == "D_M_2_3") {
    next
  }
  data$D_M <- as.Date(data$D_M, format="%m-%d") 
  scen_list[[name]] <- data
  
}

#### ---------------------------------------------------------------------------

# Load custom functions. Commented because this does not exist ahaha
#source(file.path(wd, "app_funcs.R"))


# Parameters
locations <- c("Vuoksi", "Kymijoki", "Saarijärven reitti", "Rautalammin reitti",
               "Vantaanjoki", "Aurajoki","Kokemäenjoki","Längelmäveden-Hauhon reitti", 
                "Loimijoki","Lapväärtinjoki", "Laihianjoki",
                "Kyrönjoki", "Lapuanjoki","Perhonjoki", 
                "Kalajoki", "Pyhäjoki", "Siikajoki","Ala-Oulujoki", "Ontojärvi-Lentua","Iijoki", "Simojoki",
                "Ala-Kemijoki","Ylä-Ounasjoki", "Kitinen", "Tornionjoki-Muonionjoki","Teno", "Paatsjoki")

timeframe_names <- c("2010-2039", "2040-2069") # 1, 2
scenario_names <- c("Lämmin ja märkä", "Kylmä", "Usean skenaarion keskiarvo") # 1, 2, 3



#varit <- c("ref_ka" = "gray21", "ref_maxmin" = "grey75", "lammin_ka" = "tan1", "lammin_maxmin" ="tan1", 
           #"kylma_ka" = "turquoise3", "kylma_maxmin" = "turquoise3", "ka45_ka" = "indianred2", "ka45_maxmin" = "indianred2")
#load("ref_list.RData")

#### ShinyApp Server -----------------------------------------------------------

server <- function(input, output){
  
  #output$tab <- renderTable({
    
    #thisName <- paste(input$location, input$timeframe, input$scenario, sep = "_")
    #message(thisName)
    #list_dfs[[thisName]]
  #})
  
  output$plo <- renderPlot({
    
    thisName <- paste(input$location, input$timeframe, input$scenario, sep = "_")
    thisPlot <- scen_list[[thisName]]
    
    plo <- ggplot(data = thisPlot, 
                  aes(x = D_M, y = mean, group = "group")) +
      geom_line(size = 1.2) +
      geom_ribbon(aes(ymin=min, ymax=max, colour = "red"),
                  fill=NA, size = 1.1, linetype = 2) +
       
      
      # MUUT ASETUKSET
      # Y ja X akselit
      scale_y_continuous(breaks = scales::pretty_breaks(n = 6))+
      
      # x-akseli, kuukausittain
      scale_x_date(expand = c(0,0),date_labels = "%b", date_breaks = "1 month")+
      # Tyyliseikat
      theme(axis.title.x=element_blank(),
            axis.text.x = element_text(size=10, face = "bold"),
            axis.text.y = element_text(size=10),
            panel.background = element_blank(),
            panel.grid.major = element_blank(),
            axis.line = element_line(colour="grey"),
            legend.spacing.y = unit(-0.4, "cm"),
            legend.position = c(1, 1.05),
            legend.justification = c("right", "top"),
            legend.box.just = "left",
            legend.margin = margin(6, 6, 6, 6),
            legend.background = element_blank(),
            legend.text = element_text(size=10),
            legend.key.size = unit(0.5, "cm"),
            legend.box.background = element_rect(alpha("white", 0.3), color =NA),
            plot.title = element_text(size=12, face="bold")) 
    
    plo
  })
}


#### ShinyApp User Interface ---------------------------------------------------
ui <- shinyUI(fluidPage(
  
  # Import CSS style from external file
  #tags$head(htmltools::includeCSS(csspath)),
  
  titlePanel(h1("TESTI"), 
             windowTitle = "ClimVeturi visualisoinnit"),
  

  ### sidebarLayout contents ---------------------------------------------------
  sidebarLayout(
    sidebarPanel(
      width = 3,
      id = "sidebar",
      
      selectInput(
        inputId = "location", 
        label = HTML("Valitse vesistöalue"),
        choices = locations),
      
      radioButtons(
        inputId = "timeframe",
        label = "Valitse ajanjakso",
        choiceNames = timeframe_names,
        choiceValues = seq(1:length(timeframe_names))
      ),
      
      radioButtons(
        inputId = "scenario",
        label = "Valitse skenaario",
        choiceNames = scenario_names,
        choiceValues = seq(1:length(scenario_names))
      ),
      
      HTML(paste("<p id='version-info' style='color: grey; font-size: small;'>App versio<br>", 
                 app_v, "</p>")),
    ),
    
    
    ### mainPanel contents -----------------------------------------------------
    mainPanel(
      width = 9,
      
      #h3("Taulukko"),
      #tableOutput("tab"),
      hr(),
      h3("Kuvaaja"),
      plotOutput("plo")
    )
  )
))


### Run ShinyApp ---------------------------------------------------------------

shinyApp(ui = ui, server = server)
