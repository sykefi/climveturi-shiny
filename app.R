# Climveturi App
# 19.10.2020, 21.10, 22.10, 26.10, 29.10



# Clear workspace
rm(list = ls())

### INITIALISE -----------------------------------------------------------------

#### Working paths ####
wd <- getwd()

# Component paths. Commented because this does not exist ahaha
#csspath <- file.path(wd, "app_style.css")

# App version
app_v <- "0004 (28.10.2020)"


# Import libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
library(reshape2)
library(data.table)
library(formattable)
library(shinythemes)
library(shinyjs)
library(htmltools)
library(leaflet)
library(leaflet.minicharts)
library(rgdal)
library(DT)



setwd("C:/Users/e1007642/Documents/ClimVeturi/git/shiny")

### load data -------------------------------------------------------------


# Plots and tables
ref_list <- readRDS("data/ref_list.rds")
scen_list <- readRDS("data/scen_list.rds")
chg_dfs <- readRDS("data/chg_dfs.rds")

# Flood data
flood <- read.table("data/flood_coord_proj.txt", dec = ",", sep = "\t", header=TRUE, stringsAsFactors = FALSE)
flood <- flood[,c(1,2,3,6,4,5,9,7,8,11,10)] 
flood[,c(3:9)] <- round(flood[,c(3:9)], 2)
names(flood) <- c("ID", "Vesistö", "Alue", "Keskiarvo 2010-2039","Maksimi 2010-2039","Minimi 2010-2039", 
                  "Keskiarvo 2040-2069", "Maksimi 2040-2069","Minimi 2040-2069", "lat", "long")

# Create separate dataframes and append to list, use in Flood-tab to create table and map
# Selection depends on the table created when reprojecting he coordinates. 
# Here columns: 1 = ID, 2 = Vesisto (names), 3 = numbered codes for areas,
# 4-6 = 2010-39 mean max min, 7-9 = 2040-69 mean max min, 10-11 = long lat
flood_1_nimi <- flood[,c(2,4:6)]
flood_2_nimi <- flood[,c(2,7:9)]
flood_1 <- flood[,c(2,4:6, 10,11)]
flood_2 <- flood[,c(2,7:9,10,11)]
flood_list <- list(flood_1_nimi = flood_1_nimi, flood_2_nimi = flood_2_nimi, 
                   flood_1 = flood_1, flood_2 = flood_2)

# read geopackage
valuma <- readOGR("data/valuma.gpkg")
valuma <- spTransform(valuma, CRS("+init=epsg:4326"))



#### ---------------------------------------------------------------------------

# Parameters
locations <- c("Vuoksi", "Kymijoki", "Naarajärvi", "Konnevesi","Vantaanjoki",
               "Aurajoki","Kokemäenjoki Pori","Valkeakoski Mallasvesi",
               "Loimijoki","Lapväärtinjoki", "Laihianjoki",
               "Kyrönjoki", "Lapuanjoki","Perhonjoki",
               "Kalajoki", "Pyhäjoki", "Siikajoki","Ala-Oulujoki",
               "Niemelänjärvi", "Iijoki", "Simojoki",
               "Kemijoki Isohaara","Ounasjoki Hossa", "Kitinen", 
               "Tornionjoki-Muonionjoki","Teno", "Paatsjoki") %>%
  sort()

timeframe_names <- c("2010-2039", "2040-2069") # 1, 2
scenario_names <- c("Lämmin ja märkä", "Kylmä", "Usean skenaarion keskiarvo") # 1, 2, 3


#### ShinyApp Server -----------------------------------------------------------

server <- function(input, output){
  
### First tab with discharges  ------------
  
  # Table with % changes
  output$table <- renderFormattable({
    
    thisName <- paste(input$location, input$timeframe,
                      input$scenario, "%", sep = "_")
    # Set custom colours for % thresholds
    muutos_form <- formatter("span", 
                             style = x ~ style(
                               font.weight = "bold",
                               color = ifelse(x >= 20, "#b2182b",
                                              ifelse(x < 20 & x >= 10, "#ef8a62",
                                                     ifelse(x < 10 & x >=0, "#ffce99",
                                                            ifelse(x < 0 & x >= -10, "#b3d9ff",
                                                                   ifelse(x < -10 & x >= -20, "#67a9cf",
                                                                          ifelse(x <-20, "#2166ac", "black"))))))),
                             x ~ icontext(ifelse(x>0, "arrow-up", "arrow-down"), x)
    )
    # Create table
    formattable(chg_dfs[[thisName]],
                list(disp = formatter("span", 
                                      style = x ~ style(
                                        font.weight = "bold")),
                     `Virtaama` = formatter("span"),
                     `Muutosprosentti` = muutos_form
                ))
    
    
  })
  
  
  # Plot
  output$plo <- renderPlot({
    
    thisName <- paste(input$location, input$timeframe, input$scenario, sep = "_")
    thisPlot <- scen_list[[thisName]]
    
    nameRef <- paste(input$location, "ref", sep ="_")
    thisRefPlot <- ref_list[[nameRef]]
    
    cols <- c("ref1" = "grey40",
              "ref2" = "grey75",
              "1" = "tan1", 
              "2" = "turquoise3", 
              "3" = "indianred2")
    
    scens <- c("1" = "Lämmin ja märkä (MIROC-ESM-CHEM globaali ilmastomalli RCP4.5 päästöskenaariolla)",
               "2" = "Kylmä (CESM1-CAM5 globaali ilmastomalli RCP2.6 päästöskenaariolla)",
               "3" = "Usean skenaarion keskiarvo (RCP4.5 päästöskenaariolla)")
    
    times <- c("1" = "2010-2039",
               "2" = "2040-2069")
    
    
    
    plo <- ggplot(
      data = thisRefPlot,aes(x = D_M, y = mean,  group = "group")) +
      labs(title= paste(input$location,
                        "\nAjanjakso: ", times[input$timeframe],
                        "\nSkenaario: ", scens[input$scenario]),
           y = expression(paste("Virtaama (", m^3,"/s)", sep=""))) +
      
      # Control period ribbom + geom line in all of the plots
      geom_ribbon(aes(ymin=min, ymax=max, fill = "ref2"), 
                  colour = NA, alpha = 0.5) +
      geom_line(aes(y = mean, colour = "ref1"), size = 1.2, alpha = 0.8) +
      
      # Changes when input changes
      geom_line(data=thisPlot, aes(y = mean, colour = as.character(input$scenario), group = 1),
                size = 1.2, alpha = 0.8) +
      geom_ribbon(data=thisPlot, aes(ymin = min, ymax = max, colour = as.character(input$scenario), group = 1),linetype = 3,
                  fill = NA, size = 1.1, alpha = 0.8) +
      
      
      # x-axis
      scale_x_date(expand = c(0,0),date_labels = "%b", date_breaks = "1 month")+
      
      # colour & legend settings
      scale_colour_manual(name = " ", values = cols,
                          breaks = c("ref1", as.character(input$scenario)),
                          labels = c("1981-2010 simuloitu keskiarvo",
                                     paste(times[input$timeframe], "Simuloitu keskiarvo ja vaihteluväli", sep = " "))) +
      scale_fill_manual(name = " ", values = cols,
                        breaks = c("ref2"),
                        labels = c("1981-2010 simuloitu vaihteluväli (max-min)")) +
      
      guides(colour = guide_legend(override.aes = list(linetype=c(1,1),
                                                       shape = c(16, 16)))) +
      
      
      # Style settings
      theme(axis.title.x=element_blank(),
            axis.text.x = element_text(size=12, face = "bold"),
            axis.text.y = element_text(size=12),
            panel.background = element_blank(),
            #panel.grid.major.y = element_line(colour="grey70"),
            axis.line = element_line(colour="grey"),
            legend.position ="bottom",
            legend.justification = c("left", "top"),
            legend.margin = margin(6, 6, 6, 6),
            legend.background = element_blank(),
            legend.text = element_text(size=11),
            legend.box.background = element_rect(alpha("white", 0.3), color =NA),
            plot.title = element_text(size=14)) 
    
    # copy to global environment for saving
    plo_out <<- plo
    
    # display plot
    plo
    
  })
  
  # Download button for plot
  output$kuvaaja_lataus <- downloadHandler(
    filename = function() {
      paste("kuvaaja_",input$location, "_", input$timeframe, "_", 
            input$scenario, ".png",
            sep = "")
    },
    
    content = function(file) {
      ggsave(plot = plo_out, file, height = 10, width = 16, dpi = 150)
    }
  )
  
### Second tab with floods  ---------------------
  
  
  output$table2 <- renderDT({
    
    tableData <- paste("flood", input$timeframe2, "nimi", sep="_")
   
    muutos_form <- formatter("span",
                             style = x ~ style(
                               font.weight = "bold",
                               color = ifelse(x >= 20, "#b2182b",
                                              ifelse(x < 20 & x >= 10, "#ef8a62",
                                                     ifelse(x < 10 & x >=0, "#ffce99",
                                                            ifelse(x < 0 & x >= -10, "#b3d9ff",
                                                                   ifelse(x < -10 & x >= -20, "#67a9cf",
                                                                          ifelse(x <-20, "#2166ac", "black"))))))),
                             x ~ icontext(ifelse(x>0, "arrow-up", "arrow-down"), x)
    )
    
    as.datatable(formattable(flood_list[[tableData]],
                list(disp = formatter("span", 
                                      style = x ~ style(
                                        font.weight = "bold")),
                     area(col = 2:4) ~ muutos_form)
                     
                    
                ), selection="multiple", escape=FALSE, 
                options = list(sDom  = '<"top">lrt<"bottom">ip', pageLength= 27, lengthChange = FALSE), 
                rownames = FALSE)
    
    
  })
  # Basemap
  output$map <- renderLeaflet({
    
    # I want to make a map with radio buttons where user can select which column is visualized
    # And the radius and colour is based on the value.
    # Also filterin option (map_click)?
    
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircles(data = flood_1, lng = ~lat, lat = ~long,
                 weight = 10, fill = TRUE) %>%
      addPolygons(data = valuma,
                  color = "#e14747",
                  weight = 1,
                  smoothFactor = 0.5,
                  fill = FALSE)
    
    
      
    
  })
  
  # Change barplots when timeframe changes
  # Edit so that default is 2010-39, now default is nothing
  # maybe checkbox?

  # observeEvent(input$timeframe2, {
  # 
  #   mapData <- flood_list[[paste("flood", input$timeframe2, sep="_")]]
  # 
  #   colors <- c("#ebdc87", "#ef8a62", "#67a9cf")
  # 
  #   leafletProxy("map", data = mapData) %>%
  #     
  #     addMinicharts(mapData$lat, mapData$long,
  #                   type = "bar",
  #                   chartdata = mapData[,c(2:4)],
  #                   colorPalette = colors,
  #                   width = 50,
  #                   height = 60)
  # })


}


#### ShinyApp User Interface ---------------------------------------------------
ui <- shinyUI(fluidPage(
  useShinyjs(),
  theme = shinytheme("flatly"),
  
  
  headerPanel(
    title=tags$a(href='https://www.syke.fi/fi-FI',tags$img(src='SYKE_tunnus_rgb_vaaka.png', 
                                                           height = 50*0.75, width = 182*0.75), target="_blank"),
    windowTitle = "ClimVeTuri ilmastonmuutos"
  ),
  
  # Import CSS style from external file
  #tags$head(htmltools::includeCSS(csspath)),
  
  titlePanel(h4("Ilmastonmuutoksen vaikutus vesistöihin -visualisointityökalu")),
  
  tabsetPanel(
    tabPanel("Muutokset virtaamissa", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 width = 2,
                 id = "sidebar",
                 
                 helpText("Visualisoi ilmastonmuutoksen vaikutuksia vesistöjen virtaamiin eri ajanjaksoilla ja skenaarioilla."),
                 
                 
                 selectInput(
                   inputId = "location", 
                   label = HTML("Valitse vesistö"),
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
                 
                 HTML(paste("<p id='version-info' style='color: grey; font-size: small;'>Versio<br>", 
                            app_v, "</p>")),
               ),
               
               mainPanel(
                 
                 fluidRow(
                   column(8,
                          h5("Kuvaaja")),
                   column(8,downloadButton("kuvaaja_lataus",
                                           label = HTML("<i class='icon file' title='Lataa kuvaaja (png)'></i>"))),
                   column(8, plotOutput("plo")),
                   br(),
                   #column(12, h5("Muutokset virtaamissa suhteessa referenssijaksoon (1981-2010) valitulla ajanjaksolla ja skenaariolla (%)")),
                   column(12,  formattableOutput("table",width = 400))
                 )),
             )
    ),
    
    tabPanel("Muutokset tulvissa", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 width = 2,
                 
                 helpText("Visualisoi ilmastonmuutoksen vaikutuksia kerran sadassa vuodessa esiintyviin tulviin eri ajanjaksoilla."),
                
                 radioButtons(
                   inputId = "timeframe2",
                   label = "Valitse ajanjakso",
                   choiceNames = timeframe_names,
                   choiceValues = seq(1:length(timeframe_names))
                 )
               ),
               
               mainPanel(
                 fluidRow(
                   
                   column(6,
                          br(),
                          strong("Kuinka paljon kerran sadassa vuodessa tapahtuva tulva muuttuu ilmastonmuutoksen vaikutuksesta?"),
                          p("Taulukkoon ja karttaan on arvioitu 25 eri ilmastonmuutosskenaarion avulla, kuinka paljon 100-vuoden tulva muuttuu enintaan, vähintään ja keskimäärin valitulla ajanjaksolla suhteessa referenssijaksoon (1981-2010). Luvut ovat prosentteja (%)"),
                          
                          DTOutput("table2",width = 400)),
                   
                   column(6,
                          leafletOutput("map", height=750))
                 )),
               
             )
    ),
    
    tabPanel("Ohje ja taustaa",
             sidebarLayout(
               sidebarPanel(
                 width = 2,
                 
                 strong("Tällä sivulla:"),
                 em("taustaa, sovelluksen käyttöohje, yhteystiedot ja palaute."),
                 helpText("Tietoa hankkeesta: ", 
                          tags$a(href= "https://www.syke.fi/fi-FI/Tutkimus__kehittaminen/Tutkimus_ja_kehittamishankkeet/Hankkeet/ClimVeTuri",
                                 "ClimVeTuri", target="_blank")),
                 
               ),
             
             mainPanel(
               fluidRow(
                 column(8,
                        includeMarkdown('./user-guide/userguide.rmd'))))
    )
    
    
  ))
))









### Run ShinyApp ---------------------------------------------------------------

shinyApp(ui = ui, server = server)