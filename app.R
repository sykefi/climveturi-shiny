# Climveturi App


# Clear workspace
rm(list = ls())

### INITIALISE -----------------------------------------------------------------


# App version
app_v <- "0007 (05.11.2020)"


# Import libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(reshape2)
library(data.table)
library(formattable)
library(shinythemes)
library(shinyjs)
library(htmltools)
library(leaflet)
library(rgdal)
library(ggiraph)
library(DT)
library(reactable)

#wd <- setwd("C:/Users/e1007642/Documents/ClimVeturi/git/shiny")

# css path
csspath <- "app_style.css"

### load data -------------------------------------------------------------

# Plots and tables
ref_list <- readRDS("data/ref_list.rds")
scen_list <- readRDS("data/scen_list.rds")
chg_dfs <- readRDS("data/chg_dfs.rds")

# Flood data
flood <- read.table("data/flood_coord_proj.txt", dec = ",", sep = "\t", header=TRUE, stringsAsFactors = FALSE, encoding = "UTF-8")
flood <- flood[,c(1,2,3,6,4,5,9,7,8,11,10)] 
flood[,c(3:9)] <- round(flood[,c(3:9)], 0)
names(flood) <- c("ID", "Nimi", "Alue", "Keskiarvo","Maksimi","Minimi", 
                  "Keskiarvo", "Maksimi","Minimi", "lat", "long")

# Create separate dataframes and append to list, use in Flood-tab to create table and map
# Selection depends on the table created when reprojecting he coordinates. 

# 2010-39 with name
flood_1_nimi <- flood[,c(2,4:6)]
flood_2_nimi <- flood[,c(2,7:9)]
# 2010-39
flood_1 <- flood[,c(2,4:6, 10,11)]
# 2040-69
flood_2 <- flood[,c(2,7:9,10,11)]

flood_list <- list(flood_1_nimi = flood_1_nimi, flood_2_nimi = flood_2_nimi, 
                   flood_1 = flood_1, flood_2 = flood_2)


# read geopackage
valuma <- readOGR("data/valuma_line.gpkg")
valuma <- spTransform(valuma, CRS("+init=epsg:4326"))



#### ---------------------------------------------------------------------------

# Parameters
locations <- c("Vuoksi", "Kymijoki", "Naarajärvi, Saarijärven reitti", "Konnevesi","Vantaanjoki",
               "Aurajoki","Kokemäenjoki, Pori","Valkeakoski, Mallasvesi",
               "Loimijoki","Lapväärtinjoki", "Laihianjoki",
               "Kyrönjoki", "Lapuanjoki","Perhonjoki",
               "Kalajoki", "Pyhäjoki", "Siikajoki","Oulujoki, Merikoski","Niemelänjärvi", "Iijoki", "Simojoki",
               "Kemijoki, Isohaara","Ounasjoki, Hossa", "Kitinen", "Tornionjoki, Tornio","Teno", "Paatsjoki, Kaitakoski") %>%
  sort()

timeframe_names <- c("2010-2039", "2040-2069") # 1, 2
scenario_names <- c("Usean skenaarion keskiarvo","Lämmin ja märkä", "Kylmä") # 1, 2, 3


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
    # Create table (33a5 is unicode for m3)
    formattable(chg_dfs[[thisName]], col.names=c("Virtaama (\u33a5/s)", "Muutos (%)"),
                list(disp = formatter("span", 
                                      style = x ~ style(
                                        font.weight = "bold")),
                     `Virtaama` = formatter("span"),
                     `Muutosprosentti` = muutos_form)
                     
                )
    
    
  })
  
  
  # Plot
  output$plo <- renderggiraph({
    
    thisName <- paste(input$location, input$timeframe, input$scenario, sep = "_")
    thisPlot <- scen_list[[thisName]]
    
    nameRef <- paste(input$location, "ref", sep ="_")
    thisRefPlot <- ref_list[[nameRef]]
    
    cols <- c("ref1" = "grey40",
              "ref2" = "grey75",
              "1" = "indianred2", 
              "2" = "tan1", 
              "3" = "turquoise3")
    
    scens <- c("1" = "Usean skenaarion keskiarvo (RCP4.5 päästöskenaariolla)",
               "2" = "Lämmin ja märkä (MIROC-ESM-CHEM globaali ilmastomalli RCP4.5 päästöskenaariolla)",
               "3" = "Kylmä (CESM1-CAM5 globaali ilmastomalli RCP2.6 päästöskenaariolla)")
    
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
      geom_line(aes(y = mean, colour = "ref1"), size = 1.4, alpha = 0.8) +
      
      # Changes when input changes
      geom_line(data=thisPlot, aes(y = mean, colour = as.character(input$scenario), group = 1),
                size = 1.4, alpha = 0.8) +
      geom_ribbon(data=thisPlot, aes(ymin = min, ymax = max, colour = as.character(input$scenario), group = 1),linetype = 3,
                  fill = NA, size = 1.2, alpha = 0.8) +
      
      
      
      # x-axis
      scale_x_date(expand = c(0,0),date_labels = "%b", date_breaks = "1 month")+
      
      
      # colour & legend settings
      scale_colour_manual(name = " ", values = cols,
                          breaks = c("ref1", as.character(input$scenario)),
                          labels = c("1981-2010 simuloitu keskiarvo",
                                     paste(times[input$timeframe], "simuloitu keskiarvo ja vaihteluväli", sep = " "))) +
      scale_fill_manual(name = " ", values = cols,
                        breaks = c("ref2"),
                        labels = c("1981-2010 simuloitu vaihteluväli (max-min)")) +
      
      guides(colour = guide_legend(nrow= 2,override.aes = list(linetype=c(1,1),
                                                       shape = c(16, 16)))) +
      
      
      # Style settings
      theme(axis.title.x=element_blank(),
            axis.text.x = element_text(size=20, face = "bold"),
            axis.text.y = element_text(size=20),
            axis.title.y = element_text(size = 20),
            panel.background = element_blank(),
            #panel.grid.major.y = element_line(colour="grey70"),
            axis.line = element_line(colour="grey"),
            legend.position ="bottom",
            legend.justification = c("left", "top"),
            legend.margin = margin(),
            legend.background = element_blank(),
            legend.text = element_text(size=20),
            legend.box.background = element_rect(alpha("white", 0.3), color =NA),
            plot.title = element_text(size=25))
    
    
    
    # copy to global environment for saving
    plo_out <<-
      plo +
      theme(legend.text = element_text(size = 16),
            axis.text = element_text(size = 18),
            axis.title = element_text(size = 18))
    
    # display plot
    ggiraph(code = print(plo),
            width_svg = 16.7,
            height_svg = 9)
    
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
  
  # Map for page 1: locations
  output$map_1 <- renderLeaflet({
    
    
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron,
                       option=leafletOptions(minZoom = 5, maxZoom = 8)) %>%
      addCircles(data = flood_1, lng = ~lat, lat = ~long,
                 weight = 10, fill = TRUE, label = ~htmlEscape(Nimi),
                 labelOptions = labelOptions(textsize = "12px")) 

      # addPolylines(data = valuma,
      #             color = "#e14747",
      #             weight = 1,
      #             smoothFactor = 0.5)
    
    # to do: add legend
    
    
  })
  
  # Highlight the input location on map
  observe({
    thisPoint <- subset(flood_1, flood_1$Nimi == input$location)

    leafletProxy(mapId = "map_1") %>%
      clearGroup("highlighted_point") %>%
      addCircleMarkers(data = thisPoint, lng=~lat, lat=~long,
                       color = "yellow", group = "highlighted_point",
                       label = ~htmlEscape(Nimi),
                       labelOptions = labelOptions(textsize = "12px"))
  })
  
### Second tab with floods  ---------------------
  
  
  # output$table2 <- renderDT({
  #   
  #   tableData <- paste("flood", input$timeframe2, "nimi", sep="_")
  #  
  #   muutos_form <- formatter("span",
  #                            style = x ~ style(
  #                              font.weight = "bold",
  #                              color = ifelse(x >= 20, "#b2182b",
  #                                             ifelse(x < 20 & x >= 10, "#ef8a62",
  #                                                    ifelse(x < 10 & x >=0, "#ffce99",
  #                                                           ifelse(x < 0 & x >= -10, "#b3d9ff",
  #                                                                  ifelse(x < -10 & x >= -20, "#67a9cf",
  #                                                                         ifelse(x <-20, "#2166ac", "black"))))))),
  #                            x ~ icontext(ifelse(x>0, "arrow-up", "arrow-down"), x)
  #   )
  #   
  #   as.datatable(formattable(flood_list[[tableData]],
  #               list(disp = formatter("span", 
  #                                     style = x ~ style(
  #                                       font.weight = "bold")),
  #                    area(col = 2:4) ~ muutos_form)
  #                    
  #                   
  #               ), 
  #               selection ="multiple", escape=FALSE, 
  #               options = list(sDom  = '<"top">lrt<"bottom">ip', pageLength= 27, lengthChange = FALSE,
  #                              columnDefs = list(list(targets = c(0), type = "num-fmt"))),
  #               rownames = FALSE)
  #   
  #   
  # })
  
  # Flood table made with reactable https://glin.github.io/reactable/ v. 0.2.3
  output$table2 <- renderReactable({
    tableData <- paste("flood", input$timeframe2, "nimi", sep="_")
    
    reactable(flood_list[[tableData]],
              height = 600,
              pagination = FALSE,
              highlight = TRUE,
              
              columns = list(
                Nimi = colDef(
                  name = "Vesistö",
                  width = 230
                ),
                
                Keskiarvo = colDef(
                  name = "Keskiarvo (%)",
                  cell = function(value) {
                    if (value >= 0) paste0("+", value) else value
                  },
                  style = function(value) {
                    if (value >= 20) {
                    color <- "#b2182b"
                  } else if (value < 20 & value >= 10) {
                    color <- "#ef8a62"
                  } else if (value < 10 & value >=0) {
                    color <- "#ffce99"
                  } else if (value < 0 & value >= -10) {
                    color <- "#b3d9ff"
                  } else if (value < -10 & value >= -20) {
                    color <- "#67a9cf"
                  } else {
                    color <- "#2166ac"
                  }
                  list(color = color, fontWeight = "bold")
                  
                }),
                Maksimi = colDef(
                  name = "Maksimi (%)",
                  cell = function(value) {
                    if (value >= 0) paste0("+", value) else value
                  },
                  style = function(value) {
                  if (value >= 20) {
                    color <- "#b2182b"
                  } else if (value < 20 & value >= 10) {
                    color <- "#ef8a62"
                  } else if (value < 10 & value >=0) {
                    color <- "#ffce99"
                  } else if (value < 0 & value >= -10) {
                    color <- "#b3d9ff"
                  } else if (value < -10 & value >= -20) {
                    color <- "#67a9cf"
                  } else {
                    color <- "#2166ac"
                  }
                  list(color = color, fontWeight = "bold")
                }) ,
                
                Minimi = colDef(
                  name = "Minimi (%)",
                  cell = function(value) {
                    if (value >= 0) paste0("+", value) else value
                  },
                  style = function(value) {
                  if (value >= 20) {
                    color <- "#b2182b"
                  } else if (value < 20 & value >= 10) {
                    color <- "#ef8a62"
                  } else if (value < 10 & value >=0) {
                    color <- "#ffce99"
                  } else if (value < 0 & value >= -10) {
                    color <- "#b3d9ff"
                  } else if (value < -10 & value >= -20) {
                    color <- "#67a9cf"
                  } else {
                    color <- "#2166ac"
                  }
                  list(color = color, fontWeight = "bold")
                })
                
              ),
              
              )
    
    
  }) 
  
  
  # Map for page 2
  output$map_2 <- renderLeaflet({
    
    # I want to make a map with radio buttons where user can select which column is visualized
    # And the radius and colour is based on the value.
    # Also filtering option (map_click)?
    
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron,
                       option=leafletOptions(minZoom = 5, maxZoom = 8)) %>%
      addCircles(data = flood_1, lng = ~lat, lat = ~long,
                 weight = 10, fill = TRUE, label = ~htmlEscape(Nimi),
                 labelOptions = labelOptions(textsize = "12px")) %>%
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
  
  tags$head(tags$link(rel = "stylesheet", 
                      type = "text/css", 
                      href = "https://use.fontawesome.com/releases/v5.13.0/css/all.css"),
            htmltools::includeCSS(csspath)),
  
  
  headerPanel(
    title=tags$a(href='https://www.syke.fi/fi-FI',tags$img(src='SYKE_tunnus_rgb_vaaka.png', 
                                                           height = 50*0.75, width = 182*0.75), target="_blank"),
    windowTitle = "ClimVeTuri ilmastonmuutos"
  ),
  
  # Import CSS style from external file
  #tags$head(htmltools::includeCSS(csspath)),
  
  titlePanel(h4("Ilmastonmuutoksen vaikutus vesistöihin -visualisointityökalu")),
  
  # First tab #########
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
                   column(12,
                          fluidRow(
                            column(7,
                                   h5("Kuvaaja"),
                                   downloadButton("kuvaaja_lataus",
                                                  label = HTML("<i class='icon file' title='Lataa kuvaaja (png)'></i>")),
                                   ggiraphOutput("plo", 
                                                 width = "100%",
                                                 height = "100%"),
                                   
                                   h5("Taulukko"),
                                   formattableOutput("table",width = 400)),
                                   
                          column(4,
                                 "Mallinnettujen virtaamapisteiden sijainti kartalla",
                                 leafletOutput("map_1", height = 750))
                          ))
                   
                 )),
             )
    ),
    # Second tab ############
    tabPanel("Muutokset tulvissa", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 width = 2,
                 
                 helpText("Visualisoi ilmastonmuutoksen vaikutuksia kerran sadassa vuodessa (1/100a) esiintyviin tulviin eri ajanjaksoilla."),
                
                 radioButtons(
                   inputId = "timeframe2",
                   label = "Valitse ajanjakso",
                   choiceNames = timeframe_names,
                   choiceValues = seq(1:length(timeframe_names))
                 )
               ),
               
               mainPanel(
                 fluidRow(
                   column(12,
                          fluidRow(
                          column(7,
                                 br(),
                                 strong("Kuinka paljon keskimäärin kerran sadassa vuodessa tapahtuva tulvan (1/100a) arvioidaan muuttuvan ilmastonmuutoksen vaikutuksesta?"),
                                 p(" "),
                                 p("Taulukkoon ja karttaan on arvioitu 25 eri ilmastonmuutosskenaarion avulla, kuinka paljon 100-vuoden avovesitulva muuttuu enintään, vähintään ja keskimäärin valitulla ajanjaksolla suhteessa referenssijaksoon (1981-2010). Luvut ovat prosentteja (%)"),
                                 
                                 reactableOutput("table2",width = 650)),
                          
                          column(4,
                                 " ",
                                 leafletOutput("map_2", height=750))))
                 )),
               
             )
    ),
    # Third tab ##############
    tabPanel("Lisätietoa",
             sidebarLayout(
               sidebarPanel(
                 width = 2,
                 
                 strong("Tällä sivulla:"),
                 em("taustaa, lisätietoa, yhteystiedot ja palaute."),
                 helpText("Tietoa hankkeesta: ", 
                          tags$a(href= "https://www.syke.fi/fi-FI/Tutkimus__kehittaminen/Tutkimus_ja_kehittamishankkeet/Hankkeet/ClimVeTuri",
                                 "ClimVeTuri", target="_blank")),
                 
               ),
             
             mainPanel(
               fluidRow(
                 column(8,
                        includeMarkdown('userguide/user_guide.rmd'))
                 ))
    )
    
    
  ))
))









### Run ShinyApp ---------------------------------------------------------------

shinyApp(ui = ui, server = server)