# Climveturi App


# Clear workspace
rm(list = ls())

### INITIALISE -----------------------------------------------------------------


# App version
app_v <- "0010 (19.11.2020)"

# Import libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(reshape2)
library(data.table)
library(shinythemes)
library(shinyjs)
library(htmltools)
library(leaflet)
library(rgdal)
library(ggiraph)
library(DT)
library(reactable)
library(stringr)
library(tippy)
library(shinyBS)
library(crosstalk)

#wd <- setwd("C:/Users/e1007642/Documents/ClimVeturi/git/shiny")


# css path
csspath <- "app_style.css"

### Load & wrangle data ------------------------------------------------------------------

# Plots and tables
ref_list <- readRDS("data/ref_list.rds")
scen_list <- readRDS("data/scen_list.rds")
chg_dfs <- readRDS("data/chg_dfs.rds")

# Flood data
flood <- read.table("data/flood_coord_proj.txt", dec = ",", sep = "\t", 
                    header=TRUE, stringsAsFactors = FALSE, encoding = "UTF-8")
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

# Create absolute variables for mapping radius in leaflet
flood_1$Abs_mean <- abs(flood_1$Keskiarvo)
flood_2$Abs_mean <- abs(flood_2$Keskiarvo)
flood_1$Abs_max <- abs(flood_1$Maksimi)
flood_2$Abs_max <- abs(flood_2$Maksimi)
flood_1$Abs_min <- abs(flood_1$Minimi)
flood_2$Abs_min <- abs(flood_2$Minimi)

# Append to list
flood_list <- list(flood_1_nimi = flood_1_nimi, flood_2_nimi = flood_2_nimi, 
                   flood_1 = flood_1, flood_2 = flood_2)

# read geopackage
# valuma <- readOGR("data/valuma_line.gpkg")
# valuma <- spTransform(valuma, CRS("+init=epsg:4326"))


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
floodmap_names <- c("Keskiarvo (%)", "Maksimi (%)", "Minimi (%)")


#### ShinyApp Server -----------------------------------------------------------

server <- function(input, output, session){
  
  
  
### First tab with discharges  ------------
  
  # Table with changes in mean made with reactable https://glin.github.io/reactable/ v. 0.2.3
  output$table1 <- renderReactable({
    thisName <- paste(input$location, input$timeframe,
                      input$scenario, "%", sep = "_")
    
    chg_dfs[[thisName]] <- chg_dfs[[thisName]][, c("Virtaama_ref", "Virtaama_ilm", "Muutos")]
    reactable(chg_dfs[[thisName]],
              pagination = FALSE,
              highlight = FALSE,
              sortable = FALSE,
              
              columns = list(
                Virtaama_ref = colDef(
                  name = "Virtaama (\u33a5/s) referenssijakso",
                  
                ),
                
                Virtaama_ilm = colDef(
                  name = "Virtaama (\u33a5/s) ilmastonmuutos",
                  style = function(value) {
                  list(fontWeight = "bold")
                    }),
                
                Muutos = colDef(
                  name = "Muutos",
                  cell = function(value) {
                    if (value >= 0) paste0("+", value, " %") else paste0(value, " %")
                  },
                  style = function(value) {
                    if (value >= 20) {
                      color <- "#b2182b"
                    } else if (value < 20 & value >= 10) {
                      color <- "#ef8a62"
                    } else if (value < 10 & value >=0) {
                      color <- "#FFC999"
                    } else if (value < 0 & value >= -10) {
                      color <- "#90D4E7"
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
    
    m_labels <- c("tammi", "helmi", "maalis", "huhti", "touko", "kesä", "heinä", "elo", "syys", "loka", "marras", "joulu")
    m_breaks <- c("2020-01-01", "2020-02-01","2020-03-01","2020-04-01","2020-05-01",
                  "2020-06-01","2020-07-01","2020-08-01","2020-09-01","2020-10-01",
                  "2020-11-01","2020-12-01")
    
    
   
    
    plo <- ggplot(
      data = thisRefPlot,aes(x = D_M, y = mean,  group = "group")) +
      labs(title= paste(input$location,
                        "\nAjanjakso: ", times[input$timeframe],
                        "\nSkenaario: ", scens[input$scenario]),
           y = expression(paste("Virtaama (", m^3,"/s)", sep=""))) +
      
      # Control period ribbom + geom line in all of the plots
      geom_ribbon(aes(ymin=min, ymax=max, fill = "ref2"), 
                  colour = NA, alpha = 0.5) +
      geom_line(aes(y = mean, colour = "ref1"), size = 1.6, alpha = 0.8) +
      
      # Changes when input changes
      geom_line(data=thisPlot, aes(y = mean, colour = as.character(input$scenario), group = 1),
                size = 1.6, alpha = 0.8) +
      geom_ribbon(data=thisPlot, aes(ymin = min, ymax = max, colour = as.character(input$scenario), group = 1),linetype = 3,
                  fill = NA, size = 1.4, alpha = 0.8) +
      
      
      
      # x-axis
      scale_x_date(expand = c(0,0),labels = m_labels, breaks = as.Date(m_breaks))+
      
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
            axis.text.x = element_text(size=25, face = "bold"),
            axis.text.y = element_text(size=25),
            axis.title.y = element_text(size = 25),
            panel.background = element_blank(),
            #panel.grid.major.y = element_line(colour="grey70"),
            axis.line = element_line(colour="grey"),
            legend.position ="bottom",
            legend.justification = c("left", "top"),
            legend.margin = margin(),
            legend.background = element_blank(),
            legend.text = element_text(size=30),
            legend.box.background = element_rect(alpha("white", 0.3), color =NA),
            plot.title = element_text(size=28))
    
    
    
    # copy to global environment for saving
    plo_out <<-
      plo +
      theme(legend.text = element_text(size = 16),
            axis.text = element_text(size = 18),
            axis.title = element_text(size = 18))
    
    # display plot
    ggiraph(code = print(plo),
            width_svg = 16.7,
            height_svg = 11)
    
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
  output$map1 <- renderLeaflet({
    
    
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron,
                       option=leafletOptions(minZoom = 5, maxZoom = 8)) %>%
      addCircleMarkers(data = flood_1, lng = ~lat, lat = ~long,
                 weight = 1,
                 radius = 5,
                 color = "#3275B8",
                 fillOpacity = 0.8,
                 stroke = FALSE,
                 label = ~htmlEscape(Nimi),
                 labelOptions = labelOptions(textsize = "14px"),
                 layerId = locations) 
      # addPolylines(data = valuma,
      #             color = "#e14747",
      #             weight = 1,
      #             smoothFactor = 0.5)

    
  })
  
  # Highlight the input location on map
  observe({
    thisPoint <- subset(flood_1, flood_1$Nimi == input$location)

    leafletProxy(mapId = "map1") %>%
      clearGroup("highlighted_point") %>%
      addCircleMarkers(data = thisPoint, lng=~lat, lat=~long,
                       color = "#275A90", group = "highlighted_point",
                       label = ~htmlEscape(Nimi),
                       labelOptions = labelOptions(textsize = "14px")
                       )
  })
  
  # Update the location selectInput on map click (https://www.r-bloggers.com/2016/03/r-shiny-leaflet-using-observers/)
  
  observeEvent(input$map1_marker_click, { 
    p <- input$map1_marker_click
    if(!is.null(p$id)){
      if(is.null(input$location) || input$location!=p$id) updateSelectInput(session, "location", selected=p$id)
    }
  })
  
  
  
### Second tab with floods  ---------------------
  
  
  # Flood table made with reactable https://glin.github.io/reactable/ v. 0.2.3
  output$table2 <- renderReactable({
    tableData <- paste("flood", input$timeframe2, "nimi", sep="_")
    
    reactable(SharedData$new(flood_list[[tableData]], group ="floods"),
              height = 600,
              pagination = FALSE,
              highlight = TRUE,
              defaultSortOrder = "desc",
              
              columns = list(
                Nimi = colDef(
                  name = "Vesistö",
                  width = 200
                ),
                
                Keskiarvo = colDef(
                  name = "Keskiarvo",
                  cell = function(value) {
                    if (value >= 0) paste0("+", value, " %") else paste0(value, " %")
                  },
                  style = function(value) {
                    if (value >= 20) {
                    color <- "#b2182b"
                  } else if (value < 20 & value >= 10) {
                    color <- "#ef8a62"
                  } else if (value < 10 & value >=0) {
                    color <- "#FFC999"
                  } else if (value < 0 & value >= -10) {
                    color <- "#90D4E7"
                  } else if (value < -10 & value >= -20) {
                    color <- "#67a9cf"
                  } else {
                    color <- "#2166ac"
                  }
                  list(color = color, fontWeight = "bold")
                  
                }),
                Maksimi = colDef(
                  name = "Maksimi",
                  cell = function(value) {
                    if (value >= 0) paste0("+", value, " %") else paste0(value, " %")
                  },
                  style = function(value) {
                  if (value >= 20) {
                    color <- "#b2182b"
                  } else if (value < 20 & value >= 10) {
                    color <- "#ef8a62"
                  } else if (value < 10 & value >=0) {
                    color <- "#FFC999"
                  } else if (value < 0 & value >= -10) {
                    color <- "#90D4E7"
                  } else if (value < -10 & value >= -20) {
                    color <- "#67a9cf"
                  } else {
                    color <- "#2166ac"
                  }
                  list(color = color)
                }) ,
                
                Minimi = colDef(
                  name = "Minimi",
                  cell = function(value) {
                    if (value >= 0) paste0("+", value, " %") else paste0(value, " %")
                  },
                  style = function(value) {
                  if (value >= 20) {
                    color <- "#b2182b"
                  } else if (value < 20 & value >= 10) {
                    color <- "#ef8a62"
                  } else if (value < 10 & value >=0) {
                    color <- "#FFC999"
                  } else if (value < 0 & value >= -10) {
                    color <- "#90D4E7"
                  } else if (value < -10 & value >= -20) {
                    color <- "#67a9cf"
                  } else {
                    color <- "#2166ac"
                  }
                  list(color = color)
                })
                
              ),
              
              )
    
    
  }) 
  
  
  # Map for page 2
  output$map2 <- renderLeaflet({
    mapData <- paste("flood", input$timeframe2, sep="_")
    

    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron,
                       option=leafletOptions(minZoom = 5, maxZoom = 8)) %>%
      # Default is mean
      addCircleMarkers(data = flood_list[[mapData]], lng = ~lat, lat = ~long,
                 weight = 1,
                 #radius = ~flood_list[[mapData]][[col_name()]],
                 stroke = FALSE,
                 radius = 5,
                 color = "#3275B8",
                 fillOpacity = 0.8,
                 label = ~htmlEscape(Nimi),
                 labelOptions = labelOptions(textsize = "14px"),
                 #color = ~pal(flood_list[[mapData]][[col_name()]]),
                 group = "markers") 
      

    
  })
  
  # # Highlight row on table based on map click event
  # observeEvent(input$map2_marker_click, { 
  #   f <- input$map1_marker_click
  #   if(!is.null(f$id)){
  #     if(is.null(input$location) || input$location!=f$id) 
  #       updateReactable("table2", selected=f$id,)
  #   }
  # })
  
  # Observe input and change the visualized column NEEDS MODIFICATION
  map_proxy <- leafletProxy("map2")
  observeEvent(input$floodMap, {
    
    bins <- c(50, 0, -50)
    cols <- c("#3275B8", "#b2182b")
    pal <- colorBin(cols, bins = bins, pretty = FALSE)
    
    mapData <- paste("flood", input$timeframe2, sep="_")
    
    col_name <- reactive({
      name <- input$floodMap
      name})
    
    col_value <- reactive({
      if (input$floodMap == "Abs_mean") {
        value <- "Keskiarvo"
      } else if (input$floodMap == "Abs_max") {
        value <- "Maksimi"
      } else if (input$floodMap == "Abs_min") {
        value <- "Minimi"
      }
    })

    map_proxy %>%
      clearGroup("markers") %>%
      addCircleMarkers(data = flood_list[[mapData]], lng = ~lat, lat = ~long,
                       weight = 10,
                       radius = ~sqrt(flood_list[[mapData]][[col_name()]])*5,
                       stroke = FALSE,
                       fillOpacity = 0.4,
                       color = ~pal(flood_list[[mapData]][[col_value()]]),
                       label = ~paste(Nimi, ":", flood_list[[mapData]][[col_value()]], "%"),
                       labelOptions = labelOptions(textsize = "14px"),
                       group = "markers") 

  })

  
  
  
}




#### ShinyApp User Interface ---------------------------------------------------
ui <- shinyUI(fluidPage(
  
  
  useShinyjs(),
  theme = "app_style.css",
  
  tags$head(tags$link(rel = "stylesheet", 
                      type = "text/css",
                      href="//fonts.googleapis.com/css?family=Raleway"),
            htmltools::includeCSS(csspath)),
  
  
  headerPanel(
    title=tags$a(href='https://www.syke.fi/fi-FI', target="_blank"),
    windowTitle = "Ilmastonmuutoksen vaikutus vesistöihin"),
  
  titlePanel(h3("Ilmastonmuutoksen vaikutus vesistöihin -visualisointityökalu")),
  
  
  # First tab #########
  tabsetPanel(
    tabPanel("Ilmastonmuutos ja virtaamat", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 width = 2,
                 id = "sidebar",
                 
                 helpText("Visualisoi ilmastonmuutoksen vaikutuksia vesistöjen virtaamiin eri ajanjaksoilla ja skenaarioilla."),
                 
                 tags$div(title= "Valitse vesistö, jolla sijaitsevan virtaamapisteen tuloksia visualisoidaan.",
                          selectInput(inputId = "location",
                                      label = HTML("Valitse vesistö"),
                                      choices = locations,
                                      selected = "")),
             
                 tags$div(title="Valitse yksi kahdesta tulevaisuuden ajanjaksosta.", 
                          radioButtons(
                            inputId = "timeframe",
                            label = "Valitse ajanjakso",
                            choiceNames = timeframe_names,
                            choiceValues = seq(1:length(timeframe_names))
                 )),
                 
                
                 tags$div(title= "Valitse yksi kolmesta skenaariovaihtoehdosta. Lisätietoa skenaarioista löydät Lisätietoa-välilehdeltä.",
                          radioButtons(inputId = "scenario",label = "Valitse skenaario", 
                                       choiceNames = scenario_names,
                                       choiceValues = seq(1:length(scenario_names))
                 )),
                 
                 # Download plot & table
                 br(),
                 strong("Latauslinkit"),
                 tags$div(title="Lataa näytöllä oleva kuvaaja työasemalle png-muodossa.",
                          downloadLink("kuvaaja_lataus", label = "Lataa kuvaaja (png)")),
                 
                 div(),
                 tags$div(title= "Lataa näytöllä oleva taulukko työasemalle csv-muodossa.",
                          downloadLink("taulukko1_lataus", label = "Lataa taulukko (csv)")),
                
                 div(),
                 br(),
                 HTML(paste("<p id='version-info' style='color: grey; font-size: small;'>Versio<br>", 
                            app_v, "</p>")),
               ),
               
               mainPanel(
                 
                 fluidRow(
                   column(12,
                          fluidRow(
                            column(6,
                                   br(),
                                   ggiraphOutput("plo", 
                                                 width = "100%",
                                                 height = "100%"),
                                   br(),
                                   reactableOutput("table1", width = "100%")),
        
                          column(5,
                                 br(),
                                 p("Mallinnettujen virtaamapisteiden sijainti kartalla"),
                                 
                                 leafletOutput("map1", height = 750, width = "100%"))
                          ))
                   
                 )),
             )
    ),
    # Second tab ############
    tabPanel("Ilmastonmuutos ja tulvat", fluid = TRUE,
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
                          column(6,
                                 br(),
                                 strong("Kuinka paljon keskimäärin kerran sadassa vuodessa tapahtuva tulvan (1/100a) arvioidaan muuttuvan ilmastonmuutoksen vaikutuksesta?"),
                                 p(" "),
                                 p("Taulukkoon ja karttaan on arvioitu 25 eri ilmastonmuutosskenaarion avulla, kuinka paljon 100-vuoden avovesitulva muuttuu valitulla ajanjaksolla suhteessa referenssijaksoon (1981-2010). Keskiarvo kertoo 25 skenaarion keskimääräisen muutoksen, maksimi on skenaarioiden suurin ja minimi pienin muutos."),
                                 
                                 reactableOutput("table2", width = "100%")),
                          
                          column(5,
                                 br(),
                                 strong("Visualisoi muutokset tulvissa valitsemalla taso kartalta."),
                                 p(span(strong("Punainen", style = "color:#b2182b")), "väri viittaa tulvien kasvuun ja ",
                                 span(strong("sininen", style ="color:#3275B8")), "vähenemiseen."),
                                 leafletOutput("map2", height=750, width = "100%"),
                                 
                                 absolutePanel(top =100, right = -160, 
                                               radioButtons("floodMap", "Valitse taso",
                                                            choiceNames= floodmap_names,
                                                            choiceValues = c("Abs_mean", "Abs_max", "Abs_min"),
                                                            selected = character(0)),
                                              
                                 ))))
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