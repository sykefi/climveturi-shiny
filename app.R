# Climveturi App
# 19.10.2020, 21.10, 22.10, 26.10



# Clear workspace
rm(list = ls())

### INITIALISE -----------------------------------------------------------------

#### Working paths ####
wd <- getwd()

# Component paths. Commented because this does not exist ahaha
#csspath <- file.path(wd, "app_style.css")

# App version
app_v <- "0003 (26.10.2020)"


# Import libraries
require(shiny)
require(ggplot2)
require(dplyr)
require(lubridate)
require(reshape2)
require(DT)
require(data.table)
require(formattable)
require(shinythemes)
require(shinyjs)
library(htmltools)
library(leaflet)
library(leaflet.minicharts)



setwd("C:/Users/e1007642/Documents/ClimVeturi/git/shiny")

### load data -------------------------------------------------------------


# Plots and tables
ref_list <- readRDS("data/ref_list.rds")
scen_list <- readRDS("data/scen_list.rds")
chg_dfs <- readRDS("data/chg_dfs.rds")

# Flood


#### ---------------------------------------------------------------------------

# Load custom functions. Commented because this does not exist ahaha
#source(file.path(wd, "app_funcs.R"))


# Parameters
locations <- c("Vuoksi", "Kymijoki", "Saarijärven reitti", "Rautalammin reitti",
               "Vantaanjoki","Aurajoki","Kokemäenjoki","Längelmäveden-Hauhon reitti",
               "Loimijoki","Lapväärtinjoki", "Laihianjoki",
               "Kyrönjoki", "Lapuanjoki","Perhonjoki",
               "Kalajoki", "Pyhäjoki", "Siikajoki","Ala-Oulujoki",
               "Ontojärvi-Lentua","Iijoki", "Simojoki",
               "Ala-Kemijoki","Ylä-Ounasjoki", "Kitinen", 
               "Tornionjoki-Muonionjoki","Teno", "Paatsjoki") %>%
  sort()

timeframe_names <- c("2010-2039", "2040-2069") # 1, 2
scenario_names <- c("Lämmin ja märkä", "Kylmä", "Usean skenaarion keskiarvo") # 1, 2, 3



#load("ref_list.RData")

#### ShinyApp Server -----------------------------------------------------------

server <- function(input, output){
  
  # Hide sidebar in flood-page 
  observeEvent(input[["tabset"]], {
    if(input[["tabset"]] == "Muutokset tulvissa?"){
      hideElement(selector = "#sidebar")
      # removeCssClass("main", "col-sm-8")
      # addCssClass("main", "col-sm-12")
    }else{
      showElement(selector = "#sidebar")
      # removeCssClass("main", "col-sm-12")
      # addCssClass("main", "col-sm-8")
    }
  })
  
  # Table with % changes
  output$table <- renderFormattable({
    
    thisName <- paste(input$location, input$timeframe,
                      input$scenario, "%", sep = "_")
    
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
    
  
    
    plo <- ggplot(data = thisRefPlot, 
                  aes(x = D_M, y = mean,  group = "group")) +
      
      labs(title= paste(input$location,
                        "\nAjanjakso: ", times[input$timeframe],
                        "\nSkenaario: ", scens[input$scenario]),
           y = expression(paste("Virtaama (", m^3,"/s)", sep=""))) +
      
      geom_ribbon(aes(ymin=min, ymax=max, fill = "ref2"), 
                  colour = NA, alpha = 0.5) +
      geom_line(aes(y = mean, colour = "ref1"), size = 1.2, alpha = 0.8) +
      
      geom_line(data=thisPlot, aes(y = mean, colour = as.character(input$scenario), group = 1),
                size = 1.2, alpha = 0.8) +
      geom_ribbon(data=thisPlot, aes(ymin = min, ymax = max, colour = as.character(input$scenario), group = 1),linetype = 3,
                  fill = NA, size = 1.1, alpha = 0.8) +
      
      
      # MUUT ASETUKSET
      # Y ja X akselit
      scale_y_continuous(breaks = scales::pretty_breaks(n = 6))+
      
      # x-akseli, kuukausittain
      scale_x_date(expand = c(0,0),date_labels = "%b", date_breaks = "1 month")+
      
      scale_colour_manual(name = " ", values = cols,
                          breaks = c("ref1", as.character(input$scenario)),
                          labels = c("1981-2010 simuloitu keskiarvo",
                                     paste(times[input$timeframe], "Simuloitu keskiarvo ja vaihteluväli", sep = " "))) +
      scale_fill_manual(name = " ", values = cols,
                        breaks = c("ref2"),
                        labels = c("1981-2010 simuloitu vaihteluväli (max-min)")) +
    
      guides(colour = guide_legend(override.aes = list(linetype=c(1,1),
                                                       shape = c(16, 16)))) +
      
      
      # Tyyliseikat
      theme(axis.title.x=element_blank(),
            axis.text.x = element_text(size=12, face = "bold"),
            axis.text.y = element_text(size=12),
            panel.background = element_blank(),
            #panel.grid.major.y = element_line(colour="grey70"),
            axis.line = element_line(colour="grey"),
            legend.position ="bottom",
            legend.justification = c("left", "top"),
            #legend.box.just = "left",
            legend.margin = margin(6, 6, 6, 6),
            legend.background = element_blank(),
            legend.text = element_text(size=11),
            legend.box.background = element_rect(alpha("white", 0.3), color =NA),
            plot.title = element_text(size=14)) 
    
    #width = 9

    plo_out <<- plo
    
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
  
}


#### ShinyApp User Interface ---------------------------------------------------
ui <- shinyUI(fluidPage(
  useShinyjs(),
  theme = shinytheme("paper"),
  
  
  headerPanel(
    title=tags$a(href='https://www.syke.fi/fi-FI',tags$img(src='SYKE_tunnus_rgb_vaaka.png', 
                                                           height = 50*0.75, width = 182*0.75), target="_blank"),
    windowTitle = "ClimVeTuri ilmastonmuutos"
  ),

  # Import CSS style from external file
  #tags$head(htmltools::includeCSS(csspath)),
  
  titlePanel(h4("ClimVeturi visualisoinnit")),
  
  
  ### sidebarLayout contents ---------------------------------------------------
  sidebarLayout(
    sidebarPanel(
      width = 3,
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
    
    
    
    
    ### mainPanel contents -----------------------------------------------------
    mainPanel(
      #width = 8,
      
      tabsetPanel(
        tabPanel("Muutokset virtaamissa", 
                 fluidRow(
                   column(8,
                          h5("Kuvaaja")),
                   column(8,downloadButton("kuvaaja_lataus",
                                            label = HTML("<i class='icon file' title='Lataa kuvaaja (png)'></i>"))),
                   column(8, plotOutput("plo")),
                   br(),
                   column(12, h6("Muutokset virtaamissa suhteessa referenssijaksoon (1981-2010) valitulla ajanjaksolla ja skenaariolla")),
                   column(12,  formattableOutput("table",width = 400))
                 )),
        
     
        
        
        tabPanel("Muutokset tulvissa?",
                 fluidRow(
                   h6("Tänne visualisointeja liittyen 100-vuoden tulvien muutoksiin."),
                   p("Esim. kartta ja taulukko filttereiden mukaisesti/karttaa klikkaamalla...?")
                 )),
        
        
        
        tabPanel("Käyttöohjeet",
                 fluidRow(
                   column(8,
                          includeMarkdown('./user-guide/userguide.rmd')
                   )
                 )),
        id = "tabset"
      ),
      id = "main"
      
      
      
    )
  )
))


### Run ShinyApp ---------------------------------------------------------------

shinyApp(ui = ui, server = server)