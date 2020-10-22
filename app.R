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
app_v <- "0002 (22.10.2020)"

# Install libraries
#install.packages("shiny")
#install.package("ggplot2")

# Import libraries
require(shiny)
require(ggplot2)
require(dplyr)
require(lubridate)
require(reshape2)



setwd("C:/Users/e1007642/Documents/ClimVeturi/git/shiny")

### load data -------------------------------------------------------------


# Plots
ref_list <- readRDS("data/ref_list.rds")
scen_list <- readRDS("data/scen_list.rds")


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
  
  
  #output$tab <- renderTable({
  
  #thisName <- paste(input$location, input$timeframe, input$scenario, sep = "_")
  #message(thisName)
  #list_dfs[[thisName]]
  #})
  
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
    
    scens <- c("1" = "Lämmin ja märkä",
               "2" = "Kylmä",
               "3" = "Usean skenaarion keskiarvo")
    
    times <- c("1" = "2010-2039",
               "2" = "2040-2069")
    
    ##
    #options(repr.plot.width = 14, repr.plot.height = 8)
    
    plo <- ggplot(data = thisRefPlot, 
                  aes(x = D_M, y = mean,  group = "group")) +
      
      labs(title= paste(input$location, "\nSkenaario: ", scens[input$scenario],
                        "\nAjanjakso: ", times[input$timeframe]), 
           y = expression(paste("Virtaama (", m^3,"/s)", sep=""))) +
      
      geom_ribbon(aes(ymin=min, ymax=max, fill = "ref2"), 
                  colour = NA, alpha = 0.5) +
      geom_line(aes(y = mean, colour = "ref1"), size = 1.2, alpha = 0.8) +
      
      geom_line(data=thisPlot, aes(y = mean, colour = as.character(input$scenario), group = 1),
                size = 1.2, alpha = 0.8) +
      geom_ribbon(data=thisPlot, aes(ymin = min, ymax = max, colour = as.character(input$scenario), group = 1),linetype = 3,
                  fill = NA, size = 1.05, alpha = 0.8) +
      
      
      # MUUT ASETUKSET
      # Y ja X akselit
      scale_y_continuous(breaks = scales::pretty_breaks(n = 6))+
      
      # x-akseli, kuukausittain
      scale_x_date(expand = c(0,0),date_labels = "%b", date_breaks = "1 month")+
      
      scale_colour_manual(name = " ", values = cols,
                          breaks = c("ref1", as.character(input$scenario)),
                          labels = c("1981-2010 simuloitu keskiarvo",
                                     "Simuloitu keskiarvo ja vaihteluväli")) +
      scale_fill_manual(name = " ", values = cols,
                        breaks = c("ref2"),
                        labels = c("1981-2010 simuloitu vaihteluväli (max-min)")) +
    
      guides(colour = guide_legend(override.aes = list(linetype=c(1,1),
                                                       shape = c(16, 16)))) +
      
      
      # Tyyliseikat
      theme(axis.title.x=element_blank(),
            axis.text.x = element_text(size=11, face = "bold"),
            axis.text.y = element_text(size=11),
            panel.background = element_blank(),
            panel.grid.major.y = element_line(colour="grey70"),
            axis.line = element_line(colour="grey"),
            legend.position = "bottom",
            legend.justification ="left",
            #legend.box.just = "left",
            legend.margin = margin(6, 6, 6, 6),
            legend.background = element_blank(),
            legend.text = element_text(size=11),
            legend.box.background = element_rect(alpha("white", 0.3), color =NA),
            plot.title = element_text(size=14)) 
    
    #width = 9
    
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
      h4("Virtaamien muutos muuttuvassa ilmastossa"),
      plotOutput("plo")
    )
  )
))


### Run ShinyApp ---------------------------------------------------------------

shinyApp(ui = ui, server = server)