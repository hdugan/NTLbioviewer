# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.

library(shiny)
library(tidyverse)
library(lubridate)
library(leaflet)
library(EDIutils)

# Define UI for data viewer application
shinyUI(fluidPage(
  
  # Application title
  # titlePanel("NTL-LTER data viewer"),
  titlePanel(title=div(img(src="https://lter.limnology.wisc.edu/wp-content/uploads/sites/2029/2023/06/NTL_logo_notext-768x768.png",
                           height="5%", width = "5%"),
                       "NTL-LTER CPUE")),
  
  helpText("North Temperate Lakes Long-Term Ecological Research"),
  
  tags$head(tags$style("#verb{color: black;
                                 font-size: 10px;
                                 font-style: italic;
                                 }")),
  
  # Sidebar with user inputs
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = 'input.vars', 
                  label = 'Choose variable', 
                  choices = list(Biology = c('Crayfish',
                                             'Fish')),
                  selected = 'Water Temperature (degC)'),
      # selectInput(inputId = 'input.lake', 
      #             label = 'Choose lake', 
      #             choices = list(NorthernLakes = c('All northern lakes',
      #                                              'Allequash',
      #                                              'Big Musky',
      #                                              'Crystal',
      #                                              'Crystal Bog',
      #                                              'Sparkling',
      #                                              'Trout',
      #                                              'Trout Bog'),
      #                            `Southern Lakes` = c(
      #                              'All southern lakes',
      #                              'Mendota',
      #                              'Monona',
      #                              'Fish',
      #                              'Wingra'))),
      
      uiOutput("dataLakes"),
      uiOutput("dataSpecies"),
      uiOutput("dataGear"),
      # radioButtons("plottype", "Plot type:",
      #              c("Time-Series" = "plot.ts",
      #                "Annual Means" = "plot.am",
      #                "Monthly Boxplots" = "plot.mb"),
      #              selected = 'plot.ts'),
      # selectInput(inputId = 'input.depth', 
      #             label = 'Choose depth', 
      #             choices = c(0:20)),
      # checkboxInput(inputId = "scales", label = 'Free y-axis', value = FALSE),
      checkboxGroupInput(inputId = "scales", label = c('Options'), 
                         choices = c('Free y-axis', 'Log y-axis'),
                         selected = c('Free y-axis')),
      
      downloadButton(outputId = 'downloadImage', 'Download plot'),
    ),
    
    # Show the tabs 
    mainPanel(
      tabsetPanel(
        tabPanel("Data", plotOutput("distPlot")),
        # tabPanel("Data", plotOutput("distPlot"), textOutput("verb")),
        tabPanel("Map", leafletOutput("myMap"))
      )
    )
    
    # mainPanel(
    #     # plotOutput("distPlot")
    #     textOutput("testvar"),
    #     # textOutput("testvar2"),
    # )
    
  ),
  # Footer
  hr(),
  # print(textOutput("urlname"))
  print(uiOutput("urlname")),
  p("This material is based upon work supported by the National Science Foundation under Cooperative Agreement #DEB-2025982, NTL LTER. Any opinions, findings, conclusions, or recommendations expressed in the material are those of the author(s) and do not necessarily reflect the views of the National Science Foundation.", 
    align="left", style = "font-size:11px; color: #751e04;"),
))
