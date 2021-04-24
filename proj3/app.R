#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(tigris)
library(ggplot2)
library(lubridate)
library(mapview)
library(dplyr)
library(tidycensus)
library(tidyverse)
library(sf)
library(leaflet)



# reading in data
data <- read.csv("Energy_Usage_2010.csv", sep = ',', header = TRUE)
chiblocks <- blocks(state = 'IL', county = 'Cook', year = '2010')
data$CENSUS.BLOCK <- as.character(data$CENSUS.BLOCK)
data$GEOID10 <- data$CENSUS.BLOCK
chitracts <- tracts(state = 'IL', county = 'Cook', year = '2010')

neighborhood <- unique(data$COMMUNITY.AREA.NAME)
neighborhood2 <- unique(data$COMMUNITY.AREA.NAME)

# selections for the dropdowns
sources <- list('Electricity' = 'TOTAL.KWH', 'Gas' = 'TOTAL.THERMS', 'Age' = 'AVERAGE.BUILDING.AGE', 
                'Height' = 'AVERAGE.BUILDING.HEIGHT', 'Population' = 'TOTAL.POPULATION')

months <- list('All' = 'TOTAL', 'January' = 'JANUARY.2010', 'February' = 'FEBRUARY.2010', 'March' = 'MARCH.2010',
               'April' = 'APRIL.2010', 'May' = 'MAY.2010', 'June' = 'JUNE.2010', 'July' = 'JULY.2010',
               'August' = 'AUGUST.2010', 'September' = 'SEPTEMBER.2010', 'October' = 'OCTOBER.2010', 'November' = 'NOVEMBER.2010',
               'December' = 'DECEMBER.2010')

types <- list('All' = 'All', 'Residential' = 'Residential', 'Commmercial' = 'Commercial', 'Industrial' = 'Industrial')

sources2 <- list('Electricity' = 'TOTAL.KWH', 'Gas' = 'TOTAL.THERMS', 'Age' = 'AVERAGE.BUILDING.AGE', 
                'Height' = 'AVERAGE.BUILDING.HEIGHT', 'Population' = 'TOTAL.POPULATION')

months2 <- list('January' = 'JANUARY.2010', 'February' = 'FEBRUARY.2010', 'March' = 'MARCH.2010',
               'April' = 'APRIL.2010', 'May' = 'MAY.2010', 'June' = 'JUNE.2010', 'July' = 'JULY.2010',
               'August' = 'AUGUST.2010', 'September' = 'SEPTEMBER.2010', 'October' = 'OCTOBER.2010', 'November' = 'NOVEMBER.2010',
               'December' = 'DECEMBER.2010')

types2 <- list('All' = 'All', 'Residential' = 'Residential', 'Commmercial' = 'Commercial', 'Industrial' = 'Industrial')



# shiny application 
ui <- dashboardPage(
    skin = "green",
    dashboardHeader(title = "We've Got the Power"),
    dashboardSidebar(disable = FALSE, collapsed = FALSE, 
                     # menu on the left hand side
                     sidebarMenu(
                         menuItem("West Side Map", tabName = "west", icon = icon("map-pin")),
                         menuItem("Neighborhood Comparisons", tabName = "states", icon = icon("equals")),
                         menuItem("Comparison Charts", tabName = "charts", icon = icon("chart-line")),
                         menuItem("Chicago", tabName = "chi", icon = icon("city")),
                         menuItem("About", tabName = "credits", icon = icon("check"))
                     ),
                     sidebarMenuOutput("menu"),
                     selectInput("Community", "Select a community", neighborhood, selected = 'Near West Side'),
                     selectInput("Community2", "Select another community", neighborhood2, selected = 'Loop')
    ),
    dashboardBody(
        tabItems(
            # Near West Side Leaflet map
            tabItem(tabName = "west",
                    fluidRow(
                        column(6,
                               box(title = "Mapview Map of Near West Side", solidHeader = TRUE,
                                   status = "primary", width = 12,
                                   leafletOutput("map1")
                               ),
                               selectInput("Source", "Select a source to visualize", sources, selected = 'Electricity'),
                               selectInput("Month", "Select a month to visualize", months, selected = 'All'),
                               selectInput("Type", "Select a type to visualize", types, selected = 'All')
                        ),
                        column(6,
                               box(title = "Line Graphs of Near West Side", solidHeader = TRUE,
                                   status = "primary", width = 12,
                                   plotOutput("plot1")
                               ),
                               box(title = "Table of Near West Side", solidHeader = TRUE,
                                   status = "primary", width = 12,
                                   dataTableOutput("tab1")
                               )
                        )
                    )
            ),
            # different neighborhood comparisons and different graphs
            tabItem(tabName = "states",
                    fluidRow(
                        column(5,
                               box(title = "Map 1", solidHeader = TRUE,
                                   status = "primary", width = 12,
                                   leafletOutput("map2")
                               ),
                               selectInput("Source2", "Select a source to visualize", sources, selected = 'Electricity'),
                               selectInput("Month2", "Select a month to visualize", months, selected = 'All'),
                               selectInput("Type2", "Select a type to visualize", types, selected = 'All')
                        ),
                        column(5,
                               box(title = "Map 2", solidHeader = TRUE,
                                   status = "primary", width = 12,
                                   leafletOutput("map3")
                               ),
                               selectInput("Source3", "Select a source to visualize", sources2, selected = 'Electricity'),
                               selectInput("Month3", "Select a month to visualize", months2, selected = 'All'),
                               selectInput("Type3", "Select a type to visualize", types2, selected = 'All')
                        ),
                    )
            ),
            tabItem(tabName = "charts",
                    fluidRow(
                        column(6,
                               box(title = "Line Graph 1", solidHeader = TRUE,
                                   status = "primary", width = 12,
                                   plotOutput("plot2")
                               ),
                               box(title = "Table 1", solidHeader = TRUE,
                                   status = "primary", width = 12,
                                   dataTableOutput("tab2")
                               )
                        ),
                        column(6,
                               box(title = "Line Graph 2", solidHeader = TRUE,
                                   status = "primary", width = 12,
                                   plotOutput("plot3")
                               ),
                               box(title = "Table 2", solidHeader = TRUE,
                                   status = "primary", width = 12,
                                   dataTableOutput("tab3")
                               )
                        )
                    )
            ),
            # Chicago map
            tabItem(tabName = "chi",
                    column(10,
                           box(title = "Chicago Map", solidHeader = TRUE,
                               status = "primary", width = 12,
                               leafletOutput("map4")
                           ),
                           selectInput("Source4", "Select a source to visualize", sources, selected = 'Electricity'),
                           selectInput("Month4", "Select a month to visualize", months, selected = 'All'),
                           selectInput("Type4", "Select a type to visualize", types, selected = 'All')
                    ),
                    column(1,
                    )
            ),
            #about
            tabItem(tabName = "credits",
                    h1("About"),
                    h2("Created by: Matthew Ghuneim"),
                    h3("This was the third project for CS 424 Spring 2021. 
                       I downloaded the data file from the City of Chicago website and loaded it in to R.
                       The code available to run this is located here: https://github.com/mghuneim/424project3")
            )
        )
    )
)

server <- function(input, output) {
    
    # all functions for the maps
    output$map1 <- renderLeaflet({
        chidata <- subset(data, data$COMMUNITY.AREA.NAME == 'Near West Side')
        chiblocks <- subset(chiblocks, GEOID10 %in% chidata$GEOID10)
        chiblocks <- merge(chiblocks, chidata, by.x = 'GEOID10', by.y = 'GEOID10')
        
        if ('TOTAL.KWH' %in% input$Source | 'TOTAL.THERMS' %in% input$Source){
            if ('All' %in% input$Month & 'TOTAL.KWH' %in% input$Source){
                p <- paste0(input$Month, ".KWH")
            }
            else if ((!'All' %in% input$Month) & 'TOTAL.KWH' %in% input$Source){
                p <- paste0("KWH.", input$Month)
            }
            else if ((!'All' %in% input$Month) & 'TOTAL.THERMS' %in% input$Source){
                p <- paste0("THERM.", input$Month)
            }
            
        }
        
        mapview(chiblocks, zcol = p)@map
        
    })
    
    output$map2 <- renderLeaflet({
        chidata <- subset(data, data$COMMUNITY.AREA.NAME == input$Community)
        chiblocks <- subset(chiblocks, GEOID10 %in% chidata$GEOID10)
        chiblocks <- merge(chiblocks, chidata, by.x = 'GEOID10', by.y = 'GEOID10')
        
        if ('TOTAL.KWH' %in% input$Source2 | 'TOTAL.THERMS' %in% input$Source2){
            if ((!'All' %in% input$Month2) & 'TOTAL.KWH' %in% input$Source2){
                p <- paste0("KWH.", input$Month2)
            }
            else if ((!'All' %in% input$Month2) & 'TOTAL.THERMS' %in% input$Source2){
                p <- paste0("THERM.", input$Month2)
            }
        }
        
        mapview(chiblocks, zcol = p)@map
        
    })
    
    output$map3 <- renderLeaflet({
        chidata <- subset(data, data$COMMUNITY.AREA.NAME == input$Community2)
        chiblocks <- subset(chiblocks, GEOID10 %in% chidata$GEOID10)
        chiblocks <- merge(chiblocks, chidata, by.x = 'GEOID10', by.y = 'GEOID10')
        
        if ('TOTAL.KWH' %in% input$Source3 | 'TOTAL.THERMS' %in% input$Source3){
            if ((!'All' %in% input$Month3) & 'TOTAL.KWH' %in% input$Source3){
                p <- paste0("KWH.", input$Month3)
            }
            else if ((!'All' %in% input$Month3) & 'TOTAL.THERMS' %in% input$Source3){
                p <- paste0("THERM.", input$Month3)
            }
        }
        
        mapview(chiblocks, zcol = p)@map
        
    })
    
    output$map4 <- renderLeaflet({
        chiblocks <- subset(chiblocks, GEOID10 %in% data$GEOID10)
        chiblocks <- merge(chiblocks, data, by.x = 'GEOID10', by.y = 'GEOID10')
        
        p <- input$Source
        if ('TOTAL.KWH' %in% input$Source4 | 'TOTAL.THERMS' %in% input$Source4){
            if ((!'All' %in% input$Month4) & 'TOTAL.KWH' %in% input$Source4){
                p <- paste0("KWH.", input$Month4)
            }
            else if ((!'All' %in% input$Month4) & 'TOTAL.THERMS' %in% input$Source4){
                p <- paste0("THERM.", input$Month4)
            }
        }
        
        mapview(chiblocks, zcol = p)@map
        
    })
    
    # all functions for the graphs
    output$plot1 <- renderPlot({
        if ('TOTAL.KWH' %in% input$Source){
            kwh <- data.frame(colSums(data[, 5:16]), na.rm = TRUE)
            
        }
    })

    
}

# Run the application 
shinyApp(ui = ui, server = server)
