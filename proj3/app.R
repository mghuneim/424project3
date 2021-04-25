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
library(DT)



# reading in data
data <- read.csv("Energy_Usage_2010.csv", sep = ',', header = TRUE)
chiblocks <- blocks(state = 'IL', county = 'Cook', year = '2010')
data$CENSUS.BLOCK <- as.character(data$CENSUS.BLOCK)
data$GEOID10 <- data$CENSUS.BLOCK
chitracts <- tracts(state = 'IL', county = 'Cook', year = '2010')

neighborhood <- unique(data$COMMUNITY.AREA.NAME)
neighborhood2 <- unique(data$COMMUNITY.AREA.NAME)

# selections for the dropdowns
sources <- list('Electricity' = 'TOTAL.KWH', 'Gas' = 'TOTAL.THERMS', 'Age' = 'AVERAGE.BUILDING.AGE', 'Type' = 'BUILDING.TYPE',
                'Height' = 'AVERAGE.STORIES', 'Population' = 'TOTAL.POPULATION')

months <- list('January' = 'JANUARY.2010', 'February' = 'FEBRUARY.2010', 'March' = 'MARCH.2010',
               'April' = 'APRIL.2010', 'May' = 'MAY.2010', 'June' = 'JUNE.2010', 'July' = 'JULY.2010',
               'August' = 'AUGUST.2010', 'September' = 'SEPTEMBER.2010', 'October' = 'OCTOBER.2010', 'November' = 'NOVEMBER.2010',
               'December' = 'DECEMBER.2010')



sources2 <- list('Electricity' = 'TOTAL.KWH', 'Gas' = 'TOTAL.THERMS', 'Age' = 'AVERAGE.BUILDING.AGE', 'Type' = 'BUILDING.TYPE',
                'Height' = 'AVERAGE.BUILDING.HEIGHT', 'Population' = 'TOTAL.POPULATION')

months2 <- list('January' = 'JANUARY.2010', 'February' = 'FEBRUARY.2010', 'March' = 'MARCH.2010',
               'April' = 'APRIL.2010', 'May' = 'MAY.2010', 'June' = 'JUNE.2010', 'July' = 'JULY.2010',
               'August' = 'AUGUST.2010', 'September' = 'SEPTEMBER.2010', 'October' = 'OCTOBER.2010', 'November' = 'NOVEMBER.2010',
               'December' = 'DECEMBER.2010')


cityOptions <- list('Oldest Buildings', 'Newest Buildings', 'Tallest Buildings', 'Most Electricity', 'Most Gas', 'Most Population', 'Most Occupied', 'Renters (Highest %)')

chartMonths <- month.name


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
                               selectInput("Month", "Select a month to visualize", months, selected = 'All')
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
                        column(6,
                               box(title = "Map 1", solidHeader = TRUE,
                                   status = "primary", width = 12,
                                   leafletOutput("map2")
                               ),
                               selectInput("Source2", "Select a source to visualize", sources, selected = 'Electricity'),
                               selectInput("Month2", "Select a month to visualize", months, selected = 'All')
                        ),
                        column(6,
                               box(title = "Map 2", solidHeader = TRUE,
                                   status = "primary", width = 12,
                                   leafletOutput("map3")
                               ),
                               selectInput("Source3", "Select a source to visualize", sources2, selected = 'Electricity'),
                               selectInput("Month3", "Select a month to visualize", months2, selected = 'All')
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
            tabItem(tabName = "chi",
                    fluidRow(
                        column(8,
                               box(title = "Mapview Map of Chicago", solidHeader = TRUE,
                                   status = "primary", width = 12,
                                   leafletOutput("map4")
                               ),
                        ),
                        column(4, 
                        selectInput("Source4", "Select a tract to visualize", cityOptions, selected = 'Oldest Buildings')
                        )
                    )
            ),
            #about
            tabItem(tabName = "credits",
                    h1("About"),
                    h2("Created by: Matthew Ghuneim"),
                    h3("This was the third project for CS 424 Spring 2021. 
                       I downloaded the data file from the City of Chicago website and loaded it in to R.
                       The code available to run this is located here: https://github.com/mghuneim/424project3. 
                       The original data file is located here:  https://data.cityofchicago.org/Environment-Sustainable-Development/Energy-Usage-2010/8yq3-m6wp")
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
                m1 <- mapview(chiblocks, zcol = 'TOTAL.KWH')
            }
            else if ((!'All' %in% input$Month) & 'TOTAL.KWH' %in% input$Source){
                p <- paste0("KWH.", input$Month)
                m1 <- mapview(chiblocks, zcol = p)
            }
            else if ((!'All' %in% input$Month) & 'TOTAL.THERMS' %in% input$Source){
                p <- paste0("THERM.", input$Month)
                m1 <- mapview(chiblocks, zcol = p)
            }
            
        }
        else if (input$Source == 'AVERAGE.BUILDING.AGE'){
            m1 <- mapview(chiblocks, zcol = 'AVERAGE.BUILDING.AGE')
        }
        else if (input$Source == 'BUILDING.TYPE'){
            m1 <- mapview(chiblocks, zcol = 'BUILDING.TYPE')
        }
        else if (input$Source == 'AVERAGE.STORIES'){
            m1 <- mapview(chiblocks, zcol = 'AVERAGE.STORIES')
        }
        else if (input$Source == 'TOTAL.POPULATION'){
            m1 <- mapview(chiblocks, zcol = 'TOTAL.POPULATION')
        }
        
        
        m1@map
        
    })
    
    output$map2 <- renderLeaflet({
        chidata <- subset(data, data$COMMUNITY.AREA.NAME == input$Community)
        chiblocks <- subset(chiblocks, GEOID10 %in% chidata$GEOID10)
        chiblocks <- merge(chiblocks, chidata, by.x = 'GEOID10', by.y = 'GEOID10')
        
        if ('TOTAL.KWH' %in% input$Source2 | 'TOTAL.THERMS' %in% input$Source2){
            if ((!'All' %in% input$Month2) & 'TOTAL.KWH' %in% input$Source2){
                p <- paste0("KWH.", input$Month2)
                m1 <- mapview(chiblocks, zcol = p)
            }
            else if ((!'All' %in% input$Month2) & 'TOTAL.THERMS' %in% input$Source2){
                p <- paste0("THERM.", input$Month2)
                m1 <- mapview(chiblocks, zcol = p)
            }
        }
        else if (input$Source2 == 'AVERAGE.BUILDING.AGE'){
            m1 <- mapview(chiblocks, zcol = 'AVERAGE.BUILDING.AGE')
        }
        else if (input$Source2 == 'AVERAGE.STORIES'){
            m1 <- mapview(chiblocks, zcol = 'AVERAGE.STORIES')
        }
        else if (input$Source2 == 'TOTAL.POPULATION'){
            m1 <- mapview(chiblocks, zcol = 'TOTAL.POPULATION')
        }
       
        
        m1@map
        
    })
    
    output$map3 <- renderLeaflet({
        chidata <- subset(data, data$COMMUNITY.AREA.NAME == input$Community2)
        chiblocks <- subset(chiblocks, GEOID10 %in% chidata$GEOID10)
        chiblocks <- merge(chiblocks, chidata, by.x = 'GEOID10', by.y = 'GEOID10')
        
        if ('TOTAL.KWH' %in% input$Source3 | 'TOTAL.THERMS' %in% input$Source3){
            if ((!'All' %in% input$Month3) & 'TOTAL.KWH' %in% input$Source3){
                p <- paste0("KWH.", input$Month3)
                m1 <- mapview(chiblocks, zcol = p)
            }
            else if ((!'All' %in% input$Month3) & 'TOTAL.THERMS' %in% input$Source3){
                p <- paste0("THERM.", input$Month3)
                m1 <- mapview(chiblocks, zcol = p)
            }
        }
        else if (input$Source3 == 'AVERAGE.BUILDING.AGE'){
            m1 <- mapview(chiblocks, zcol = 'AVERAGE.BUILDING.AGE')
        }
        else if (input$Source3 == 'AVERAGE.STORIES'){
            m1 <- mapview(chiblocks, zcol = 'AVERAGE.STORIES')
        }
        else if (input$Source3 == 'TOTAL.POPULATION'){
            m1 <- mapview(chiblocks, zcol = 'TOTAL.POPULATION')
        }
        m1@map
        
    })
    
    output$map4 <- renderLeaflet({
        chidata <- data
        chiblocks <- subset(chiblocks, GEOID10 %in% chidata$GEOID10)
        
        if (input$Source4 == 'Oldest Buildings'){
            chiblocks <- merge(chiblocks, chidata, by.x = 'GEOID10', by.y = 'GEOID10')
            chiblocks <- head(chiblocks[order(chiblocks$AVERAGE.BUILDING.AGE, decreasing = TRUE),], n = 180)
            m1 <- mapview(chiblocks, zcol = 'AVERAGE.BUILDING.AGE')
        }
        else if (input$Source4 == 'Newest Buildings'){
            chiblocks <- merge(chiblocks, chidata, by.x = 'GEOID10', by.y = 'GEOID10')
            chiblocks <- head(chiblocks[order(chiblocks$AVERAGE.BUILDING.AGE, decreasing = FALSE),], n = 180)
            m1 <- mapview(chiblocks, zcol = 'AVERAGE.BUILDING.AGE')
        }
        else if (input$Source4 == 'Tallest Buildings'){
            chiblocks <- merge(chiblocks, chidata, by.x = 'GEOID10', by.y = 'GEOID10')
            chiblocks <- head(chiblocks[order(chiblocks$AVERAGE.STORIES, decreasing = TRUE),], n = 180)
            m1 <- mapview(chiblocks, zcol = 'AVERAGE.STORIES')
        }
        else if (input$Source4 == 'Most Electricity'){
            chiblocks <- merge(chiblocks, chidata, by.x = 'GEOID10', by.y = 'GEOID10')
            chiblocks <- head(chiblocks[order(chiblocks$TOTAL.KWH, decreasing = TRUE),], n = 180)
            m1 <- mapview(chiblocks, zcol = 'TOTAL.KWH')
        }
        else if (input$Source4 == 'Most Gas'){
            chiblocks <- merge(chiblocks, chidata, by.x = 'GEOID10', by.y = 'GEOID10')
            chiblocks <- head(chiblocks[order(chiblocks$TOTAL.THERMS, decreasing = TRUE),], n = 180)
            m1 <- mapview(chiblocks, zcol = 'TOTAL.THERMS')
        }
        else if (input$Source4 == 'Most Population'){
            chiblocks <- merge(chiblocks, chidata, by.x = 'GEOID10', by.y = 'GEOID10')
            chiblocks <- head(chiblocks[order(chiblocks$TOTAL.POPULATION, decreasing = TRUE),], n = 180)
            m1 <- mapview(chiblocks, zcol = 'TOTAL.POPULATION')
        }
        else if (input$Source4 == 'Most Occupied'){
            chiblocks <- merge(chiblocks, chidata, by.x = 'GEOID10', by.y = 'GEOID10')
            chiblocks <- head(chiblocks[order(chiblocks$OCCUPIED.UNITS.PERCENTAGE, decreasing = TRUE),], n = 180)
            m1 <- mapview(chiblocks, zcol = 'OCCUPIED.UNITS.PERCENTAGE')
        }
        else if (input$Source4 == 'Renters (Highest %)'){
            chiblocks <- merge(chiblocks, chidata, by.x = 'GEOID10', by.y = 'GEOID10')
            chiblocks <- head(chiblocks[order(chiblocks$RENTER.OCCUPIED.HOUSING.PERCENTAGE, decreasing = TRUE),], n = 180)
            m1 <- mapview(chiblocks, zcol = 'RENTER.OCCUPIED.HOUSING.PERCENTAGE')
        }
        
    
        m1@map
        
    })
    
    # all functions for the graphs
    output$plot1 <- renderPlot({
        data2 <- subset(data, data$COMMUNITY.AREA.NAME == "Near West Side")
        if('TOTAL.KWH' %in% input$Source){
            kwh <- data.frame(colSums(data2[, 5:16], na.rm = TRUE))
            colnames(kwh)[1] <- c("Totals")
            kwh$month <- chartMonths
            kwh$month = factor(kwh$month, levels = month.name)
            ggplot(kwh, aes(x = month, y = Totals, fill = month)) + geom_bar(stat = 'identity')  + xlab('Month') + ylab('Total Electricity Usage')
        }
        else if ('TOTAL.THERMS' %in% input$Source){
            therm <- data.frame(colSums(data2[, 20:31], na.rm = TRUE))
            colnames(therm)[1] <- c("Totals")
            therm$month <- chartMonths
            therm$month = factor(therm$month, levels = month.name)
            ggplot(therm, aes(x = month, y = Totals, fill = month)) + geom_bar(stat = 'identity') + xlab('Month') + ylab('Total Gas Usage')
        }
    })
    output$plot2 <- renderPlot({
        data2 <- subset(data, data$COMMUNITY.AREA.NAME == input$Community)
        if('TOTAL.KWH' %in% input$Source2){
            kwh <- data.frame(colSums(data2[, 5:16], na.rm = TRUE))
            colnames(kwh)[1] <- c("Totals")
            kwh$month <- chartMonths
            kwh$month = factor(kwh$month, levels = month.name)
            ggplot(kwh, aes(x = month, y = Totals, fill = month)) + geom_bar(stat = 'identity') + xlab('Month') + ylab('Total Electricity Usage')
        }
        else if ('TOTAL.THERMS' %in% input$Source2){
            therm <- data.frame(colSums(data2[, 20:31], na.rm = TRUE))
            colnames(therm)[1] <- c("Totals")
            therm$month <- chartMonths
            therm$month = factor(therm$month, levels = month.name)
            ggplot(therm, aes(x = month, y = Totals, fill = month)) + geom_bar(stat = 'identity') + xlab('Month') + ylab('Total Gas Usage')
        }
    })
    output$plot3 <- renderPlot({
        data2 <- subset(data, data$COMMUNITY.AREA.NAME == input$Community2)
        if('TOTAL.KWH' %in% input$Source3){
            kwh <- data.frame(colSums(data2[, 5:16], na.rm = TRUE))
            colnames(kwh)[1] <- c("Totals")
            kwh$month <- chartMonths
            kwh$month = factor(kwh$month, levels = month.name)
            ggplot(kwh, aes(x = month, y = Totals, fill = month)) + geom_bar(stat = 'identity') + xlab('Month') + ylab('Total Electricity Usage')
        }
        else if ('TOTAL.THERMS' %in% input$Source3){
            therm <- data.frame(colSums(data2[, 20:31], na.rm = TRUE))
            colnames(therm)[1] <- c("Totals")
            therm$month <- chartMonths
            therm$month = factor(therm$month, levels = month.name)
            ggplot(therm, aes(x = month, y = Totals, fill = month)) + geom_bar(stat = 'identity') + xlab('Month') + ylab('Total Gas Usage')
        }
    })
    
    # all functions for the tables
    output$tab1 <- DT::renderDataTable({
        data2 <- subset(data, data$COMMUNITY.AREA.NAME == "Near West Side")
        if ('TOTAL.KWH' %in% input$Source){
            kwh <- data.frame(colSums(data2[, 5:16], na.rm = TRUE))
            colnames(kwh)[1] <- c("Totals")
            kwh$month <- chartMonths
            datatable(kwh, options = list(pageLength = 2), rownames = FALSE)
        }
        else if ('TOTAL.THERMS' %in% input$Source){
            therm <- data.frame(colSums(data2[, 20:31], na.rm = TRUE))
            colnames(therm)[1] <- c("Totals")
            therm$month <- chartMonths
            datatable(therm, options = list(pageLength = 2), rownames = FALSE)
        }
    })
    output$tab2 <- DT::renderDataTable({
        data2 <- subset(data, data$COMMUNITY.AREA.NAME == input$Community)
        if ('TOTAL.KWH' %in% input$Source2){
            kwh <- data.frame(colSums(data2[, 5:16], na.rm = TRUE))
            colnames(kwh)[1] <- c("Totals")
            kwh$month <- chartMonths
            datatable(kwh, options = list(pageLength = 2), rownames = FALSE)
        }
        else if ('TOTAL.THERMS' %in% input$Source2){
            therm <- data.frame(colSums(data2[, 20:31], na.rm = TRUE))
            colnames(therm)[1] <- c("Totals")
            therm$month <- chartMonths
            datatable(therm, options = list(pageLength = 2), rownames = FALSE)
        }
    })
    output$tab3 <- renderDataTable({
        data2 <- subset(data, data$COMMUNITY.AREA.NAME == input$Community2)
        if ('TOTAL.KWH' %in% input$Source3){
            kwh <- data.frame(colSums(data2[, 5:16], na.rm = TRUE))
            colnames(kwh)[1] <- c("Totals")
            kwh$month <- chartMonths
            datatable(kwh, options = list(pageLength = 2), rownames = FALSE)
        }
        else if ('TOTAL.THERMS' %in% input$Source3){
            therm <- data.frame(colSums(data2[, 20:31], na.rm = TRUE))
            colnames(therm)[1] <- c("Totals")
            therm$month <- chartMonths
            datatable(therm, options = list(pageLength = 2), rownames = FALSE)
        }
    })

    
}

# Run the application 
shinyApp(ui = ui, server = server)
