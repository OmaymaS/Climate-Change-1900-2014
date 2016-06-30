#Load Libraries
library(shiny)
library(ggplot2)
library(dplyr)


# Define UI for miles per gallon application
shinyUI(fluidPage(
        
        # Application title
        titlePanel("Climate Change in Major Cities (1900-2014)"),
        
        # Sidebar with controls to select city, month and type of plot
        sidebarLayout(

                sidebarPanel(
                        helpText("Type/Select one or more cities:"),
                        
                        uiOutput("CitySelector"),
                        
                        helpText("Type/Select one or more months:"),
                        uiOutput("MonthSelector"),
                        
                        helpText("Select type of plot:"),
                        checkboxGroupInput("checkPlot", 
                                           label = ("Plots"), 
                                           choices=c("GAM Plot","Point Plot"),
                                           selected = "GAM Plot"
                                         ),
                        
                        helpText("Note that : \"Early data was collected by technicians using mercury thermometers, where any variation in the visit time impacted measurements. In the 1940s, the construction of airports caused many weather stations to be moved. In the 1980s, there was a move to electronic thermometers that are said to have a cooling bias.\""),
                        
                        tags$a(href = "https://www.kaggle.com/berkeleyearth/climate-change-earth-surface-temperature-data", "Source")
       
                ),
                
                #Main Panel contains the plot/s
                mainPanel(

                        textOutput("overview"),
                        plotOutput("RegPlot")
                        # verbatimTextOutput("ff")
                        

                        )
                )
))
