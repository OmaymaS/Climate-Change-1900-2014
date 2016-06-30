#Load Libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(purrr)
library(tidyr)
library(mgcv)

#Load data
cmodels_details<-readRDS("./cmodels_details")

shinyServer(function(input, output) {
        
        output$overview<-renderText("This Shiny App provides a fast and easy way to explore the \"Earth Surface Temperature Data\" published on Kaggle and compiled originally by \"Berkeley Earth\". 
                                    Here you can plot the average temprature over time from 1900-2014 in 100 major cities.
                                    The model used is a Generalized Additive Model(GAM)")
        
        
        #read the 100 cities names (the unique values)
        CityNames<-unique(cmodels_details$City) 
        
        #Cities names list
        output$CitySelector<-renderUI({
                selectInput('cities', 'City',
                            CityNames, 
                            multiple=TRUE, 
                            selectize=TRUE, 
                            selected="Jakarta") #default value
        })
        
        #Months abbreviation list
        output$MonthSelector<-renderUI({
                selectInput('months', 'Month', 
                            set_names(c(1:12),month.abb), 
                            multiple=TRUE, 
                            selectize=TRUE,
                            selected=1) #default January
        })
        
        #get the selected cities
        SelectedCity<-reactive({
                
                if(is.null(input$cities) || length(input$cities)==0)
                        return()
                as.vector(input$cities)
                
        })
        
        #get the selected month
        SelectedMonth<-reactive({
                
                if(is.null(input$months) || length(input$months)==0)
                        return()
                as.numeric(as.vector(input$months))
                
        })
        

        #filter the data according to the selected city and month/s
        citiesDF<-reactive({
                        cmodels_details %>%
                        unnest(data)%>%
                        filter(City %in% SelectedCity()) %>%
                        filter(Month %in% SelectedMonth())
        }) 
        
        output$ff <- renderPrint({
                names(citiesDF())
        })
        
        #get Check group input (type of plot)
        checkedVal <- reactive({
                as.vector(input$checkPlot)
             
        }) 
        
        ############PLOT#########
        output$RegPlot<-renderPlot({
                #check if city and month are not null
                if ((length(SelectedCity())>0) && (length(SelectedMonth())>0))
                        
                        {g<-ggplot(citiesDF(),
                                   aes(x=dt,y=AverageTemperature,
                                       colour=factor(Month)))+
                                labs(x="Year",
                                     y="Average Temperature")+
                                facet_wrap(~City)+
                                scale_color_discrete(name="Month",
                                                     breaks=c(1:12),
                                                     labels=month.abb)
                        
                        if ("GAM Plot" %in% checkedVal())

                                g<-g+stat_smooth(method="gam", formula=y~s(x),se=FALSE)
                        
                        if ("Point Plot" %in% checkedVal())
                                
                                g<-g+geom_point(aes(alpha=0.4))+
                                        guides(alpha=FALSE)
                        
                        g
                                               }
        })
        #########################
})