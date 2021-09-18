#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("GNSS-R"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Some settings",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel (tabsetPanel(
            type = "tab",
            tabPanel("Distribution of reflection points", leafletOutput("map")),
            tabPanel("Relextion data", tableOutput("r.table")),
            tabPanel("text", textOutput("txt"))
        ))
    )
))
