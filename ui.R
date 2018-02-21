
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
shinyUI(fluidPage(
   headerPanel("Twitter Analysis"),
   sidebarPanel( textInput("term", "Enter a term", ""),
                 sliderInput("count",
                             label = "Number of tweets to load:",
                             min = 0, max = 200, value = 0),
                 submitButton(text="Run")),
   mainPanel(
      h4("Algumas análises"),
      plotOutput("wordcl"),
      plotOutput("plotlocal"),
      plotOutput("plotautor"))
))
