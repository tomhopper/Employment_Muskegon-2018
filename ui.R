
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggvis)
library(shinythemes)

#shinyUI(
fluidPage(theme = shinytheme("cerulean"),
          
          # Application title
          titlePanel("Muskegon Unemployment Rate"),
          
          fluidRow(
            p("Monthly unemployment data from the Bureau of Labor Statistics, bls.gov, with seasonal adjustment. Shiny app by Thomas Hopper, (c) 2017."),
            htmlOutput("employmentValue"),
            
            # Sidebar with a slider input for number of bins
            sidebarLayout(
              sidebarPanel(
                selectInput(inputId = "component",
                            choices = c("observed","adjusted","seasonal","random"),
                            selected = "observed",
                            label = "Component to graph:"
                )
              ),
              
              # Show a plot of the generated distribution
              mainPanel(
                #plotOutput("distPlot")
                wellPanel(
                  uiOutput("graph_title"),
                  htmlOutput("em_plot_ui")#,
                  #uiOutput("daterange")
                )
              )
            ),
            h3("References"),
            tags$ul(
              tags$li("R Core Team (2016). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna,
  Austria. URL https://www.R-project.org/."),
              tags$li("Winston Chang and Hadley Wickham (2016). ggvis: Interactive Grammar of Graphics. R package version 0.4.3.
  https://CRAN.R-project.org/package=ggvis")
            )
          )
)
#)
