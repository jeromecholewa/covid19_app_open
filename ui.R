#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel("Plot graphs related to the Covid-19 pandemic"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      h5("Questions? Souhaits? Bug? Contactez jeromecholewa@gmail.com"),
      # textOutput("FILE"),
      # textOutput("errorMessage"),

      br(),
      # radioButtons("new_or_cumu", "Cas cumulés ou nouveaux cas/jr:",
      #              c("Cumulés" = "",
      #                "Nouveaux cas par jr" = "_new")),
      #br(),
      # radioButtons("ppermillion", "Cas par million?",
      #              c("Valeurs absolues" = FALSE,
      #                "Cas par million" = TRUE)),
      selectInput("country_chosen",
                  label = "Choississez un ou des pays",
                  selected = "France", multiple = TRUE, choices = pays),
      br(), br(), br(),br(), br(),
      submitButton("Mettre à jour"),
      br(), br()
    ),

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Courbes principales",
                           fluidRow(column(3, checkboxInput("confirmed",
                                                            "Cas Confirmés", TRUE)),
                                    column(3,
                                           checkboxInput("recovered",
                                                            "Rémissions", FALSE)),
                                    column(5,
                                           checkboxInput("deaths","Décès", FALSE))),
                           fluidRow(column(6,
                                           radioButtons("new_or_cumu", "Cas cumulés ou nouveaux cas/jr:",
                                                        inline = TRUE,
                                                        c("Cumulés" = "",
                                                          "Nouveaux cas par jr" = "_new"))),
                                    column(6,
                                           radioButtons("ppermillion",
                                                          "Cas par million?",
                                                          inline = TRUE,
                                                          c("Valeurs absolues" = FALSE,
                                                            "Cas par million" = TRUE)))),
                           #
                           h5(paste0("Données mises à jour le ", update_datetime)),
                           h3(htmlOutput("errorMessage")),
                           plotlyOutput("Plot_ly1"),
                           uiOutput("tab"),
                           # textOutput("countries_graph"),
                           # textOutput("texti")
                           #plotOutput("Plot1")

                  )
                  ,
                  tabPanel("Jour zéro - décès",
                           h5(paste0("Données mises à jour le ", update_datetime)),
                           h5("Le jour zéro est quand le pays a passé pour la première fois la barre des 10 décès"),
                           plotlyOutput("Plot_ly_deces_jour0"),
                           uiOutput("tab2"),
                           br(),
                           h5("Le jour zéro est quand le pays a passé pour la première fois la barre des 0.15 décès par million"),
                           h3(htmlOutput("errorMessage2")),
                           plotlyOutput("Plot_ly_deces_jour0_million"),
                  )
                  ,
                  tabPanel("Jour zéro - Cas confirmés",
                           h5(paste0("Données mises à jour le ", update_datetime)),
                           h5("Le jour zéro est quand le pays a passé pour la première fois la barre des 30 cas confirmés"),
                           plotlyOutput("Plot_ly_confirmes_jour0"),
                           uiOutput("tab3") ,
                           br(),
                           h5("Le jour zéro est quand le pays a passé pour la première fois la barre des 0.50 cas confirmés par million"),
                           h3(htmlOutput("errorMessage3")),
                           plotlyOutput("Plot_ly_confirmes_jour0_million")
                  )
      )
    )
  )
))
