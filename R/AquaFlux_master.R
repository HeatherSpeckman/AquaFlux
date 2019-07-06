


#####################################################################
#####################################################################
#####################################################################
#             AquaFlux Codes                                      ##
#####################################################################
#####################################################################
#####################################################################
# shinythemes,
# shinydashboard,
# shinyjs,
# xts,
# oce,
# timeDate
#####################################################################
#       Master UI
#####################################################################

#####
.AquaFlux.ui = shiny::fluidPage(
  #  useShinyjs(),
  # Title
  shiny::titlePanel("AquaFlux"),
  shiny::navbarPage("AquaFlux",
                    shiny::tabPanel("Set Up Site", {.ui.setup.site }) ,
                    shiny::tabPanel("List Broken Sensors", {.ui.BrokenSensors }),
                    shiny::tabPanel("Process dT Data", {.ui.QAQC }),
                    shiny::tabPanel("Export Sap Flux", {.ui.export })
  )
)



