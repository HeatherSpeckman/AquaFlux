
#####################################################################
#####################################################################
#             UI Functions: Start Up Panel                              ##
#####################################################################
#####################################################################
# The below are AquaFlux ui commands
# only UI commands are found here


#####################################################################
#####################################################################
#  UI: Broken Sensors
#####################################################################
#####################################################################

############## auto QAQC

.ui.BrokenSensors = shiny::fluidPage(
  shiny::actionButton(inputId="FindBrokenSensors", label="Find Broken Sensors"),
  shiny::helpText("Looking at the last 5 days of data..."),
  shiny::hr(),
  shiny::helpText("The following sensors have mostly NA data"),
  shiny::htmlOutput("BrokenSensors.mes.na") ,
  shiny::hr(),
  shiny::helpText("The following sensors have really low dT data"),
  shiny::htmlOutput("BrokenSensors.mes.low") ,
  shiny::hr(),
  shiny::helpText("The following sensors have large sd"),
  shiny::htmlOutput("BrokenSensors.mes.sd")
)
