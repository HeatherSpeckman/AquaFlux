
#####################################################################
#####################################################################
#             UI Functions: Process dT panel                      ##
#####################################################################
#####################################################################
# The below are AquaFlux ui commands
# only UI commands are found here



#####################################################################
#       Sub-UI: Export
#####################################################################

.ui.export = shiny::fluidPage(
  shiny::fluidRow(
                  shiny::actionButton(inputId="b.export", label="I am done processing my data.  Please export it"),
                  shiny::textOutput("export.mes.1"),
                  shiny::textOutput("export.mes.2"),
                  shiny::textOutput("export.mes.3"),
                  shiny::br(),
                  shiny::textOutput("export.mes.4"),
                  shiny::textOutput("export.mes.5"),
                  shiny::textOutput("export.mes.6"),
                  shiny::textOutput("export.mes.7"),
                  shiny::textOutput("export.mes.8"),
                  shiny::textOutput("export.mes.9"),
                  shiny::textOutput("export.mes.10")
  )
)
