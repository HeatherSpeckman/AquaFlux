
#####################################################################
#####################################################################
#             UI Functions: Process dT panel                      ##
#####################################################################
#####################################################################
# The below are AquaFlux ui commands
# only UI commands are found here


#####################################################################
#####################################################################
# Process dT Panel: base functions
#####################################################################
#####################################################################


#######################################
###### Background
#######################################

############## Column 1
.DOY.range =       shiny::sliderInput(inputId = "DOY.range",
                                      label = "DOY range for current window:",
                                      min = 0,
                                      max = 367,
                                      value = c(0,367)
)
############## Column 2
.processing.options =c("<none selected>" = "none",
                       "QAQC - Auto" = "autoqaqc",
                       "QAQC - Manual" = "manqaqc",
                       "Tmax Selection" = "tmax",
                       "Restore Data" = "restore",
                       "Custom Calib" = "calib")
.plot.options.list = c("Use Time Colors" = "use.time.col",
                       "VPD" = "VPD",
                       "Air Temp." = "AirTC",
                       "Raw dT Data" = "raw",
                       "Tmax" = "Tmax",
                       "Plot Sap Flux" = "flux")
.plot.options = shiny::checkboxGroupInput("pick.plot.options", "Extra Variables to plot:",
                                          .plot.options.list, inline = T)
############## auto QAQC
.accept.options = c( "<no answer>" = "none",
                     "Delete" = "Accept",
                     "Keep" = "Regect")


############## manual QAQC
.qaqc.button.vector =      c("<none selected>" = "none",
                             "Delete Above" = "above",
                             "Delete Below" = "below",
                             "Delete Before" = "before",
                             "Delete After" = "after",
                             "Delete Between" = "between",
                             "Delete Points" = "points",
                             "Delete All (click graph to confrim)" = "all")

##############  Tmax
.Tmax.options = c( "<none>" = "none",
                   "Manual Tmax pick" = "manual",
                   "Delete Tmax" = "delete")
.tmaxRadio = shiny::radioButtons( "tmaxRadio", label=" ",
                                  choices=.Tmax.options,
                                  selected = "none",
                                  inline=T)
############## Restore
.restore.options = c( "<none selected>" = "none",
                      "Undo Delete" = "undo",
                      "Restore Between" = "between",
                      "Restore All" = "all")
.restore.picker = shiny::radioButtons("restore.option", "Restore Data (click graph to confrim):", .restore.options, selected = "none")


#######################################
###### Actual UI: auto QAQC
#######################################




############## auto QAQC
.ui.QAQC = shiny::fluidPage(
  shiny::fluidRow(
    ########### doy navigator
    shiny::column(3,
                  # window selector
                  shiny::uiOutput("tree.name"),
                  .DOY.range,
                  shiny::actionButton(inputId="b.move.back", label="Move Back"),
                  shiny::actionButton(inputId="save.data", label="Save"),
                  shiny::actionButton(inputId="b.move.foward", label="Move Foward")
    ),
    ########### pick processing options
    shiny::column(4, offset = 1,
                  .plot.options,
                  shiny::radioButtons( "processOption", "Processing options:",
                                       choices=.processing.options,
                                       selected = "none",
                                       inline=F)
    ),
    ############# sub menu's: conditional panels that show up
    shiny::column(4,
                  # Auto qaqc
                  shiny::uiOutput('Aqaqc_pro_maxDT'),
                  shiny::uiOutput('Aqaqc_pro_minDT'),
                  shiny::uiOutput('Aqaqc_dt_spike'),
                  shiny::uiOutput('Aqaqc_auto.clean'),
                  shiny::uiOutput('Aqaqc_autoQAQCmes'),
                  shiny::uiOutput('Aqaqc_Delete'),

                  # v$processOption == 'manqaqc
                  shiny::uiOutput('Mqaqc_1orAll'),
                  shiny::uiOutput('Mqaqc_manual'),

                  # v$processOption == 'tmax
                  shiny::uiOutput('tmax_VPDthres'),
                  shiny::uiOutput('tmax_VPDhr'),
                  shiny::uiOutput('tmax_Tmaxsize'),
                  shiny::uiOutput('tmax_AutoTmax'),
                  shiny::uiOutput('tmax_radio'),

                  # v$processOption == 'restore
                  shiny::uiOutput('restore_picker'),

                  # v$processOption == 'calib
                  shiny::uiOutput('calib_alpha'),
                  shiny::uiOutput('calib_beta')
    )

  ),
  ############# plots
  shiny::plotOutput("plot.dT", click = "dT_plot_click"),
  shiny::plotOutput("plot.sapflux", click = "flux_plot_click") # } )
)
#

