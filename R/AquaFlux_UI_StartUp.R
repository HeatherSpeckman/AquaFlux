#####################################################################
#####################################################################
#             UI Functions: Start Up Panel                              ##
#####################################################################
#####################################################################
# The below are AquaFlux ui commands
# only UI commands are found here


.test.GuiDir = function(a, lab, v){
print("lilo test2")
  # get the file path
  b = unlist(a[1])
  b0 = paste0(b, collapse = "/")
  b1 = b0
  unix.start = "~/"
  windows.start = "~/"
  if ( .Platform$OS.type=="unix"){ b1= paste0(unix.start,b0)}
  if ( .Platform$OS.type=="windows"){   print("windows"); print(b1) } #lilo
  # paste0(windows.start,b0)
#stop("PROGRAM THIS FOR WINDOWS");
  
  # test if it's a good path
  good.path = file.exists(b1)
  # if it's a good path, use it
  if (good.path==T & b1!=unix.start & b1!=windows.start){ #lilo-- add start for windows
    if (lab=="GuiDirSave"){
      v$setup.SiteType = paste( "Save directory accepted:", b1)
      v$AquaFlux.work.dir = b1
    }
    if (lab=="GuiDirMet"){
      v$setup.SiteType = paste( "Met directory accepted:", b1)
      v$accepted.met.dir = T
      v$met.dir = b1
    }
    if (lab=="GuiDirDT"){
      v$setup.SiteType = paste( "dT directory accepted:", b1)
      v$accepted.dt.dir = T
      v$dt.dir = b1
    }
  } #else
  # export
  v
}




#####################################################################
#####################################################################
# Start Up Panel: First questions
#####################################################################
#####################################################################

#######################################
###### Background
#######################################

########### .dir.declare.save

.dir.declare.save.man = shiny::textInput('AquaFlux.work.dir', "Where do you want AquaFlux to save stuff?", value = "<Copy & Paste file path>")
.dir.declare.save.nav = shinyFiles::shinyDirButton("GuiDirSave", "Or navigate to directory", "Upload")
.dir.declare.save =   shiny::fluidPage(
  shiny::tags$div(.dir.declare.save.man,  style="display:inline-block"),
  shiny::tags$div(.dir.declare.save.nav,  style="display:inline-block")
  # tags$div(.dir.declare.save.man,  style="display:inline-block"),
  # tags$div(.dir.declare.save.nav,  style="display:inline-block")
)

#######################################
###### Actual UI
#######################################




#####################################################################
#####################################################################
# Start Up Panel: New Site, basic questions (formally .ui.setup.1)
#####################################################################
#####################################################################


#######################################
###### Background
#######################################
#
.delim.options = c("Comma" = ",",
                   "Tab" = "/t",
                   "Semicolon " = ";",
                   "Space" = " ")


########### .dir.declare.met
.dir.declare.met.man = shiny::textInput('met.dir', "Where is your meteorological data?", value = "<Copy & Paste file path>")
.dir.declare.met.nav = shinyFiles::shinyDirButton("GuiDirMet", "Or navigate to directory", "Upload")
.dir.declare.met =   shiny::fluidPage(
  shiny::tags$div(.dir.declare.met.man,  style="display:inline-block"),
  shiny::tags$div(.dir.declare.met.nav,  style="display:inline-block")
)


########### .dir.declare.met
.dir.declare.dt.man = shiny::textInput('dt.dir', "Raw dT Directory", value = "<Copy & Paste file path>")
.dir.declare.dt.nav = shinyFiles::shinyDirButton("GuiDirDT", "Or navigate to directory", "Upload")
.dir.declare.dt =   shiny::fluidPage(
  shiny::tags$div(.dir.declare.dt.man,  style="display:inline-block"),
  shiny::tags$div(.dir.declare.dt.nav,  style="display:inline-block")
)


#######################################
###### Actual UI
#######################################


#####################################################################
#####################################################################
# Start Up Panel: New Site, 2nd questions about units and labels (formally .ui.setup.2)
#####################################################################
#####################################################################

#######################################
###### Background
#######################################

########### menu options for units
.air.temp.units.options = c("C" = "C",
                            "F" = "F",
                            "K" = "K",
                            "<NA>" = "NA")
.dT.units.options = c("mV" = "mV",
                      "C" = "C",
                      "F" = "F",
                      "K" = "K")
.RH.units.options = c("%" = "%",
                      "Decimal", "Decimal",
                      "<NA>" = "NA")

.timestamp.options = c("<select>" = "none",
                       '%Y-%m-%d %H:%M'= '%Y-%m-%d %H:%M',
                       '%Y-%d-%m %H:%M'= '%Y-%d-%m %H:%M',
                       '%d/%m/%Y %H:%M'= '%d/%m/%Y %H:%M',
                       '%m/%d/%y %H:%M'= '%m/%d/%y %H:%M',
                       '%d/%m/%y %H:%M'= '%d/%m/%y %H:%M',
                       "<manually add>" = "manual")


.start.up.ui.questions =     shiny::fluidPage(
  .dir.declare.save,
  shiny::tags$div(shiny::verbatimTextOutput("setup.SiteType")),
  shiny::actionButton(inputId="launch.load", label="Load Exisiting Site (takes a minute)"),
  shiny::actionButton(inputId="launch.new", label="Start New Site")
)

################################################################
################################################################
# Start Up Panel: MASTER
################################################################
################################################################


.ui.setup.site =    shiny::fluidPage(
 shiny::uiOutput('start.up.questions'),
 shiny::uiOutput('start.up.warning'),
 shiny::uiOutput('start.up.loading'),
 shiny::uiOutput('ui.setup.1'),
 shiny::uiOutput('ui.setup.2'),
 shiny::uiOutput('ui.setup.done')
)







