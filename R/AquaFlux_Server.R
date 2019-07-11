


#####################################################################
#####################################################################
#####################################################################
#             Server                                      ##
#####################################################################
#####################################################################
#####################################################################



.AquaFlux.server <- function(input, output, session) {


  v <- shiny::reactiveValues(
    ####### File name tags  -- used to name AquaFlux's saves of data
    tag.dT.raw = "All dT Data Unfiltered",
    tag.dT.clean = "Sapflux dT cleaned- Current",
    tag.flag = "All Sapflux Flags- Current",
    tag.Tmax = "Tmax- Current",
    tag.flux = "Sapflux- Current",
    tag.log.deletion = "Deletion Log- Current",
    tag.site.matadata = "Site MetaData.csv",
    #### standard time format
    standard.time.format  = '%Y-%m-%d %H:%M', # Campbell default.
    alternate.time.format = '%m/%d/%y %H:%M', # Excel default
    third.time.format = '%d/%m/%Y %H:%M', # Excel default
    R.time.format         = "%Y-%m-%d %H:%M", # R default
    ######### doy
    min.DOY = 0,
    max.DOY = 367,
    min.DOY.global = 0,
    max.DOY.global = 367,
    number.of.saves.to.keep = 10,
    ####################### opening blank values these are advance options
    max.gap.length = 8,
    min.number.of.columns.in.a.data.file = 2,
    ################ these are from your data
    nonsapflux.columns = NA,
    AquaFlux.work.dir = "<Copy & Paste file path>",
    site.names = "MissingSiteName",
    number.of.sites = 1,
    n.met.data="<select>",
    n.dT.data = "<select>",
    importOption = "NoSite",
    accepted.met.dir = F,
    accepted.dt.dir = F,
    ################   for the "Set up" panel
    setup.SiteType = "",
    StartButton = "empty",
    SetupStep = 1,
    setup.done = F,
    Setup1 = "no",
    setup.load.message = " ",
    setup.SiteType = " ",
    ################# import
    import.status = "not.done",
    sapflux.names = NA,
    # plot
    MinDT_brokensensor = 0,
    VPD.thres = 0.2,
    tree.number = 1,
    b.range = NA,
    tree.name = "none",
    move.dir = "none",
    pick.plot.options = "none",
    LDate.local = c(0,400),
    dT.local = c(0,1),
    restore.options = "none",
    i.need.for.Tmax.auto = NA,
    autoQAQCmes = " ",
    LastAutoFilter= "none",
    export.done = F,
    plotAutoQAQC = F,
    BrokenSensors.mes.na = "<not ran yet>",
    BrokenSensors.mes.sd = "<not ran yet>",
    BrokenSensors.mes.low = "<not ran yet>",
    #### QAQC
    #  qaqc.1orAll = "False",
    ######## export stuff
    export.done = F,
    export.mes.1=" ",
    export.mes.2=" ",
    export.mes.3=" ",
    export.mes.4=" ",
    export.mes.5=" ",
    export.mes.6=" ",
    export.mes.7=" ",
    export.mes.8=" ",
    export.mes.9=" ",
    export.mes.10=" ",
    time.last.save = Sys.time()
  )








  #   ##############################################
  #   #       GUI folder navigate         ##
  #   ##############################################
  #   # GuiDir
  shinyFiles::shinyDirChoose(input, 'GuiDirSave',  roots = c(home=paste0("/", unlist(strsplit( getwd(), "/"))[2])) )
  shinyFiles::shinyDirChoose(input, 'GuiDirMet',  roots = c(home=paste0("/", unlist(strsplit( getwd(), "/"))[2])) )
  shinyFiles::shinyDirChoose(input, 'GuiDirDT' ,  roots = c(home=paste0("/", unlist(strsplit( getwd(), "/"))[2])) )
  output$GuiDir <- shiny::renderPrint(shiny::reactive(input$GuiDirSave))
  output$GuiDir <- shiny::renderPrint(shiny::reactive(input$GuiDirMet))
  output$GuiDir <- shiny::renderPrint(shiny::reactive(input$GuiDirDT))

  
  #### observe the paths
  shiny::observe({ a = input$GuiDirSave; lab="GuiDirSave"; v = .test.GuiDir(a, lab, v ) } ) # paste0("/",unlist(strsplit(getwd(),"/"))[2]) 
  shiny::observe({ a = input$GuiDirMet; lab="GuiDirMet"; v = .test.GuiDir(a, lab, v    ) } )
  shiny::observe({ a = input$GuiDirDT; lab="GuiDirDT"; v = .test.GuiDir(a, lab, v ) } )




  ##############################################
  #       Set up functions - step 0          ##
  ##############################################

  shiny::observe({ v$AquaFlux.work.dir = input$AquaFlux.work.dir; })
  shiny::observe({v$StartButton  = input$StartButton})
  output$start.up.questions <- renderUI({ if ( v$import.status == 'not.done') { .start.up.ui.questions  } })
  output$output.message <- renderText({ v$setup.SiteType })
  output$start.up.warning <- renderUI({ if ( v$import.status == 'WARNING') {
    shiny::fluidPage(
      shiny::verbatimTextOutput("output.message"),
      shiny::actionButton( "NotSure", "No!  Don't delete my files!"),
      shiny::actionButton( "IAmSure", "That's ok, I want to start fresh.  Delete my files")
    ) } })

  output$start.up.loading <- renderUI({ if ( v$import.status == 'Loading') {
    shiny::fluidPage(
      shiny::verbatimTextOutput("output.message")
    ) } })

  shiny::observeEvent(input$NotSure,{ v$import.status <- "not.done"; v$setup.SiteType="Please enter your save directory" })
  shiny::observeEvent(input$IAmSure,{ v = .IAmSureNewSite(v) })

  shiny::observeEvent(input$launch.load,{
    v$import.status <- "Loading";
    v$setup.SiteType=paste("Loading your old and new data...")
    v= .launch.load(v) #
  })

  shiny::observeEvent(input$launch.new,{ v= .launch.new(v) })
  output$setup.SiteType <- renderText({ v$setup.SiteType })



  #   ##############################################
  #   #       Set up functions - step 1          ##
  #   ##############################################
  .dir.declare.met.full = renderUI({
    shiny::fluidPage(
      if ( v$accepted.met.dir == F) {   .dir.declare.met  },
      if ( v$accepted.met.dir == T) {  shiny::helpText("Met directory accepted")  }
    )
  })
  .dir.declare.dt.full = renderUI({
    shiny::fluidPage(
      if ( v$accepted.dt.dir == F) {   .dir.declare.dt  },
      if ( v$accepted.dt.dir == T) {  shiny::helpText("Site directory accepted")  }
    )
  })

  output$ui.setup.1 = renderUI({ if ( v$import.status == 'ReadyForNew1') {
    shiny::fluidPage(
      .dir.declare.met.full,
      shiny::helpText("Note: if your met and dT data are saved in the same files, paste that directory in both fields"),
      shiny::selectInput("delim.sep", "Data Delimiter", .delim.options),
      shiny::numericInput("number.of.before.headers", "How many lines before your data headers?", min=0,max=30, value=1),
      shiny::numericInput("number.of.lines.before.data", "How many lines before your data proper?", min=1,max=30, value=4),
      shiny::textAreaInput("site.names", "Site Names", "<Site name, 1 on each line>", width = "400px"),
      .dir.declare.dt.full, #
      shiny::actionButton( "Setup1", "Check my work (this will take a minute)"),
      shiny::verbatimTextOutput(v$output.message)
    ) } })

  #   ##############################################
  #   #       Set up functions - step 2          ##
  #   ##############################################
  shiny::observe({ v$met.dir = input$met.dir})
  shiny::observe({ v$delim.sep = input$delim.sep})
  shiny::observe({ v$number.of.before.headers = input$number.of.before.headers })
  shiny::observe({ v$number.of.lines.before.data = input$number.of.lines.before.data})
  shiny::observe({ v$site.names = input$site.names})
  shiny::observe({ v$dt.dir = input$dt.dir  })
  shiny::observeEvent(input$Setup1,{    v=.test.setup1(v) })



  ##############################################
  #       Set up functions - step 2          ##
  ##############################################
  shiny::observe({ v$study.year = input$study.year})
  shiny::observe({ v$selected.timestamp.format = input$selected.timestamp.format })

  output$pick.dT.ts.name <- renderUI({
    colnames = c("<select>", v$n.dT.data)
    shiny::selectInput("dT.ts.name", "Timestamp Name in dT data",
                       choices  = colnames,
                       selected = as.character(v$import.dT.ts.name) )
  })
  shiny::observe({ v$dT.ts.name = input$dT.ts.name})
  output$pick.met.ts.name <- renderUI({
    colnames = c("<select>", v$n.met.data)
    shiny::selectInput("met.ts.name", "Timestamp Name in Met. data",
                       choices  = colnames)
  })
  shiny::observe({ v$met.ts.name = input$met.ts.name})
  shiny::observe({v$dT.units = input$dT.units })
  output$pick_AirT_name <- renderUI({
    colnames = c("<select>", "<No Air T data>", v$n.met.data)
    shiny::selectInput("met.air.temp.label", "Air Temp = ?",
                       choices  = colnames)
  })
  shiny::observe({ v$met.air.temp.label = input$met.air.temp.label})
  shiny::observe({v$met.air.temp.units = input$met.air.temp.units })
  output$pick_RH_name <- renderUI({
    colnames = c("<select>", "<No RH data>", v$n.met.data)
    shiny::selectInput("met.RH.label", "Relative Humidity = ?",
                       choices  = colnames)
  })
  shiny::observe({ v$met.RH.label = input$met.RH.label})
  shiny::observe({v$met.RH.units = input$met.RH.units })
  output$choose_nonsapflux <- renderUI({
    colnames <- v$n.dT.data
    shiny::checkboxGroupInput("nonsapflux.columns", "Choose non-sap flux columns",
                              choices  = colnames)
  })
  shiny::observe({ v$nonsapflux.columns = input$nonsapflux.columns})
  shiny::observeEvent(input$finish.set.up,{
    ########### time
    .save.site.metadata(v)
    v = .finish.importing(v)
    v$setup.load.message = "Loaded in your site and data.  Continue on next panel."
    v$setup.done = T
    v$import.status = "done"
  })
  output$setup.load.message <- renderText({
    v$setup.load.message
  })

  output$ui.setup.2 <- renderUI({ if ( v$import.status == 'ReadyForNew2') {
    shiny::fluidPage(
      shiny::helpText("Questions about your time format (see 'strptime' and manual for help)"),
      shiny::textInput('study.year', "Study Year (can only do 1 year at a time)", value = "<Enter Study Year>"),
      shiny::selectInput("selected.timestamp.format", "Common Timestamp Format", .timestamp.options),
      shiny::uiOutput("pick.dT.ts.name"),
      shiny::uiOutput("pick.met.ts.name"),
      shiny::br(),
      shiny::helpText("Questions about units"),
      shiny::selectInput("dT.units", "What are the dT units?", .dT.units.options),
      shiny::uiOutput("pick_AirT_name"),
      shiny::selectInput("met.air.temp.units", "Air Temp. Units", .air.temp.units.options, selected="mV"),
      shiny::uiOutput("pick_RH_name"),
      shiny::selectInput("met.RH.units", "Relative Humidity Units", .RH.units.options),
      uiOutput("choose_nonsapflux"),
      conditionalPanel( "v$import.status = 'start.new'" , {
        actionButton(inputId="finish.set.up", label="Finish Site Set Up (takes a minute)") }),
      verbatimTextOutput("setup.load.message")
    ) } })


  output$ui.setup.done <- renderUI({ if ( v$import.status == 'done') {
    shiny::fluidPage(
      shiny::helpText("Data Sucessfully Imported!  Start processing.")
    ) } })
  ##############################################
  #   Broken Sensors        ##
  ##############################################
  shiny::observeEvent(input$FindBrokenSensors,{  v$BrokenSensorReports = .BrokenSensors.detect(v)  })
  output$BrokenSensors.mes.na <- renderText({ v$BrokenSensors.mes.na })
  output$BrokenSensors.mes.low <- renderText({ v$BrokenSensors.mes.low })
  output$BrokenSensors.mes.sd <- renderText({ v$BrokenSensors.mes.sd })

  ##############################################
  #   Processing col 1: Generic Plot Data         ##
  ##############################################
  # pick a tree
  output$tree.name <- renderUI({
    colnames = c(v$sapflux.names)
    # Create the checkboxes and select them all by default
    shiny::selectInput("tree.name", "Pick a sensor",
                       choices  = colnames)
  })
  observe({ v$tree.name = input$tree.name;
  v = .get.local.data(v);
  })
  # doy range stuff
  observe({ v$min.DOY = input$DOY.range[1] })
  observe({ v$max.DOY = input$DOY.range[2] })
  observe({ shiny::updateSliderInput(session, "DOY.range", value = c(v$min.DOY,v$max.DOY),
                                     min = v$min.DOY.global , max = v$max.DOY.global  )
  })
  # move doy
  observeEvent(input$b.move.back, {   v$move.dir= "back"; v=.move.window(v);   })
  observeEvent(input$save.data, {   .save.AquaFlux(v);   })
  observeEvent(input$b.move.foward, {  v$move.dir= "foward"; v=.move.window(v);   })
  ##############################################
  #   Processing col 2: Pick processing options        ##
  ##############################################
  ########### pick plot  options
  observe({ v$pick.plot.options = input$pick.plot.options})
  observe({ shiny::updateCheckboxGroupInput(session, "pick.plot.options", selected = v$pick.plot.options) })
  ########### pick processing  options
  observe({ v$processOption = input$processOption;
  if(v$processOption=="tmax"){  .Tmax.turn.on.plotting(v) } })
  ##############################################
  #   Processing col 3- QAQC auto   ##
  ##############################################
  # actually suggest
  observeEvent(input$auto.clean, {  v = .autoQAQC.suggest(v) })
  # options
  observe({ v$MinDT = as.numeric(as.character(input$MinDT)) })
  observe({ v$MaxDT = as.numeric(as.character(input$MaxDT)) })
  observe({ v$DT.despike.dev = as.numeric(as.character(input$DT.despike.dev)) })
  # output message
  output$autoQAQCmes <- renderText({  autoQAQCmes = v$autoQAQCmes })
  # Accept / reject
  observeEvent(input$b.AutoQAQC.keep, {  v = .autoQAQC.keep(v)    })
  observeEvent(input$b.AutoQAQC.del, {   v =  .autoQAQC.del(v) })
  ##############################################
  #   Processing col 3- QAQC manual   ##
  ##############################################
  observeEvent(input$qaqc.manual, {
    v$plotAutoQAQC = F;v$tmaxRadio="none";v$restore.option="none"; # clear other bottons
    v$qaqc.manual <- input$qaqc.manual;
  })
  observe({ shiny::updateRadioButtons(session, "qaqc.manual", selected = v$qaqc.manual) })
  # observe({ v$qaqc.1orAll = input$qaqc.1orAll})
  ##############################################
  #   Processing col 3- Calib ##
  ##############################################
  observe({ v$alpha = as.numeric(as.character(input$alpha)) })
  observe({ v$beta = as.numeric(as.character(input$beta)) })


  ##############################################
  #   Processing col 3- Tmax ##
  ##############################################
  # generic bottons

  observe({ v$VPD.thres = as.numeric(as.character(input$VPD.thres)); })
  observe({ v$VPD.below.thres.for.x.hr = as.numeric(as.character(input$VPD.below.thres.for.x.hr)) })

  # generic bottons

  observe({ v$VPD.below.thres.for.x.hr = as.numeric(as.character(input$VPD.below.thres.for.x.hr)) })
  observe({ v$auto.Tmax.window.size = as.numeric(as.character(input$auto.Tmax.window.size)) })
  observeEvent(input$auto.Tmax, { v =.Tmax.autopick(v) })
  observeEvent(input$tmaxRadio, {
    v$tmaxRadio=input$tmaxRadio
    if (v$tmaxRadio=="none"){} # no nothing
    if (v$tmaxRadio=="manual" | v$tmaxRadio=="delete"){ v$qaqc.manual="none"; v$plotAutoQAQC = F; v$restore.option="none";  }# clear other bottons, then click on graph
  } )
  ##############################################
  #   Processing col 3- Restore   ##
  ##############################################
  observeEvent(input$restore.option, {
    v$restore.option <- input$restore.option;  # save
    if (v$restore.option!="none"){ # if not 'none', reset other bottons
      v$plotAutoQAQC = F;v$qaqc.manual="none";v$tmaxRadio="none";
    }
  })
  observe({ shiny::updateSelectInput(session, "restore.option", selected = v$restore.option) })
  ##############################################
  #            Plot Graphs   ##
  ##############################################
  output$plot.dT <- renderPlot({ ; v$pick.bottun = .plot.dT(v) })
  output$plot.sapflux <- renderPlot({ ; .plot.sapflux(v) })
  ##############################################
  #             Click the dT graph   ##
  ##############################################
  observeEvent(input$dT_plot_click,
               {
                 # auto qaqc - none
                 # manual qaqc -
                 if(v$qaqc.manual == "above"){  v=.delete.above(v,input$dT_plot_click$y);  }
                 if(v$qaqc.manual == "below"){ v=.delete.below(v,input$dT_plot_click$y);  }
                 if(v$qaqc.manual == "before"){ v=.delete.before(v,input$dT_plot_click$x);  }
                 if(v$qaqc.manual == "after"){  v=.delete.after(v,input$dT_plot_click$x);  }
                 if(v$qaqc.manual == "between"){v=.delete.between(v,input$dT_plot_click$x);  }
                 if(v$qaqc.manual == "points"){  v=.delete.points(v,input$dT_plot_click);   }
                 if(v$qaqc.manual == "all"){     v=.delete.all(v);  }
                 if(v$qaqc.manual==  "undo"){   v=.restore.undo(v);  }
                 # Tmax
                 if(v$tmaxRadio=="delete"){  v = .Tmax.delete(v,input$dT_plot_click) }
                 if(v$tmaxRadio=="manual"){  v = .Tmax.pick.manual(v,input$dT_plot_click) }
                 # restore
                 if(v$restore.option=="undo"){  v=.restore.undo(v);  }
                 if(v$restore.option=="between"){  v=.restore.between(v,input$dT_plot_click);  }
                 if(v$restore.option=="all"){   v=.restore.all(v);  }
               }
  )
  ##############################################
  #             Export   ##
  ##############################################
  observeEvent(input$b.export,{
    .export.final.data(v)
    v$export.mes.1 = paste("At",Sys.time(),"AquaFlux data was exported:")
    v$export.mes.2 = paste("CSV data saved to:", v$final.dir)
    v$export.mes.3 = paste( "PDF Graphs saved to:", v$graph.dir)
    v$export.mes.4 = "Data was also exported to base R (global envir):"
    v$export.mes.5 = " 'raw.dT.data' = your raw dT data, in C."
    v$export.mes.6 = " 'dT.data' = your post-QAQC dT data, in C."
    v$export.mes.7 = " 'Tmax.data.points' = the Tmax points you picked."
    v$export.mes.8 = " 'Tmax.data.line' = the Tmax line (interpolated from Tmax.data.points)"
    v$export.mes.9 = " 'sapflux.data' = your sap flux dT data, in g s-1 m-2.sapwood"
    v$export.mes.10 = " 'met.data' = your met data, including VPD."
  }) #
  output$export.mes.1 <- renderText({ v$export.mes.1 })
  output$export.mes.2 <- renderText({ v$export.mes.2 })
  output$export.mes.3 <- renderText({ v$export.mes.3 })
  output$export.mes.4 <- renderText({ v$export.mes.4 })
  output$export.mes.5 <- renderText({ v$export.mes.5 })
  output$export.mes.6 <- renderText({ v$export.mes.6 })
  output$export.mes.7 <- renderText({ v$export.mes.7 })
  output$export.mes.8 <- renderText({ v$export.mes.8 })
  output$export.mes.9 <- renderText({ v$export.mes.9 })
  output$export.mes.10 <- renderText({ v$export.mes.10 })


  ########################################
  # v$processOption == 'autoqaqc
  output$Aqaqc_pro_minDT <- renderUI({ if ( v$processOption == 'autoqaqc'){ shiny::textInput('MinDT', "Min accepted dT", value = "10") } } )
  output$Aqaqc_pro_maxDT <- renderUI({ if ( v$processOption == 'autoqaqc'){ shiny::textInput('MaxDT', "Max accepted dT", value = "30") } })
  output$Aqaqc_dt_spike  <- renderUI({ if ( v$processOption == 'autoqaqc') { shiny::textInput('DT.despike.dev', "Max Deviance (for despike)", value = "12")}})
  output$Aqaqc_auto.clean<- renderUI({ if ( v$processOption == 'autoqaqc') { shiny::actionButton(inputId="auto.clean", label="Suggest new QAQC")}})
  output$Aqaqc_autoQAQCmes<- renderUI({ if ( v$processOption == 'autoqaqc') { shiny::verbatimTextOutput("autoQAQCmes") }})
  output$Aqaqc_Delete <- renderUI({ if ( v$processOption == 'autoqaqc') { shiny::actionButton(inputId="b.AutoQAQC.del", label="Delete") }})

  # v$processOption == 'manqaqc
  output$Mqaqc_1orAll <- renderUI({ if ( v$processOption == 'manqaqc') { shiny::checkboxGroupInput("qaqc.1orAll", label=" ", choices  = c("Apply to *ALL* trees-- can NOT 'undo delete' " = "all"))   }})
  output$Mqaqc_manual <- renderUI({ if ( v$processOption == 'manqaqc') { shiny::radioButtons( "qaqc.manual", "Manual QAQC Options:", .qaqc.button.vector,selected = "none",inline=F) }})

  # v$processOption == 'tmax
  output$tmax_VPDthres <- renderUI({ if ( v$processOption == 'tmax') { shiny::textInput('VPD.thres', "VPD Threshold (kPa)", value = 0.2) }})
  output$tmax_VPDhr <- renderUI({ if ( v$processOption == 'tmax') { shiny::textInput('VPD.below.thres.for.x.hr', "VPD must be below thres for X hrs", value = "2") }})
  output$tmax_Tmaxsize <- renderUI({ if ( v$processOption == 'tmax') { shiny::textInput('auto.Tmax.window.size', "Window size (days)", value = "7") }})
  output$tmax_AutoTmax <- renderUI({ if ( v$processOption == 'tmax') { shiny::actionButton(inputId="auto.Tmax", label="Auto Pick Tmax") }})
  output$tmax_radio <- renderUI({ if ( v$processOption == 'tmax') { .tmaxRadio }})

  # v$processOption == 'restore
  output$restore_picker <- renderUI({ if ( v$processOption == 'restore') { .restore.picker  }})

  # v$processOption == 'calib
  output$calib_alpha <- renderUI({ if ( v$processOption == 'calib') { shiny::textInput('alpha', "alpha", value =  118.99 * 10^(-6) )   }})
  output$calib_beta <- renderUI({ if ( v$processOption == 'calib') { shiny::textInput('beta', "beta", value =  1.231) }})

}





