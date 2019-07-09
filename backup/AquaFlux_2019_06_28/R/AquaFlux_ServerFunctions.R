#####################################################################
#####################################################################
#             Server Functons                             ##
#####################################################################
#####################################################################
# The below are funcitons used in the server and the server proper 


#####################################################################
#              Establish base v  reactiveValues                           ##
#####################################################################

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
  # doy
  min.DOY = 0,
  max.DOY = 367,
  min.DOY.global = 0,
  max.DOY.global = 367,
  number.of.saves.to.keep = 10,
  ####################### these are advance options
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
  MinDT = 0,
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



# ##############################################
# #       Set up functions - launch.import          ##
# ##############################################
## Active when:
# 1) You observed the launch.import botton being hit, aka the person wants to import data

.check.dir.exists = function(filepath){
  # .check.dir.exists.  Program will not continue if you don't exist.
  if ( dir.exists(filepath)==F ){
    m = paste( "ERROR: The following folder does not exist:", filepath )
   # stop()
  } else {
    m = T
  }
  m
}

.is.site.ready.to.load= function(v){
  ###### Check that pathways exist:
  # work.dir
  m = .check.dir.exists(v$AquaFlux.work.dir)
  if (m!=F){
    v$setup.SiteType = m
  }
  if (m==T){
    save.dir = paste(v$AquaFlux.work.dir, "/Current Saves", sep="")
    m = .check.dir.exists(save.dir)

    if (m!=T){
      v$setup.SiteType = m
    }
    if (m==T){
      # ccheck that the file is in there
      if (m==T){
        file.name = v$tag.site.matadata
        file.name = paste0(save.dir,"/",file.name)
        my.file.exists = file.exists(file.name)
        if (my.file.exists==F){
          v$setup.SiteType = paste( "ERROR: The following file not found:", file.name, "in folder", save.dir )
        }
        if (my.file.exists==T){
          v$setup.SiteType = "ready_to_load"
        }
      }
    }
  }
  # export
  v
}



.check.save.dir.has.stuff = function(v){
  ###### Check that pathways exist:
  # work.dir
  m = .check.dir.exists(v$AquaFlux.work.dir)
  if (m!=T){
    v$setup.SiteType = m
  }

  # ok, does the save.dir exist?
  if (m==T){
    save.dir = paste(v$AquaFlux.work.dir, "/Current Saves", sep="")
    m = .check.dir.exists(save.dir)

    if (m!=T){
      v$setup.SiteType = "Starting a new site"
      v$import.status = "ReadyForNew1"
    }

    # if dir exsits, is it populated?
    if (m==T){
      m.dir = dir(save.dir)
      if (length(m.dir)>0){
        v$setup.SiteType = paste("WARNING: this will delete files in this folder-", save.dir)
        v$import.status = "WARNING"
      } else {
        v$setup.SiteType = "Starting a new site"
        v$import.status = "ReadyForNew1"
      }
    }
  }
  # export
  v
}

.IAmSureNewSite = function(v){
  # delete files
  save.dir = paste(v$AquaFlux.work.dir, "/Current Saves", sep="")
  x = dir(save.dir, full.names=T)
  file.remove(x)
  # start fresh
  v = .launch.new(v)
  v
}

.launch.new = function(v){
  v = .check.save.dir.has.stuff(v)
  print(v$setup.SiteType)#lilo
  print("lilo- in .launch.new done")
  v
}


.launch.load = function(v){
  .is.site.ready.to.load(v)
  if (v$setup.SiteType=="ready_to_load"){
    ####### at this point we have found the data and can load it
    v = .load.site.metadata(v)  #
    v = .finish.importing(v)
    v$setup.SiteType = "Loaded in your site and data.  Continue on next tab."
    v$import.status = "done"
  }
  v
}

.test.setup1 = function(v){
  v = .setup1.finish(v)
  v
}


# ##############################################
# #       Render UI funcitons       ##
# ##############################################



