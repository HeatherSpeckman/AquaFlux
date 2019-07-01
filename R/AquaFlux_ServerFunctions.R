
#####################################################################
#####################################################################
#             Server Functons                             ##
#####################################################################
#####################################################################
# The below are funcitons used in the server and the server proper


#####################################################################
#              Establish base v  reactiveValues                           ##
#####################################################################


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
  print(v$setup.SiteType)#
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
