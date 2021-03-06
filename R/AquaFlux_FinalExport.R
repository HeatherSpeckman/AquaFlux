
#############################################################
##             Final Exports Funcitons                    ###
#############################################################
# export to R
# unclick the botton

.export.sapflux = function(v){
  sapflux.data = v$dT.data
  for (tree.name in v$sapflux.names){
    v$tree.name = tree.name
    v$tree.number = match(tree.name,names(sapflux.data))
    v = .get.local.data(v)
    y = .sapflux.calc.local(v,tree.name)
    sapflux.data[,v$tree.number] = y
  }
  # make blank sheet
  y = v$met.data
  z = data.frame(TIMESTAMP=y$TIMESTAMP,LDate=y$LDate, sapflux.data)
  # export
  .save.one.file( z, "Sap flux data- Exported", is.Tmax=F, v)
  sapflux.data <<- z
  sapflux.data
}
.export.Tmax = function(Tmax.data,LDate, sapflux.names, dT.data, v){
  if (nrow(Tmax.data)>1){
    Tmax.data = Tmax.data[is.na(Tmax.data$Name)==F,]
    ########## points
    # make it in the right order
    right.order = order(Tmax.data$Name,Tmax.data$LDate)
    Tmax.data = Tmax.data[right.order,]
    # export points
    .save.one.file( Tmax.data, "Tmax points- Exported", is.Tmax=T, v)
    Tmax.data.points = Tmax.data
    Tmax.data.line = matrix(nrow=nrow(dT.data), ncol=ncol(dT.data) )
    Tmax.data.line = as.data.frame(Tmax.data.line)
    names(Tmax.data.line) = names(dT.data)
    # get lines
    for (tree.name in v$sapflux.names){
      tree.number = match(tree.name,names(v$dT.data))
      Tmax.data.local = .Tmax.get.data(tree.name, Tmax.data, LDate)
      Tmax.data.line[,tree.number] = Tmax.data.local$line
    }
    # export lines
    .save.one.file( Tmax.data.line, "Tmax baseline- Exported", is.Tmax=F, v)
    Tmax.data.line = Tmax.data.line
  } else {
    Tmax.data.line = NA
  }
  # kick out Tmax.data.line
  Tmax.data.line
}
.export.raw = function(v){
  y = v$met.data
  z = data.frame(TIMESTAMP=y$TIMESTAMP,LDate=y$LDate, v$raw.data)
  .save.one.file( z, "dT raw- Exported", is.Tmax=F, v)
  raw.dT.data <<- z
}
.export.met = function(v){
  .save.one.file( v$met.data, "Met data- Exported", is.Tmax=F, v)
  met.data <<- v$met.data
}
.export.dT = function(v){
  y = v$met.data
  z = data.frame(TIMESTAMP=y$TIMESTAMP,LDate=y$LDate, v$dT.data)
  .save.one.file( z,  "dT cleaned- Exported", is.Tmax=F, v)
  dT.data <<- z
}
.export.graphs= function(v,sapflux.data,Tmax.data.line){
  for (tree.name in v$sapflux.names){
    # header stuff
    tree.number = match(tree.name,names(v$dT.data))
    v$tree.number = tree.number
    v$tree.name = names(v$dT.data)[v$tree.number]
    # get data
    v = .get.local.data(v)
    # start pdf
    pdf.name = paste(v$study.year,"_",tree.name,".pdf",sep="")
    pdf(pdf.name)
    par(mfrow=c(2,1))
    #####actually plot dT
    v$pick.plot.options = c("Tmax","VPD")
    .plot.dT(v)
    ###### plot sapflux
    v$pick.plot.options = c("flux")
    .plot.sapflux(v)

    # .plot.sapflux(v)
    dev.off()
  }
}
.export.final.data = function(v){
  # normal save
  v$min.DOY = v$min.DOY.global
  v$max.DOY = v$max.DOY.global
  v$time.last.save = .save.AquaFlux(v)

  ################### export each data set to csv and base R
  setwd(v$final.dir)
  sapflux.data = .export.sapflux(v)
  Tmax.data.line = .export.Tmax(v$Tmax.data,v$LDate, v$sapflux.names, v$dT.data, v)
  .export.raw(v)
  .export.dT(v)
  .export.met(v)
  ######### export graphs
  setwd(v$graph.dir)
  .export.graphs(v,sapflux.data,Tmax.data.line)
  print("Data exported")
  # report
  #.export.message(v$final.dir,v$graph.dir)
}
