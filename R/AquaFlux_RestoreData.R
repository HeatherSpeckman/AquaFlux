

#####################################################################
# Restore data
#####################################################################

# back up the restores...
#######################################
# data restoration functions

.excute.restore = function(v,backup){
  # needs: tree.number, cc.time, dT.local
  # change local
  v$tree.number = backup$tree.number
  v$cc.time = backup$cc.time
  v$LDate.local = v$LDate[v$cc.time]
  v$dT.local = backup$dT.local
  # change global
  v$dT.data[v$cc.time,v$tree.number] = v$dT.local
  # change button back
  v$qaqc.manual = "none"
  v$restore.option= "none"
  v
}
.restore.undo = function(v){
  if (length(v$backup$dT.local)>0){
    backup = v$backup
    v = .excute.restore(v,backup)
    v$restore.option= "none"
    # erase backup
    v$backup = NULL
  }
  v
}
.restore.all = function(v){
  # backup filtered data
  v$backup= .qaqc.backup(v)
  # make the backup list
  backup = list()
  backup$tree.number = v$tree.number
  backup$cc.time = v$cc.time
  backup$dT.local = v$raw.data[ v$cc.time, v$tree.number]
  # excute
  v = .excute.restore(v,backup)
  v
}
.restore.between = function(v,thres){
  if (is.null(v$bx)==T) {  # if you don't have any between.points saved
    v$bx = thres$x # save it
  } else { # if you have 1 point saved, and now two
    # get range
    b.range = sort(c( v$bx ,thres$x) ) # save the range
    v$bx = NULL # null it out
    # back up  -- can't do with multipe (I need to right the restore function)
    ###  v$backup= .qaqc.backup(v)
    ######### make backup item -- tree.number
    backup = list()
    backup$tree.number = v$tree.number
    ######### cc.time
    # cc.time: the whole window
    backup$cc.time = v$cc.time
    # cc.time.sec: the restore section
    cc.time.sec = v$LDate >= b.range[1] & v$LDate <= b.range[2]
    ######## dT data
    # the filtered
    backup$dT.local = v$dT.data[ v$cc.time, v$tree.number]
    # plug in the restored part (get from raw data)
    backup$dT.local[cc.time.sec] = v$raw.data[ cc.time.sec, v$tree.number]
    # excute
    v = .excute.restore(v,backup)
  }
  v
}

