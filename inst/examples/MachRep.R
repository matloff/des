
# simple machine repair model; m machines, r repairpersons; expon up and
# repair times, everything independent

mrp <- function(meanup,meanrep,timelim,m,r,dbg=FALSE) {
   # set up structures
   simlist <- newsim(timelim,dbg=dbg)
   simlist$reactevent <- mrpreact  
   simlist$uprate <- 1.0 / meaninterup
   simlist$reprate <- 1.0 / meanrep
   simlist$queue <- newqueue(simlist)  # queue for the repairpersons
   simlist$nrepairing <- 0  # number of machines now being repaired
   simlist$breakevnt <- 1  # breakdown
   simlist$repairevnt <- 2  # good as new!
   # bookkeeping
   simlist$totuptime <- 0.0  # total up time for all machines

   # get the ball rolling: set breakdown events for the machines, which
   # are all currently up at time 0.0
   for (i in 1:m) {
      whenbreak <- rexpon(1,simlist$uprate)a
      schedevnt(simlist,whenbreak,simlist$breakevnt)
   }

   # start sim
   mainloop(simlist)

   # sim done
   # should print out something near 1 / (srvrate - arrvrate)
   cat("mean wait:  ")
   print(simlist$totwait / simlist$totjobs)
}

incremjobnum <- function(simlist) {
   jobnum <- simlist$jobnum + 1
   simlist$jobnum <- jobnum
   jobnum
}

# what new events are triggered by the occurrence of an old one?
mm1react <- function(evnt,simlist) {
   etype <- evnt['evnttype']
   if (etype == simlist$arrvevnt) {  # job arrival
      ### # schedule next arrival
      ### timeofnextarrival <- simlist$currtime + rexp(1,simlist$arrvrate)
      ### jobnum <- incremjobnum(simlist)
      ### schedevnt(simlist,timeofnextarrival,simlist$arrvevnt,
      ###    c(timeofnextarrival,jobnum))
      # start newly-arrived job or queue it
      if (!simlist$srvrbusy) {  # server free, start job service
         simlist$srvrbusy <- TRUE
         srvduration <- rexp(1,simlist$srvrate)
         schedevnt(simlist,simlist$currtime+srvduration,simlist$srvevnt,
            evnt[3:4])  # copy over previous data for this job
      } else {  # server busy, add job to queue
         appendfcfs(simlist$queue,evnt)
      }
   } else if (etype == simlist$srvevnt) {  # job completion
      # bookkeeping
      simlist$totjobs <- simlist$totjobs + 1
      # wait time = job completion time - job arrival time
      simlist$totwait <- simlist$totwait + simlist$currtime - evnt[3]
      simlist$srvrbusy <- FALSE
      # check queue for waiting jobs
      if (nrow(simlist$queue$m) > 0) {  # nonempty queue
         qhead <- delfcfs(simlist$queue)
         # start job service
         simlist$srvrbusy <- TRUE
         srvduration <- rexp(1,simlist$srvrate)
         schedevnt(simlist,simlist$currtime+srvduration,simlist$srvevnt,
            qhead[3:4])
      }
   } 
}
