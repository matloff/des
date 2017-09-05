
# test; M/M/1 queue -- exponential ("Markov") job interarrivals,
# exponential service times, 1 server

mm1 <- function(meaninterarrv,meansrv,timelim,dbg=FALSE) {

   # set up structures
   simlist <- newsim(3,appcols=c('arrvtime','srvtime'),dbg)
   simlist$reactevent <- mm1react  
   simlist$arrvrate <- 1 / meaninterarrv
   simlist$srvrate <- 1 / meansrv
   simlist$totjobs <- 0
   simlist$totwait <- 0.0
   simlist$queue <- newqueue(simlist)
   simlist$srvrbusy <- FALSE
   # defining job numbers is good practice, always invaluable during
   # debugging
   simlist$jobnum <- 0
   # event type codes: 1 for arrival, 2 for service completion
   simlist$arrvevnt <- 1
   simlist$srvevnt <- 2

   # set up and schedule first event, including info on this job's 
   # arrival time for later use in finding mean wait until job done
   timeto1starrival <- rexp(1,simlist$arrvrate)
   jobnum <- incremjobnum(simlist)
   # schedule an event to occur at time timeto1starrival, of event type
   # arrvevnt, in our sim structure simlist; this will result in a row
   # inserted in the schedule matrix, with app-specific data consisting
   # of the arrival time of the job (so we can later determine how much
   # total time the job spent in the system) and its job number
   schedevnt(timeto1starrival,simlist$arrvevnt,simlist,
      c(timeto1starrival,jobnum))

   # start sim
   mainloop(simlist,timelim)

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
      # schedule next arrival
      timeofnextarrival <- simlist$currtime + rexp(1,simlist$arrvrate)
      jobnum <- incremjobnum(simlist)
      schedevnt(timeofnextarrival,simlist$arrvevnt,simlist,
         c(timeofnextarrival,jobnum))
      # start newly-arrived job or queue it
      if (!simlist$srvrbusy) {  # server free, start job service
         simlist$srvrbusy <- TRUE
         srvduration <- rexp(1,simlist$srvrate)
         schedevnt(simlist$currtime+srvduration,simlist$srvevnt,
            simlist,evnt[3:4])  # copy over previous data for this job
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
         schedevnt(simlist$currtime+srvduration,simlist$srvevnt,simlist,
            qhead[3:4])
      }
   } 
}
