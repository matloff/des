
# test; M/M/1 queue -- exponential ("Markov") job interarrivals,
# exponential service times, 1 server

mm1 <- function(meaninterarrv,meansrv,timelim,dbg=FALSE) {
   # set up structures
   simlist <- 
      newsim(timelim,3,appcols=c('arrvtime','jobnum'),aevntset=TRUE,dbg)
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

   exparrivals(simlist,meaninterarrv)

   # start sim
   mainloop(simlist)

   # sim done
   # should print out something near 1 / (srvrate - arrvrate)
   cat("mean wait:  ")
   print(simlist$totwait / simlist$totjobs)
}

# what new events are triggered by the occurrence of an old one?
mm1react <- function(evnt,simlist) {
   etype <- evnt['evnttype']
   if (etype == simlist$arrvevnt) {  # job arrival
      # start newly-arrived job or queue it
      if (!simlist$srvrbusy) {  # server free, start job service
         simlist$srvrbusy <- TRUE
         srvduration <- rexp(1,simlist$srvrate)
         schedevnt(simlist,simlist$currtime+srvduration,simlist$srvevnt,
            evnt[3:4])  # copy over previous data for this job
      } else {  # server busy, add job to queue
         appendfcfs(simlist$queue,evnt)
      }
   } else {  # etype = simlist$srvevnt, job completion
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
