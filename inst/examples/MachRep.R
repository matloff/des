
# simple machine repair model; m machines, r repairpersons; expon up and
# repair times, everything independent

mrp <- function(meanup,meanrep,timelim,m,r,dbg=FALSE) {
   # set up structures
   simlist <- newsim(timelim,m,appcols='startqtime',dbg=dbg)
   simlist$reactevent <- mrpreact  
   simlist$uprate <- 1.0 / meanup
   simlist$reprate <- 1.0 / meanrep
   simlist$nmach <- m
   simlist$nrepairpersons <- r
   simlist$queue <- newqueue(simlist)  # queue for the repairpersons
   simlist$nup <- m  # all machines up initially
   simlist$nrepbusy <- 0  # doesn't include those queued for repair
   simlist$breakevnt <- 1  # breakdown
   simlist$repairevnt <- 2  # good as new!
   # bookkeeping; say we are interested in mean wait until repai
   simlist$nrepairs <- 0
   simlist$totqtime <- 0.0  

   # get the ball rolling: set breakdown events for the machines, which
   # are all currently up at time 0.0
   for (i in 1:m) {
      whenbreak <- rexp(1,simlist$uprate)
      # schedule a breakdown event for machine i at time whenbreak
      schedevnt(simlist,whenbreak,simlist$breakevnt,appdata=NA)
   }

   # start sim
   mainloop(simlist)

   # sim done
   cat("mean queuing time:  ")
   print(simlist$totqtime / simlist$nrepairs)
}

# what new events are triggered by the occurrence of an old one?
mrpreact <- function(evnt,simlist) {
   etype <- evnt['evnttype']
   if (etype == simlist$breakevnt) {  # machine has gone down
      # is there is a free repairperson
      nrepb <- simlist$nrepbusy
      if (nrepb < simlist$nrepairpersons) {  
         # start repair
         simlist$nrepbusy <- nrepb - 1
         repduration <- rexp(1,simlist$reprate)
         schedevnt(simlist,simlist$currtime+repduration,simlist$repairevnt,
            appdata=NA)
      } else {  # no repairpersons free, add job to queue
         # record start
         evnt[3] <- simlist$currtime
         appendfcfs(simlist$queue,evnt)
      }
   } else {  # etype = simlist$repairevnt
      # bookkeeping
      simlist$nrepairs <- simlist$nrepairs + 1
      if (!is.na(evnt[3])) {
         simlist$totqtime <- simlist$qtime + simlist$currtime - evnt[3]
         evnt[3] <- NA
      }
      simlist$nrepbusy <- simlist$nrepbusy - 1
      # check queue for waiting jobs
      if (nrow(simlist$queue$m) > 0) {  # nonempty queue
         qhead <- delfcfs(simlist$queue)
         # start job service
         simlist$nrepbusy <- simlist$nrepbusy + 1
         srvduration <- rexp(1,simlist$reprate)
         schedevnt(simlist,simlist$currtime+srvduration,simlist$breakevnt,
            qhead[3])
      }
   } 
}
