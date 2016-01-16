
# DES.R:  R routines for discrete-event simulation (DES), with an example

# matrix version; data frame allows character event types, but much too slow

# all data is stored in an R environment variable that will be referrred
# to as simlist below; an environment variable is used so that functions
# can change their simlist argument, rather than reassigning

# the simlist will consist of the following components:
#
#       currtime:  current simulated time
#       evnts:  the events list, a matrix, one event per row
#       reactevent:  event handler function, user-supplied; creates 
#                    new events upon the occurrence of an old one;
#                    e.g. job arrival triggers either start of 
#                    service for the job or queuing it; call form is
#                    reactevent(evnt,simlist)
#       dbg:  if TRUE, will print simlist$evnts after each call to
#             simlist$reactevent(), and enter R browser for 
#             single-stepping etc.

# the application code can add further application-specific data to
# simlist, e.g. total job queuing time 

# each event will be represented by a matrix row consisting of columns
# for: 
# 
#    occurrence time
#    event type (user-defined numeric code, e.g. 1 for arrival, 2 for 
#       job completion, etc.)
#    application-specific information, if any
#
# the rows of the matrix are ordered by event time

# library functions 
# 
#       newsim:  create a new simlist
#       insevnt:  insert a new event into simlist$evnts 
#       schedevnt:  schedule a new event (calls insevnt())
#       getnextevnt:  pulls the earliest event from the event set,
#                     process it, and update the current simulated
#                     time
#       mainloop:  as the name implies
#       appendfcfs:  append job to a FCFS queue
#       delfcfs:  delete head of a FCFS queue

# event list:

#    matrix in simlist
#    one row for each event, rows ordered by event occurrence time
#    first two cols are event time, event time, then app-specific info

# outline of a typical application:

#    mysim <- newsim()    create the simlist
#    set reactevent in mysim
#    set application-specific variables in mysim, if any
#    set the first event(s) in mysim$evnts
#    mainloop(mysim,mysimtimelim)
#    print results

# create a simlist, which will be the return value, an R environment
newsim <- function(dbg=FALSE) {
   simlist <- new.env()
   simlist$currtime <- 0.0  # current simulated time
   simlist$evnts <- NULL  # event set
   simlist$dbg <- dbg
   simlist
}

# insert event evnt into simlist$evnts
insevnt <- function(evnt,simlist) {
   # if the event set is empty, set it to consist of evnt and return
   if (is.null(simlist$evnts)) {
      simlist$evnts <- matrix(evnt,nrow=1)
      return()
   }
   # otherwise, find insertion point
   inspt <- binsearch(simlist$evnts[,1],evnt[1])
   # now "insert," by reconstructing the matrix; we find what portion of
   # the current matrix should come before evnt and what portion should 
   # come after it, then string everything together
   before <- if (inspt == 1) NULL else simlist$evnts[1:(inspt-1),]
   nr <- nrow(simlist$evnts)
   after <- if (inspt <= nr) simlist$evnts[inspt:nr,] else NULL  
   simlist$evnts <- rbind(before,evnt,after)  
   rownames(simlist$evnts) <- NULL
}

# schedule new event in simlist$evnts; evnttime is the time at
# which the event is to occur; evnttype is the event type; appdata is
# a vector of numerical application-specific data
schedevnt <- function(evnttime,evnttype,simlist,appdata=NULL) {
   evnt <- c(evnttime,evnttype,appdata)
   insevnt(evnt,simlist)  
}

# start to process next event (second half done by application
# programmer via call to reactevnt() from mainloop())
getnextevnt <- function(simlist) {
   head <- simlist$evnts[1,]
   # delete head
   if (nrow(simlist$evnts) == 1) simlist$evnts <- NULL else 
      simlist$evnts <- simlist$evnts[-1,,drop=F]  
   return(head)
}

# main loop of the simulation
mainloop <- function(simlist,simtimelim) {
   while(simlist$currtime < simtimelim) {
      head <- getnextevnt(simlist)  
      # update current simulated time
      simlist$currtime <- head[1]  
      # process this event (programmer-supplied ftn)
      simlist$reactevent(head,simlist)  
      if (simlist$dbg) {
         print("event occurred:")
         print(head)
         print("events list now")
         print(simlist$evnts)
         browser()
      }
   }
}

# binary search of insertion point of y in the sorted vector x; returns
# the position in x before which y should be inserted, with the value
# length(x)+1 if y is larger than x[length(x)]; this could be replaced
# by faster C code
binsearch <- function(x,y) {
   n <- length(x)
   lo <- 1
   hi <- n
   while(lo+1 < hi) {
      mid <- floor((lo+hi)/2)
      if (y == x[mid]) return(mid)
      if (y < x[mid]) hi <- mid else lo <- mid
   }
   if (y <= x[lo]) return(lo)
   if (y < x[hi]) return(hi)
   return(hi+1)
}

# the functions below assume that queues are represented as matrices,
# one row per queued job, containing application-specific information
# about the job; the matrix is assumed stored in an environment, with
# the matrix being named m

# new queue with ncol columns
newqueue <- function(ncol) {
   q <- new.env()
   q$m <- matrix(nrow=0,ncol=ncol)
   q
}

# appends jobtoqueue to the given queue, assumed of the above form;
# the new, longer list is returned; jobtoqueue is a vector of length
# equal to the number of columns in the queue matrix
appendfcfs <- function(queue,jobtoqueue) {
   if (is.null(queue$m)) {
      queue$m <- matrix(jobtoqueue,nrow=1)
      return()
   }
   queue$m <- rbind(queue$m,jobtoqueue)
}

# deletes head of queue; assumes list-of-lists structure as decribed
# above; returns the head and new queue
delfcfs <- function(queue) {
   if (is.null(queue$m)) return(NULL) 
   qhead <- queue$m[1,]
   queue$m <- queue$m[-1,,drop=F]
   qhead
}

