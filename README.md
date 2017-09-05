# The DES Package: Discrete-Event Simulation in R

## What is discrete-event simulation?

*Discrete event simulation* refers to the simulation of systems that
have abrupt, i.e. discrete, changes. In a queuing system, for instance,
when a new job arrives, the queue length abruptly increases by 1.

## What is the DES package?

The DES package here, **DES**, is not the fastest. I recommend the
[excellent **simmer** package](https://cran.rstudio.com/web/packages/simmer/index.html) 
if speed is an issue.  On the other hand, **DES** is much easier to 
learn (good for teaching, for instance), and gives the programmer 
more control, thus making simulation of some systems easier to program.
The package uses the *event-oriented* approach, which means the
programmer codes how the system reacts to any specific event.  

## Example:  M/M/1 queue

To see how event-oriented simulation works, consider a classic example,
the M/M/1 queue:  Interjob arrival times and job service times are
exponentially distributed, and there is 1 server.

The file **inst/examples/MM1.R** in the package simulates this system. 
The functions in that file are thus application-specific, and we will
refer to them as *user-supplied*.  They call **DES** functions, which we
will refer to as *package functions*.  The *user-supplied wrapper* that runs 
the simulation is named **mm1** here.

Here there are two kinds of events, job arrival and job completion.
Think about how the system reacts to an arrival:

1. If the server is free, start service for this job.  Otherwise, add
   the job to the queue.

2. Wait for the next arrival.

What about reaction to a job service completion?

1. If the queue is nonempty, remove the head and start service for that
   job.

### Basic structures: the sim list and event set

The general information about the simulation is contained in the *sim
list*, which **mm1** not surprisingly has named **simlist**.  Here are
the first few lines:

```R
mm1 <- function(meaninterarrv,meansrv,timelim,dbg=FALSE) {
   simlist <- newsim(3,appcols=c('arrvtime','srvtime'),dbg)
   simlist$reactevent <- mm1react
   simlist$arrvrate <- 1 / meaninterarrv
   simlist$srvrate <- 1 / meansrv
   simlist$totjobs <- 0
   simlist$totwait <- 0.0
   simlist$queue <- newqueue(4)
   simlist$srvrbusy <- FALSE
```

The package function **newsim** initializes the simulation, particularly
the sim list.  The lines we see above are setting application-specific
information in the sim list, such as the arrival and service rates.  
Note too the initialization of the "bookkeeping" variables **totjobs**
and **totwai**, which we need to find the mean queue wait at the end of
the simulation.

We will explain the other application-specific components shortly, but
first note that the call to **newsim** also initializes non-application
specific components of the sim list, such as **simlist$currtime**, the
current simulated time, initialized to 0.0.  

A key non-application specific component of the sim list is the *event
set*, **simlist$evnts**, a matrix which we will discuss now.  This is,
as the name implies, the set of pending events.  At any given time we
will have an arrival pending, and possibly a pending service.  

You can easily imagine generalizations.  Say we are simulating a system
with k servers, with two job categories having separate queues, priority
given to queue 1.  Then we would always have two arrivals pending, and
have up to k service events pending.

Or, think of a machine repair model, with m machines and r
repairpersons.  Each machine alternately goes through up and down
cycles, with random up times and random repair times.  Suppose m > r and
currently i > r machines are down.  Then all r repairpersons will be
busy, with r pending service completion events, and the m-i machines
currently up have m-i breakdown events pending.

There will be one row in the event set for each pending event.  The row
will contain the simulated time at which the event is to occur, and the
event type (in the M/M/1 example, arrival or service completion).  The
row will also contain optional application-specific information, which
in our call to **newsim** we have specified as the arrival time of the
job and the service time it requires.

### User-suppplied reaction function and package function **mainloop**:

A user-supplied *reaction function* will be called to process each event
when it uoccurs.  The core function of the package function
**mainloop**, then works as follows:  

```
while simulated time < time limit do
   remove earliest event from the event set
   call the user-supplied reaction function with this event
```

The reaction function will typically add one or more new events to the
event set.  In **mm1**, for instance, if the reaction function is given
an arrival event, it will generate the next arrival event, and add it to
the event set. And if the current arrival occurs when the server is
free, it will add a job service service completion event tp the event
set.

We have named the user-supplied reaction function **mm1react**.
**DES** requires that it have the call form

```R
mm1react(evnt,simlist)
```

Note carefully that the entity that calls **mm1react** is the package
function **mainloop**.  The argument **evnt** will be the row that the
latter function has just removed from the event set.  The first two
lines 

```R
etype <- evnt['evnttype']
   if (etype == simlist$arrvevnt) {  # job arrival
```

check to see whether the event is an arrival or a job completion.  If it
is the former, this code simulates the reaction:

```R
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
```

That first line simulates the time of the next arrival.  The call to the
package function **schedevnt** then adds this pending new arrival to the
event set.  It's always nice in this kind of simulation to assign an ID
to each job, hence our inclusion of a job number here.


Now returning to the above code, it checks whether the server is busy.
If not, then the newly-arrived job can start service.  A random service
time is generated, and **schedevnt** is called to add this new service
completion event to the event set.  On the other hand, if the server is
busy, we add the newly-arrived job to the queue, via a call to the
package function **appendfcfs**.

And what happens when a job service completion occurs?  Here is the
relevant portion of the code from **mm1**:

```R
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


Well, then, how does **mainloop** itself get started?  It is called by
the user-defined wrapper, in this case **mm1**:

```R
mm1 <- function(meaninterarrv,meansrv,timelim,dbg=FALSE) {
   simlist <- newsim(3,appcols=c('arrvtime','srvtime'),dbg)
   simlist$reactevent <- mm1react
   ...
   # set up and schedule first event, including info on this job's 
   # arrival time for later use in finding mean wait until job done
   timeto1starrival <- rexp(1,simlist$arrvrate)
   jobnum <- incremjobnum(simlist)
   schedevnt(timeto1starrival,simlist$arrvevnt,simlist,
      c(timeto1starrival,jobnum))
   # start sim
   mainloop(simlist,timelim)
   ...
````

The wrapper sets up the first event, adding it to the event list, then
calls **mainloop**.

So, to simulate an M/M/1 queue with mean interarrival and service times
1.0 and 0.5, over a simulated time range of 10000.0, we would execute

```R
> mm1(1,0.5,10000)
```

## Other functions



## Process-oriented DES

All this is how event-oriented systems work.  *Process-oriented* systems
such as **simmer** (which is modeled on the Python library
[**SimPy**](https://simpy.readthedocs.io/en/latest/)) 
are very similar to the above, except that there would not be code to
generate the next arrival upon the occurrence of one arrival.  Instead,
there would be two *processes*, one for arrivals and one for the server.
If you are familiar with OS threads, each process here is similar to a
thread.
