# The DES Package: Discrete-Event Simulation in R

## What is discrete-event simulation?

*Discrete event simulation* refers to the simulation of systems that
have abrupt, i.e. discrete, changes. In a queuing system, for instance,
when a new job arrives, the queue length abruptly increases by 1.
Simulation of a weather system, on the other hand, would not fit this
definition, as quantities such as temperature vary continuously.

## What is the DES package?

The DES package here, **DES**, is not the fastest. I recommend the
[excellent **simmer** package](https://cran.rstudio.com/web/packages/simmer/index.html) 
if speed is an issue.  On the other hand, **DES** is much easier to 
learn (good for teaching, for instance), and gives the programmer 
more control, thus making simulation of more complex systems easier 
to program.  

## Example:  Machine-repair model

The **DES** package uses the *event-oriented* approach, which means the
programmer codes how the system reacts to any specific event.  To see
how event-oriented simulation works, consider a simple machine-repair
model.

We have **m** machines and **r** repairpersons.  Each machine
occasionaly breaks down.  If at the time of a breakdown there is at
least one idle repairperson, that machine's reppair is begun; otherwise,
it joins a queue for the repairperson pool.  We assume up times and
repair times are exponentially distributed, with all times being
independent and so on.

The file **inst/examples/MachRep.R** in the package simulates this system. 
The functions in that file are thus application-specific, and we will
refer to them as *user-supplied*.  They call **DES** functions, which we
will refer to as *package functions*.  The user-supplied wrapper that runs 
the simulation is named **mrp** here.

#### Event types

Here there are two kinds of events in this application, breakdown and repair.
Think about how the system reacts to a breakdown at a machine:

1. If some repairperson is free, start service for this machine.  
   Otherwise, add the job to the queue.

What about reaction to a repair completion at a machine?

1. Start the next up time for this machine.

2. If the queue is nonempty, remove the head and start repaird for that
   job.

### The sim list 

The general information about the simulation is contained in the *sim
list*, which **mrp** not surprisingly has named **simlist**.  Here are
the first few lines:

```R
mrp <- function(meanup,meanrep,timelim,m,r,dbg=FALSE) {
   # set up structures
   simlist <- newsim(timelim,m,
      appcols=c('startqtime','startuptime'),dbg=dbg)
   simlist$reactevent <- mrpreact
   simlist$uprate <- 1.0 / meanup
   simlist$reprate <- 1.0 / meanrep
   simlist$nmach <- m
   simlist$nrepairpersons <- r
   simlist$queue <- newqueue(simlist)  # queue for the repairpersons
   simlist$nup <- m  # all machines up initially
   simlist$nrepbusy <- 0  # number of busy repairpersons
   simlist$breakevnt <- 1  # breakdown
   simlist$repairevnt <- 2  # good as new!
   simlist$nrepairs <- 0
   simlist$totqtime <- 0.0
   simlist$totuptime <- 0.0
```

The package function **newsim** initializes the simulation, particularly
the non-application specific parts of the sim list, such as
**simlist$currtime**, which will hold the current simulated time.  

The lines we see above are setting application-specific information in
the sim list, such as the up time and repair rates.  Note too the
initialization of the "bookkeeping" variables **simlist$nrepairs**,
which keeps track of the number of repairs done so far, needed so we can
determine the mean queuing time later on.

### Event set

A major **DES** structure is the *event set*, a matrix that contains all
pending events, say three breakdowns and one repair.  It is initialized
by **newsim** as a component of the sim list.  User-supplied codes adds
events to the event set by calling the package function **schedevnt**.

There will be one row in the event set for each pending event.  The row
will contain the simulated time at which the event is to occur, and the
event type (in the machine-repair example, breakdown or repair
completion).  The row will also contain optional application-specific
information, which in our call to **newsim** we have specified as the
start of queueinng time of the machine and the time at which the current
up time for the machine began.  The argument **appcols** in **newsim**
gives the names of these events (the names of their columns in the event
set matrix), and the **appdata** argument in **schedevnt** gives the
particular values of this data at the time of the call.

Our user-supplied code, **mrp** here, must "get the ball rolling" by
creating the initial events.  Since our simulation will assume that all
machines start in the up mode -- it doesn't really matter in the long
run, but we need a start -- **mrp** creates breakdown events for all of
them:

```R
   for (i in 1:m) {
      whenbreak <- rexp(1,simlist$uprate)
      schedevnt(simlist,whenbreak,simlist$breakevnt,appdata=c(NA,0))
   }
```

The call to **schedevnt** schedules a breakdown event at time
**whenbreak**.  (Note that simulated time begins at 0.0.)

### User-suppplied reaction function and package function **mainloop**:

The user must supply a *reaction function*, which codes how the system
reacts to the various events.  In the **mrp** code above, you can see
that we have named that function **mrpreact**, and recorded it as a
component of the sim list.  We'll look at that function shortly too.

The core package function is **mainloop**, which works as
follows:  

```
while simulated time < time limit do
   remove earliest event from the event set
   call the user-supplied reaction function with this event
```

### Details of mrp

So, let's look at our user-supplied reaction function in this example,
**mrp**.  The first few lines are

```R
mrpreact <- function(evnt,simlist) {
   etype <- evnt['evnttype']
   if (etype == simlist$breakevnt) {  # machine has gone down
      # record this up time
      simlist$totuptime <-
         simlist$totuptime + simlist$currtime - evnt[4]
      # is there is a free repairperson?
      nrepb <- simlist$nrepbusy
      if (nrepb < simlist$nrepairpersons) {
         # start repair, no queuing
         simlist$nrepbusy <- nrepb + 1
         repduration <- rexp(1,simlist$reprate)
         schedevnt(simlist,simlist$currtime+repduration,simlist$repairevnt,
            appdata=c(NA,NA))
      } else {  # no repairpersons free, add job to queue
         # record start queue wait
         evnt[3] <- simlist$currtime
         appendfcfs(simlist$queue,evnt)
      }
   } else {  # etype = simlist$repairevnt
...

These are the lines that handle breakdown events.  Recall that our
user-supplied reaction function is called by the package function
**mainloop**, which has provided us the just-occurred event, **evnt**.



```R
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
```

Note that the package function **delfcfs** was used to delete the head
of the queue.  The structure for the latter is in **simlist$queue**,
with the actual queue being the matrix **m** in the latter.  The matrix
is in the same format as the event set.

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
