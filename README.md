# The DES Package: Discrete-Event Simulation in R

## What is discrete-event simulation?

*Discrete event simulation* refers to the simulation of systems that
have abrupt, i.e. discrete, changes. In a queuing system, for instance,
when a new job arrives, the queue length abruptly increases by 1.
Simulation of a weather system, on the other hand, would not fit this
definition, as quantities such as temperature vary continuously.

This document will give a quick introduction to the subject of DES,
using our R package **DES** as a running example.

## What is the DES package?

The R DES package here, **DES**, is not the fastest.  If speed is an
issue, 
I recommend the
[excellent **simmer** package](https://cran.r-project.org/package=simmer) 
in R, or in Python,
[**SimPy**](https://simpy.readthedocs.io/en/latest/), on which
**simmer** is based.  On the other hand, **DES** is much easier to 
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
least one idle repairperson, that machine's repair is begun; otherwise,
it joins a queue for the repairperson pool.  We assume up times and
repair times are exponentially distributed, with all times being
independent and so on.

The file **inst/examples/MachRep.R** in the package simulates this system. 
The functions in that file are thus application-specific, and we will
refer to them as *user-supplied*.  They call **DES** functions, which we
will refer to as *package functions*.  The user-supplied wrapper that runs 
the simulation is named **mrp** here.

For instance, say we wish to simulate a system in which machines have
mean up and repair times of 10.0 and 1.0, respectively, with 2 machines
and one repairperson, for 10000.0 units of simulated time.  The call
would be

```R
mrp(10,1,10000,2,1)
```

### Event types

Here there are two kinds of events in this application, breakdown and repair.
Think about how the system reacts to a breakdown at a machine:

1. If some repairperson is free, start service for this machine.  
   Otherwise, add the job to the queue.

What about reaction to a repair completion at a machine?

1. Start the next up time for this machine.

2. If the queue is nonempty, remove the head and start repair for that
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
by **newsim** as a component of the sim list named **evnts**.
User-supplied code adds events to the event set by calling the package
function **schedevnt**.

There will be one row in the event set for each pending event.  The row
will contain the simulated time at which the event is to occur, and the
event type (in the machine-repair example, breakdown or repair
completion).  The row will also contain optional application-specific
information, which in our call to **newsim** we have specified as the
start of the current queueing time of the machine, if any, and the time
at which the current up time for the machine began, if any.  The
argument **appcols** in **newsim** gives the names of these quantities
(the names of their columns in the event set matrix), and the
**appdata** argument in **schedevnt** gives the particular values of
this data at the time of the call.

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

### Details of mrpreact

So, let's look at our user-supplied reaction function in this example,
**mrpreact**.  The first few lines are

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
```

These are the lines that handle breakdown events.  Recall that our
user-supplied reaction function is called by the package function
**mainloop**, which has provided us the just-occurred event, **evnt**.
We see that the above code checks **evnt** for event type, and if it is
a breakdown event, executes the code that follows.

First there is bookkeeping to be done.  Since an up time has just ended,
we need to calculate how long this time lasted, and add that to our
running total in **simlist$totuptime**.

Next we must check whether any repairpersons are free.  If so, we call
**schedevnt** to schedule a repair event for the machine that just went
down. Otherwise, we'll need to add the machine to the queue, using the
package function **appendfcfs**. Note that in that latter case, we must
set the **startqtime** field in the event vector to the current time, so
that later when the repair time begins, we can calculate the queue
residence time for this event.

The code for the case of a repair event is similar:

```R
   } else {  # etype = simlist$repairevnt
      # bookkeeping
      simlist$nrepairs <- simlist$nrepairs + 1
      # start next up time for this machine
      uptime <- rexp(1,simlist$uprate)
      schedevnt(simlist,simlist$currtime+uptime,simlist$breakevnt,
         appdata=c(NA,simlist$currtime))
      # repairperson now free
      simlist$nrepbusy <- simlist$nrepbusy - 1
      # check queue for waiting jobs
      if (nrow(simlist$queue$m) > 0) {  # nonempty queue
         qhead <- delfcfs(simlist$queue)
         # bookkeeping
         simlist$totqtime <- simlist$totqtime + simlist$currtime - qhead[3]
         # start job service
         # this repairperson now busy
         simlist$nrepbusy <- simlist$nrepbusy + 1
         srvduration <- rexp(1,simlist$reprate)
         schedevnt(simlist,simlist$currtime+srvduration,simlist$repairevnt,
            qhead[3:4])
      }
   }
```

We schedule an up time for the newly-repaired machine, and now that this
repairperson is free, we check whether there are any machines waiting in
the queue that this repairperson can work on.  If so, we remove the head
of the queue and schedule a repair event for this machine.  Also, since
the queue wait for that machine has ended, we calculate its queue
residence time and add it to our running total.

## Example:  M/M/1 queue model

The interarrival and service time distributions are assumed exponential
("M" for "memoryless"), and there is a single server ("1").
The file **inst/examples/MM1.R** in the package simulates this system,
with **mm1** containing the overall code, and **mm1react** being the
user-supplied reaction function.  The latter handles two kinds of
events, job arrival and job service completion.

Most of the code is similar to that of our first example. The main new
part is use of the package function **exparrivals**, which we use to
pre-calculate all the arrivals.  This makes the code look a little more
like process-oriented simulation, with arrivals handled separately.
Without this, in the reaction function, each arrival would spawn the
next. 

In the initialization, the arrivals are pre-calculated with

```R
exparrivals(simlist,meaninterarrv)
```

As explained in the Technical Details section below, these are actually
stored in a special auxiliary event set, but that need not concern the
application programmer other than a special argument **aevntset** in
**newsim** that signals use of the auxiliary event set internally.

```R
   simlist <-
      newsim(timelim,3,appcols=c('arrvtime','jobnum'),aevntset=TRUE,dbg)
```

Note that there are two application-specific fields in **appcols**:
**arrvtime** is recorded so that when a job finally completes service,
we can calculate how long it was in the system, thus enabling the
computation of mean wait; **jobnum** is simply a job ID, 1,2,3,..., not
directly used here, but usually a good idea, e.g. to help in debugging.

The rest of **mm1** and **mm1react** are similar to the machine-repair
example above.

## Debugging

Setting **dbg** to TRUE in the call to **newsim** specifies debug mode.
This is then used by **mainloop**, which pauses after each event
occurrence.  The event and the new event list are printed out, and 
R browser mode is entered, enabling single-stepping and querying of
variables.

## Other functions

An important function is **cancelevnt**, which does exactly what its
name implies.

Suppose we are simulating a computer system that has some timeout
period **tmo**.  We would define a timeout event, and schedule an instance of
that event for **tmo** time later.  But if some action turns out to
occur before then, we would need to cancel the timeout.

## Technical details

(This material requires advanced knowledge of programming.)

### Management of the event set

The heart of any DES library is the code that manages the event set
processing.  Typically that is done via a *priority queue*, with
the word *priority* being interpreted in DES as earliest event time.
This can implemented as a straight queue structure, with all pending
events arranged in time order, or as a *heap*, with the earliest event
always at the top.

However, this approach is slow if coded in R, as it does not take
advantage of operations in which R is efficient, such as matrix
multiplication.  This is why for instance **simmer**'s code for this
portion of the package is written in C++.  In order to stay purely in R,
the **DES** package takes a different approach.

As mentioned earlier, **DES** implements its event set as a matrix.  The
rows of the matrix are not ordered, but the earliest event can be
obtained efficiently using R's **which.min** function on the time column
of the matrix. 

However, even this would be slow for very large event sets.  In **DES**,
we provide the option of pre-calculating arrivals, and this would make
the event sets large.  On the other hand, arrivals are inherently
ordered, so we store them separately.  Then the earliest event time is
determined as the smaller of the first arrival time and the result of
applying **which.min** to the non-arrivals event set.

### Process-oriented DES

All this is how event-oriented systems work.  *Process-oriented* systems
for DES are generally considered to be clearer. A well-kown  example 
is the Python library 
[**SimPy**](https://simpy.readthedocs.io/en/latest/),
on which **simmer** is based.

Process-oriented code is similar to *threads* programming, and may be
implemented using a threads library.  In our above examples,
process-oriented code would be similar in many respects, but with the
difference that we would have a thread for each entity.  In the machine
repair model, for instance, there may be a thread for each machine and a
thread for each repairperson.  The code for machine threads would look
something like

```
repeat
   simulate up time
   wake a repairperson or join queue
   simulate down time 
```

The code for a repairperson might be something like

```
repeat
   deactivate
   upon receiving wakeup signal, simulate repair time
```

By focusing on each individual actors, e.g. individual machines, it is
hoped that the code is clearer.

If implemented using a threads library, each simulation of a period of
time, e.g. up time above, is handled by the thread relinquishing its
timeslice, e.g. via a call to **pthread_suspend** in the **pthreads**
library. A manager thread would then add the relinquishing thread to the
event set, and activate whichever thread has the earliest event time.

Though Python does have a threads capability, **SimPy** instead takes
advantage of Python's  *generator* feature, implementing what amounts to
a non-preemptive threads system.

## Further information on DES

I have an online course on DES, using **SimPy** as the example system.
The course is in the PDF files in
[http://heather.cs.ucdavis.edu/~matloff/156/PLN](http://heather.cs.ucdavis.edu/~matloff/156/PLN),
with the first unit of the <strong>SimPy</strong> tutorial being in the file 
[http://heather.cs.ucdavis.edu/~matloff/156/PLN/DESimIntro.pdf](http://heather.cs.ucdavis.edu/~matloff/156/PLN/DESimIntro.pdf)
See the above directory of PDF files for the remainder of the tutorial.

