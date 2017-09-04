# DES
Discrete-Event Simulation in R

*Discrete event simulation* refers to the simulation of systems that
have abrupt, i.e. discrete, changes. In a queuing system, for instance,
when a new job arrives, the queue length abruptly increases by 1.

The package here, **DES**, is not the fastest. I recommend the
[excellent **simmer** package](https://cran.rstudio.com/web/packages/simmer/index.html) 
if speed is an issue.  On the other hand, **DES** is much easier to 
learn (good for teaching, for instance), and gives the programmer 
more control.

The package uses the *event-oriented* approach, which means the
programmer codes how the system reacts to any specific event.  To see
how this works, consider a classic specific, the M/M/1 queue:  Interjob
arrival times and job service times are exponentially distributed, and
there is 1 server.

Here there are two kinds of events, job arrival and job completion.
Think about how the system reacts to an arrival:

1. If the server is free, start service for this job.  Otherwise, add
   the job to the queue.

2. Wait for the next arrival.

In **DES**, one could code these as follows.  (This is from the file
**inst/examples/MM1.R** in the package.) We have named the
application-specific reaction function **mm1react**.  **DES**
requires that it have the call form

```R
mm1react(evnt,simlist)
```

The **DES** object **evnt** carrying information about a simulated event
that has just occurred, and **simlist** is a **DES** object giving all
the information about the simulation.  The first two lines 

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

That first line simulates the time of the next arrival.  The component
**currtime** contains the current simulated time, and we generate a
random time until the next arrival.  The call to the built-in **DES**
function **schedevnt** then adds this new arrival to the **DES** event
list.  It's always nice in this kind of simulation to assign an ID to
each job, hence our inclusion of a job number here.

Before looking at the rest of the above code, let's talk briefly about
the **DES** internal structure.  The key data structure is the event
list, each row of which stores a pending event.  The call to
**schedevnt** above adds a new row for that event.  Each row has the
form

<pre>
occurrence time
event type (user-defined code, e.g. 1 for arrival, 2 for 
   job completion, etc.)
application-specific information, if any
</pre>

An example of application-specific information is **jobnum** above.

What the **DES** main loop does, then, is that in each iteration it
finds the next event, i.e. the one with the earliest event time.  It
removes that event from the event list, and then calls the user-supplied
reaction function, which is **mm1react** in our example here.  The
reaction function then reacts!  Typically that function will generate
one or more new events, adding them to the event list via a call to
**schedevnt**.

Now returning to the above code, it checks whether the server is busy.
If not, then the newly-arrived job can start service.  A random service
time is generated, and **schedevnt** is called to add this new service
completion event to the event list.  On the other hand, if the server is
busy, we add the newly-arrived job to the queue, via a call to the
**DES** function **appendfcfs**.

All this is how event-oriented systems work.  *Process-oriented* systems
are very similar to the above, except that there would not be code to
generate the next arrival upon the occurrence of one arrival.
