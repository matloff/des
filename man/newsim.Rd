\name{newsim,schedevnt,getnextevnt,mainloop,newqueue,appendfcfs,delfcfs,cancelevnt,exparrivals}
\alias{newsim}
\alias{cancelevnt}
\alias{schedevnt}
\alias{getnextevnt}
\alias{mainloop}
\alias{newqueue}
\alias{appendfcfs}
\alias{delfcfs}
\alias{exparrivals}

\title{
Discrete-event simulation routines.
}

\description{
Main simulation routines.
}

\usage{
newsim(timelim,maxesize,appcols=NULL,aevntset = FALSE,dbg=FALSE) 
schedevnt(evnttime,evnttype,simlist,appdata=NULL)
getnextevnt(simlist)
mainloop(simlist) 
newqueue(simlist) 
appendfcfs(queue,jobtoqueue)
delfcfs(queue)
cancelevnt(rownum,simlist)
exparrivals(simlist,meaninterarr,batchsize = 10000)
}

\arguments{
  \item{dbg}{If TRUE, use debug mode, action pausing for each new
      event occurrence.}
  \item{devsetrows}{Number of rows to allocate initially for the event
      set.}
  \item{evnt}{An event.}
  \item{simlist}{An R environment containing the simulation, produced by
      \code{newsim}.}
  \item{evnttime}{Occurrence time for an event.}
  \item{evnttype}{Event type.}
  \item{timelim}{Time limit for simulation.}
  \item{maxesize}{Maximum number of rows needed in the event set matrix,
     excluding separate arrival event rows in the case \code{aevntset} =
     TRUE.}
  \item{x}{Vector to search in.}
  \item{y}{Value searched for.}
  \item{queue}{A queue.  Must be in a \code{simlist} environment.}
  \item{jobtoqueue}{Job to be placed in a queue.}
  \item{meaninterarrival}{Mean time between arrivals.}
  \item{batchsize}{Number of arrivals to generate in one call to
     \code{rexp}.}
}

\details{Discrete event simulation, using the event-oriented approach.

Here is an overview of the functions:

   \itemize{

   \item \code{newsim}:  Creates an R environment, containing the event
   list, current simulated time and so on, including any
   application-specific data.

   \item \code{cancelevnt}:  Removes an event from the event set
   Useful for instance for simulating timeout situations.  
   Removal is done via setting the event time to double \code{timelim}.

   \item \code{schedevnt}:  Creates a new event, and then enters it into the
   event set matrix.

   \item \code{getnextevnt}:  Removes and returns the earliest event
   from the event set.
   Removal is done via setting the event time to double \code{timelim}.

   \item \code{mainloop}:  Called by the application to start the
   simulation and run until the simulated time exceeds the
   user-specified time limit.  At each iteration, calls \code{getnextevnt} 
   and invokes the application-specific reaction function for
   the occurred event.  If \code{dbg} is set, then at each iteration the
   function will enter R \code{browser} mode, printing out the current
   event and simulated time, and giving the user an opportunity to "take
   a look around."

   \item \code{newqueue}:  Create a new work queue, an R environment.
   The main component, \code{m}, is a matrix representing the queue,
   with number of columns being application-dependent.  The user might
   add other components, e.g. running totals.

   \item \code{appendfcfs}:  Appends a job to a First Come, First Served
   queue. The job is represented by a vector to be added as a row in the
   queue matrix.

   \item \code{delfcfs}:  Deletes and returns the head of an FCFS queue.

   }

}

\section{Main Variables}{
These are in the simlist, whose application-ind
components are:

   \itemize{

   \item \code{currtime}:  Current simulated time.

   \item \code{evnts}:  The set of scheduled events; see below.

   \item \code{reactevnt}:  The reaction function, used-defined; see below.

   \item \code{dbg}:  See \code{mainloop} above.

   \item Application-specific variables.

   }
}

\section{Reaction Functions}{
These are user-defined.  The \code{DES} function
\code{mainloop} will make the call

\code{simlist$reactevent(head, simlist)}

where the user has initially set \code{simlist$reactevent} to his/her
application-specific code.  Here \code{head} is the event just now
removed from the head of the event set, and \code{simlist} is the event
set  Let's call this function the "event handler," but note that
within it there are if/else cases, one for each event type.

The For example, consider simulation of a single-server queue.  When a
job arrives, the arrivals section of the event handler will run (coded
by the event type, again user-defined).  It will record the arrival,
update any application-specific totals, and see if service can be
started for this job.  If so, the code will schedule an event for
completion of the service; if not, the code will add the job to the
queue.

}

\section{Outline of Typical Application Code}{

\preformatted{
    mysim <- newsim()    # create the simlist
    set reactevent in mysim
    set application-specific variables in mysim, if any
    set the first event(s) in mysim$evnts
    mainloop(mysim,mysimtimelim)
    print results
}

}

\author{
Norm Matloff
}

