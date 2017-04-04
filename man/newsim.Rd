\name{newsim,insevnt,schedevnt,getnextevnt,mainloop,binsearch,newqueue,appendfcfs,delfcfs,cancelevnt}
\alias{newsim}
\alias{insevnt}
\alias{cancelevnt}
\alias{schedevnt}
\alias{getnextevnt}
\alias{mainloop}
\alias{binsearch}
\alias{newqueue}
\alias{appendfcfs}
\alias{delfcfs}

\title{
Discrete-event simulation routines.
}

\description{
Main simulation routines.
}

\usage{
newsim(appcols=NULL,dbg=FALSE) 
insevnt(evnt,simlist)
schedevnt(evnttime,evnttype,simlist,appdata=NULL)
getnextevnt(simlist)
mainloop(simlist,simtimelim) 
binsearch(x,y) 
newqueue(ncol) 
appendfcfs(queue,jobtoqueue)
delfcfs(queue)
}

\arguments{
  \item{dbg}{If TRUE, use debug mode, action pausing for each new
      event occurrence.}
  \item{evnt}{An event.}
  \item{simlist}{An R environment containing the simulation, produced by
      \code{newsim}.}
  \item{evnttime}{Occurrence time for an event.}
  \item{evnttype}{Event type.}
  \item{simtimelim}{Time limit for simulation.}
  \item{x}{Vector to search in.}
  \item{y}{Value searched for.}
  \item{queue}{A queue.  Must be in a \code{simlist} environment.}
  \item{jobtoqueue}{Job to be placed in a queue.}
}

\details{Discrete event simulation, using the event-oriented approach.
See the PDF files in
\url{http://heather.cs.ucdavis.edu/~matloff/156/PLN} for an online course
on DES methods.  Note:  This package is intended as a convenience for R
users; it is not as fast as C/C++ libraries.

Here is an overview of the functions:

   \itemize{

   \item \code{newsim}:  Creates an R environment, containing the event
   list, current simulated time and so on, including any
   application-specific data.

   \item \code{insevnt}:  Inserts an event into the event list.

   \item \code{cancelevnt}:  Removes an event into the event list.
   Useful for instance for simulating timeout situations.

   \item \code{schedevnt}:  Creates a new event, and then calls
   \code{insevnt} to insert it.

   \item \code{getnextevnt}:  Removes and returns the earliest event
   from the event list.

   \item \code{mainloop}:  Called by the application to start the
   simulation and run until the simulated time exceeds the
   user-specified time limit.  At each iteration, calls
   \code{getnextevnt} and invokes the application-specific reaction to
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

Main variables:  These are in the simlist, whose application-ind
components are:

   \itemize{

   \item \code{currtime}:  Current simulated time.

   \item \code{evnts}:  The list of scheduled events; see below.

   \item \code{reactevnt}:  Event handler, used-defined; see below.

   \item \code{dbg}:  See \code{mainloop} above.

   \item Application-specific variables.

   }

Structure of the event list:  One row for each event, rows ordered by
event occurrence time.  First two columns are event time and event type
(user-defined). The remaining columns, if any, contain
application-specific information.

Event handlers:  These are user-defined.  The \code{DES} function
\code{mainloop} will make the call

\code{simlist$reactevent(head, simlist)}

where the user has initially set \code{simlist$reactevent} to his/her
application-specific code.  Here \code{head} is the event just now
removed from the head of the event list, and \code{simlist} is the event
list.  Let's call this function the "event handler," but note that
within it there are if/else cases, one for each event type.

The For example, consider simulation of a single-server queue.  When a
job arrives, the arrivals section of the event handler will run (coded
by the event type, again user-defined).  It will record the arrival,
update any application-specific totals, and see if service can be
started for this job.  If so, the code will schedule an event for
completion of the service; if not, the code will add the job to the
queue.

Outline of typical application code:

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

