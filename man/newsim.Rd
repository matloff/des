\name{newsim,insevnt,schedevnt,getnextevnt,mainloop,binsearch,newqueue,appendfcfs,delfcfs}
\alias{newsim}
\alias{insevnt}
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
newsim(dbg=FALSE)
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
  \item{queue}{A queue.}
  \item{jobtoqueue}{Job to be placed in a queue.}
}

\author{
Norm Matloff
}

