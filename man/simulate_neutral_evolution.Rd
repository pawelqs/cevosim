% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/neutral_evolution.R
\name{simulate_neutral_evolution}
\alias{simulate_neutral_evolution}
\title{Simulate neutrally evolving population with constant mutation rate}
\usage{
simulate_neutral_evolution(
  b = 0.1,
  d = 0.05,
  u_init = 1,
  start_cells = 1,
  max_cells = 1e+05,
  verbose = FALSE
)
}
\arguments{
\item{b}{birth rate}

\item{d}{death rate}

\item{u_init}{initial mutation rate}

\item{start_cells}{init population size}

\item{max_cells}{maximal population size}

\item{verbose}{verbose}
}
\value{
popolation list
}
\description{
Simulate neutrally evolving population with constant mutation rate
}
