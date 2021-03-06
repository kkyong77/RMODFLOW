% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rmf-run-opt.R
\name{rmf_run_opt}
\alias{rmf_run_opt}
\alias{run_opt}
\title{Run a MODFLOW model optimization, based on the parameter value file}
\usage{
rmf_run_opt(file, executable = "mf2005", par = NULL, include = NULL,
  trans = NULL, method = "Nelder-Mead", lower = -Inf, upper = Inf,
  control = list(), ...)

run_opt(...)
}
\arguments{
\item{file}{path to name file; typically '*.nam'}

\item{executable}{name of the MODFLOW executable to use}

\item{par}{initial parameter values (for all or only included parameters); current parameter value file values are used if par is not provided}

\item{include}{logical vector indicating which parameters in the parameter value file to include in the optimization}

\item{trans}{vector of transformations; currently only 'log' is supported}

\item{method}{optimization method: 'Nelder-Mead','BFGS','CG','SANN','Brent','L-BGFS-B','spso2011','spso2007','ipso','fips','wfips','canonical' or 'DEoptim'}

\item{lower}{lower parameter bounds}

\item{upper}{upper parameter bounds}

\item{control}{list of control arguments}

\item{...}{further arguments provided to \code{optim}, \code{hydroPSO} or \code{DEoptim}}
}
\value{
\code{optim} results with the full list of parameters
}
\description{
\code{run_modflow_opt} runs a MODFLOW optimization.
}
\section{Functions}{
\itemize{
\item \code{run_opt}: Deprecated function name
}}

