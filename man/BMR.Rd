% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BMR.R
\name{BMR}
\alias{BMR}
\title{BMR}
\usage{
BMR(label = "BMR", dict = psyquest::psyquest_dict, ...)
}
\arguments{
\item{label}{(Character scalar) Three uppercase letter acronym of the questionnaire.
This is also the label given to the results in the output file.}

\item{dict}{(i18n_dict) The psyquest dictionary used for internationalisation.}

\item{...}{Further arguments to be passed to \code{\link{BMR}()}.}
}
\description{
This function defines a Barcelona Reward Music Scale (BMR) module for incorporation into a
psychTestR timeline.
Use this function if you want to include the BMR in a
battery of other tests, or if you want to add custom psychTestR
pages to your test timeline.
For a standalone implementation of the BMR,
consider using \code{\link{BMR_standalone}()}.
}
