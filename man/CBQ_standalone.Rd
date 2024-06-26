% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/standalone.R
\name{CBQ_standalone}
\alias{CBQ_standalone}
\title{CBQ Standalone}
\usage{
CBQ_standalone(languages = psyquest::languages(), ...)
}
\arguments{
\item{languages}{(Character vector)
Determines the languages available to participants.
Possible languages include \code{"en"} (English), \code{"de"} (German), \code{"de_f"} (German (formal), and \code{"ar"} (arabic).
The first language is selected by default.}

\item{...}{Further arguments to be passed to \code{\link{standalone}()}.}
}
\description{
This function launches a standalone testing session for the CBQ questionnaire.
CBQ stands for 'Children's Behavior Questionnaire'.
}
