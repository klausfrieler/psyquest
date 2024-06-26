% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/standalone.R
\name{standalone}
\alias{standalone}
\title{Standalone}
\usage{
standalone(
  label,
  languages = psyquest::languages(),
  subscales = NULL,
  short_version = FALSE,
  configuration_filepath = NULL,
  dict = psyquest::psyquest_dict,
  admin_password = "conifer",
  researcher_email = NULL,
  with_id = FALSE,
  validate_id = "auto",
  randomize = FALSE,
  ...
)
}
\arguments{
\item{label}{(Character scalar) Three uppercase letter acronym of the questionnaire.}

\item{languages}{(Character vector)
Determines the languages available to participants.
Possible languages include \code{"en"} (English), and \code{"de"} (German).
The first language is selected by default.}

\item{subscales}{(Character vector) The subscales to be included in the questionnaire.
If no subscales are provided all subscales are selected.}

\item{short_version}{(Boolean scalar) For the short version of the questionnaire
set this to TRUE. Defaults to FALSE.}

\item{configuration_filepath}{(Character scalar) Optional path to a configuration file
exported from the GMSI-Configurator at https://shiny.gold-msi.org/gmsiconfigurator (GMS only).}

\item{dict}{(i18n_dict) The psyquest dictionary used for internationalisation.}

\item{admin_password}{(Character scalar) Password for accessing the admin panel.}

\item{researcher_email}{(Character scalar)
If not \code{NULL}, this researcher's email address is displayed at the
bottom of the screen so that online participants can ask for help.}

\item{with_id}{(Boolean) Flag if ID should be entered}

\item{validate_id}{(Character scalar or closure) Function for validating IDs or string "auto"
for default validation which means ID should consist only of alphanumeric characters.}

\item{...}{Further arguments to be passed to \code{\link{standalone}()}.}
}
\description{
This function launches a standalone testing session for a questionnaire with the specified label.
Valid labels are 'BMR', 'BFT'. 'CBQ', CCM', 'DAC', 'DEG', 'GDS', 'GMS', 'GRT', 'HOP', 'HUM', 'HSP', 'IBQ', MES', 'MES', 'MHE', 'MHI', 'MHP', 'PAC', 'PMS'
'SDQ', 'SEM', 'SES','SMP', 'SOS', 'SWL', TOI', 'TOM', and 'TPI'.
This can be used for data collection, either in the laboratory or online.
}
