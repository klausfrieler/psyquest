#' SCS
#'
#' This function defines a SCS module for incorporation into a
#' psychTestR timeline.
#' Use this function if you want to include the SCS in a
#' battery of other tests, or if you want to add custom psychTestR
#' pages to your test timeline.
#' For a standalone implementation of the SCS,
#' consider using \code{\link{SCS_standalone}()}.
#' @param label (Character scalar) Label to give the SCS results in the output file.
#' @param dict The psyquest dictionary used for internationalisation.
#' @export
SCS <- function(label = "SCS",
                dict = psyquest::psyquest_dict) {
  stopifnot(purrr::is_scalar_character(label))

  main_test(
    questionnaire = label,
    label = label,
    num_items = 25,
    offset = 1,
    arrange_vertically = FALSE
  )
}