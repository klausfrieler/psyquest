#' TOM
#'
#' This function defines a TOM module for incorporation into a
#' psychTestR timeline.
#' Use this function if you want to include the TOM in a
#' battery of other tests, or if you want to add custom psychTestR
#' pages to your test timeline.
#' For a standalone implementation of the TOM,
#' consider using \code{\link{TOM_standalone}()}.
#' @param label (Character scalar) Label to give the TOM results in the output file.
#' @param dict The psyquest dictionary used for internationalisation.
#' @export
TOM <- function(label = "TOM",
                dict = psyquest::psyquest_dict) {
  stopifnot(purrr::is_scalar_character(label))

  main_test(
    questionnaire = label,
    label = label,
    num_items = 12,
    offset = 1,
    arrange_vertically = FALSE
  )
}