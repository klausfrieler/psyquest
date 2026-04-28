#' ISM
#'
#' This function defines a ISM module for incorporation into a
#' psychTestR timeline.
#' Use this function if you want to include the ISM in a
#' battery of other tests, or if you want to add custom psychTestR
#' pages to your test timeline.
#' For a standalone implementation of the ISM,
#' consider using \code{\link{ISM_standalone}()}.
#' @param label (Character scalar) Three uppercase letter acronym of the questionnaire.
#' This is also the label given to the results in the output file.
#' @param dict (i18n_dict) The psyquest dictionary used for internationalisation.
#' @param subscales (Character vector) The subscales to be included in the questionnaire.
#' Possible subscales are \code{"Goals Choice"}, and \code{"Theory of Inteligence"}.
#' If no subscales are provided all subscales are selected.
#' @param ... Further arguments to be passed to \code{\link{ISM}()}.
#' @export
ISM <- function(label = "ISM",
                dict = psyquest::psyquest_dict,
                subscales = c(),
                ...) {
  stopifnot(purrr::is_scalar_character(label))

  questionnaire_id <- "ISM"

  main_test(
    questionnaire_id = questionnaire_id,
    label = label,
    items = get_items(questionnaire_id,
                      subscales = subscales),
    subscales = subscales,
    offset = 0,
    arrange_vertically = TRUE,
    button_style = "min-width: 326px",
    dict = dict
  )
}
