#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
#' @importFrom dplyr across c_across desc join_by
#' @importFrom magrittr %>%
#' @importFrom rlang `:=`
#' @importFrom tidyselect all_of any_of contains ends_with everything
#' @importFrom tidyselect last_col matches num_range starts_with
#' @importFrom usethis ui_info ui_stop ui_oops ui_nope ui_code

# Borrow `view()` from `{tibble}`
#' @export
view <- tibble::view

NULL
