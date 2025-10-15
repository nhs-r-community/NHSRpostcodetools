#' Use postcodes.io to get postcode data
#'
#' See \url{https://postcodes.io/docs/postcode/lookup} for details of the data
#'  that the API returns
#'
#' @param x A vector of postcodes
#' @param as_list boolean, default `FALSE`. The default behaviour is to
#'  extract the data from the list and return it as a tibble. If set `TRUE`,
#'  an unprocessed list of the JSON data from the API will be returned instead
#' @param include_codes boolean, default `TRUE`. Include columns for the ONS
#'  administrative codes for geographic units in the returned tibble. Irrelevant
#'  if `as_list` is `TRUE`; in this case all ONS codes are included as a
#'  nested list in the list data for each postcode.
#'  See \url{https://postcodes.io/docs/postcode/lookup} for details
#' @examples
#' get_postcode_data(c("NP22 3PS", "NP22 4PS", "NP22 5PS"))
#' @returns A tibble by default, otherwise a list if `as_list` is TRUE
#' @export
get_postcode_data <- function(x, as_list = FALSE, include_codes = TRUE) {
  x <- unique(toupper(purrr::discard(x, is.na)))
  assertthat::assert_that(length(x) > 0, msg = "No postcodes have been found.")

  valid_index <- purrr::map_lgl(x, validate_code, .progress = "Checking codes")
  valid_codes <- x[valid_index]
  invalid_codes <- x[!valid_index]

  if (length(invalid_codes) > 0L) {
    inv <- cli::cli_vec(invalid_codes, list(`vec-trunc` = 5))
    paste0(
      "{.fn get_postcode_data} found {length(invalid_codes)} invalid ",
      "postcode{?s}. Example{?s}: {.val {inv}}. You can use ",
      "{.fn suggest_fixes} to try to find valid replacement postcodes."
    ) |>
      cli::cli_alert_warning(wrap = TRUE)
  }

  if (length(valid_codes) > 0L) {
    results_list <- valid_codes |>
      batch_it(100L) |>
      purrr::map(bulk_lookup, .progress = "Looking up postcode data...")

    if (as_list) {
      results_list
    } else {
      results_df <- tibblise_results_list(results_list)
      if (!include_codes) {
        dplyr::select(results_df, !tidyselect::ends_with("_code"))
      } else {
        results_df
      }
    }
  } else {
    invisible(NULL)
  }
}


#' Look up data for postcodes from a data frame column and join the results on
#'
#' @param tbl A data frame
#' @param .col string. The name of the column that contains the postcodes.
#'   `"postcode"` by default.
#' @inheritParams get_postcode_data
#'
#' @examples
#' tibble::tibble(
#'   place = paste0("place_", 1:3),
#'   postcode = c("NP22 3PS", "NP22 4PS", "NP22 5PS")
#' ) |>
#'   postcode_data_join()
#' @export
postcode_data_join <- function(tbl, .col = "postcode", include_codes = TRUE) {
  assertthat::assert_that(
    inherits(tbl, "data.frame"),
    .col %in% colnames(tbl)
  )
  api_data <- get_postcode_data(tbl[[.col]], include_codes = include_codes)
  dplyr::left_join(tbl, api_data, by = dplyr::join_by({{ .col }} == "postcode"))
}


#' Unnest codes (wider): from a list-col to a column each
#' @param tbl A data frame with ONS (etc.) codes data in a list-col
#' @keywords internal
unnest_codes <- function(tbl) {
  tbl |>
    dplyr::mutate(codes_names = names(.data[["codes"]])) |>
    dplyr::mutate(dplyr::across("codes", unlist)) |>
    tidyr::pivot_wider(
      names_from = "codes_names",
      names_glue = "{codes_names}_code",
      values_from = "codes"
    )
}
