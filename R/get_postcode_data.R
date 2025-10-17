#' Use postcodes.io to get postcode data
#'
#' See \url{https://postcodes.io/docs/postcode/lookup} for details of the data
#'  that the API returns
#'
#' @param x A vector of postcodes
#' @param as_list boolean, default `FALSE`. The default behaviour is to
#'  extract the data from the list and return it as a tibble. If set `TRUE`,
#'  an unprocessed list of the JSON data from the API will be returned instead
#' @param filter character vector or helper function. The default is
#'  [filter_fields], which by default evaluates to `NULL`, which means no
#'  fields will be filtered out from the returned data. You can also use
#'  [filter_fields] to validate a vector of field names that you supply - see
#'  Examples below.
#'  The full list of field names is available in the [schema_table] data. Some
#'  other helper functions are provided for common scenarios: [exclude_codes]
#'  and [minimal_fields].
#'  See \url{https://postcodes.io/docs/postcode/bulk} for details.
#' @examples
#'  codes <- c("NP22 3PS", "NP22 4PS", "NP22 5PS")
#'  get_postcode_data(codes)
#'  my_fields <- c("postcode", "lsoa", "codes.lsoa", "eastings", "northings")
#'  # get_postcode_data(codes, filter = my_fields) is fine, but using
#'  # filter_fields() to wrap your vector gives you a validation check:
#'  get_postcode_data(codes, filter = filter_fields(my_fields))
#'  # The `schema_table` dataset within NHSRpostcodetools contains a "field"
#'  # column with all available fields. You can use this as a starting point:
#'  excl_fields <- setdiff(schema_table[["field"]], c("quality", "country"))
#'  get_postcode_data(codes, filter = filter_fields(excl_fields))
#'  # or use a helper function as a starting point to add to:
#'  get_postcode_data(codes, TRUE, filter_fields(c(minimal_fields(), "region")))
#' @returns A tibble by default, otherwise a list if `as_list` is TRUE
#' @export
get_postcode_data <- function(x, as_list = FALSE, filter = filter_fields()) {
  x <- unique(toupper(purrr::discard(x, is.na)))
  assertthat::assert_that(length(x) > 0L, msg = "No postcodes have been found.")

  valid_index <- purrr::map_lgl(x, validate_code, .progress = "Checking codes")
  valid_codes <- x[valid_index]
  invalid_codes <- x[!valid_index]

  if (length(invalid_codes) > 0L) {
    inv <- cli::cli_vec(invalid_codes, list(`vec-trunc` = 5L)) # nolint
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
      purrr::map(
        \(x) bulk_lookup(x, filter_fields = filter),
        .progress = "Looking up postcode data..."
      )

    if (as_list) {
      results_list
    } else {
      tibblise_results_list(results_list)
    }
  } else {
    cli::cli_alert_warning("No valid postcodes were supplied")
    invisible(NULL)
  }
}


#' Look up data for postcodes from a data frame column and join the results on
#'
#' Please note that the function will attempt to format the postcode column to
#'  match the standard "A(A)D(D) DAA" format. This is necessary for the join to
#'  succeed! You can use the [tidy_postcodes] function yourself in order to have
#'  more supervision of this formatting - or potentially before running
#'  [suggest_fixes].
#'
#' @param tbl A data frame
#' @param .col string. The name of the column that contains the postcodes.
#'   `"postcode"` by default.
#' @inheritParams get_postcode_data
#'
#' @examples
#' tibble::tibble(
#'   place = paste0("place_", seq(3L)),
#'   postcode = c("NP22 3PS", "NP22 4PS", "NP22 5PS")
#' ) |>
#'   postcode_data_join()
#' @export
postcode_data_join <- function(
  tbl,
  .col = "postcode",
  filter = filter_fields()
) {
  assertthat::assert_that(
    inherits(tbl, "data.frame"),
    .col %in% colnames(tbl)
  )
  assertthat::assert_that(
    (is.null(filter) || "postcode" %in% filter),
    msg = cli::cli_abort(paste0(
      "The {.val postcode} field must be included in {.arg filter} in order ",
      "for {.fn postcode_data_join} to work."
    ))
  )
  tbl <- dplyr::mutate(tbl, dplyr::across({{ .col }}, tidy_postcodes))
  api_data <- get_postcode_data(tbl[[.col]], filter = filter)
  dplyr::left_join(tbl, api_data, by = dplyr::join_by({{ .col }} == "postcode"))
}

tibblise_results_list <- function(results_list) {
  results_list |>
    purrr::map("result") |>
    purrr::list_flatten() |>
    purrr::map("result") |>
    purrr::map(purrr::compact) |>
    # If result contains nested codes then flatten these out and rename.
    # Hopefully this doesn't break things if codes are not included!
    purrr::map(\(x) purrr::list_flatten(x, name_spec = "{inner}_code")) |>
    purrr::map(tibble::as_tibble_row) |>
    purrr::list_rbind()
}
