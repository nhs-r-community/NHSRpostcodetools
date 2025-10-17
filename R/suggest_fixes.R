#' Return a table with suggested replacements for invalid postcodes
#'
#' Data for terminated invalid codes will be returned according to the nearest
#' current valid code for the old code's longitude/latitude, if possible.
#' Data for codes that are invalid due to an incorrect final letter will be
#' returned according to an autocompletion process, if possible.
#' Missing data will be returned for any other invalid postcodes that could not
#' be replaced using the two methods above.
#' Any valid codes will be excluded from the returned data frame.
#'
#' Consider using [tidy_postcodes] to normalise formatting as far as possible
#'  before using this function.
#'
#' @param codes A character vector of postcodes to be checked for validity
#' @returns A data frame with a row for each invalid postcode supplied.
#' @examples suggest_fixes(c("NP22 3PQ", "NP22 3PR", "NP22 3MN"))
#' @export
suggest_fixes <- function(codes) {
  codes <- unique(toupper(purrr::discard(codes, is.na)))
  assertthat::assert_that(
    rlang::is_character(codes),
    length(codes) > 0L
  )
  valid_index <- purrr::map_lgl(codes, validate_code)
  invalid_codes <- codes[!valid_index]

  if (length(invalid_codes) == 0L) {
    "{.fn suggest_fixes} found no postcodes that need fixing." |>
      cli::cli_alert_success()
    invisible(codes)
  } else {
    terminated_codes_list <- invalid_codes |>
      purrr::map(check_terminated_possibly) |>
      purrr::compact()
    if (length(terminated_codes_list) > 0L) {
      fixed_terminated_data <- terminated_codes_list |>
        purrr::map(tibble::as_tibble_row) |>
        purrr::list_rbind() |>
        dplyr::select(c("postcode", "longitude", "latitude")) |>
        fix_terminated()
    } else {
      fixed_terminated_data <- zero_row()
    }
    unfixed_codes <- setdiff(invalid_codes, fixed_terminated_data[["postcode"]])

    if (length(unfixed_codes) > 0L) {
      fixed_autocomplete_data <- fix_by_autocomplete(unfixed_codes)
    } else {
      fixed_autocomplete_data <- zero_row()
    }

    fixed <- dplyr::bind_rows(fixed_terminated_data, fixed_autocomplete_data)

    tibble::tibble(postcode = invalid_codes) |>
      dplyr::left_join(fixed, "postcode") |>
      dplyr::mutate(
        dplyr::across("note", \(x) tidyr::replace_na(x, "unable to fix"))
      )
  }
}


#' Use reverse geocoding to suggest replacements for terminated postcodes
#' @keywords internal
fix_terminated <- function(dat) {
  geocoded_data <- reverse_geocode(dat)
  if (nrow(geocoded_data) > 0L) {
    dat |>
      dplyr::inner_join(geocoded_data, c("longitude", "latitude")) |>
      dplyr::select(c("postcode", "new_postcode")) |>
      dplyr::mutate(note = "terminated code fixed by reverse geocoding")
  } else {
    zero_row()
  }
}


#' Use the autocompletion API to suggest replacements for invalid postcodes
#' @keywords internal
fix_by_autocomplete <- function(codes) {
  completed_codes <- purrr::map_chr(codes, autocomplete_possibly)
  if (length(completed_codes) > 0L) {
    tibble::tibble(postcode = codes, new_postcode = completed_codes) |>
      dplyr::filter(!dplyr::if_any("new_postcode", is.na)) |>
      dplyr::mutate(note = "fixed by autocompletion")
  } else {
    zero_row()
  }
}


#' A convenience function to generate a 0-row tibble to support suggest_fixes()
#' @keywords internal
zero_row <- function() {
  tibble::tibble(
    postcode = character(),
    new_postcode = character(),
    note = NA_character_
  )
}
