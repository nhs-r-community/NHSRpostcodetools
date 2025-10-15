#' Return data for suggested replacements for invalid postcodes
#'
#' Data for terminated invalid codes will be returned according to the nearest
#' current valid code for the old code's longitude/latitude, if possible.
#' Data for codes that are invalid due to an incorrect final letter will be
#' returned according to an autocompletion process, if possible.
#' Missing data will be returned for any other invalid postcodes that could not
#' be replaced using the two methods above.
#' Any valid codes will be excluded from the returned data frame.
#'
#' @param codes A character vector of postcodes to be checked for validity
#' @returns A data frame with a row for each invalid postcode supplied.
#' @examples suggest_fixes(c("hd1 2ut", "hd1 2uu", "hd1 2uv"))
#' @export
suggest_fixes <- function(codes) {
  codes <- unique(toupper(purrr::discard(codes, is.na)))
  assertthat::assert_that(
    rlang::is_character(codes),
    length(codes) > 0
  )
  valid_index <- codes |>
    purrr::map_lgl(validate_code, .progress = "Checking postcodes...")
  invalid_codes <- codes[!valid_index]

  if (length(invalid_codes) == 0L) {
    "{.fn postcode_data_join} found no postcodes that need fixing." |>
      cli::cli_alert_success()
    invisible(NULL)
  } else {
    terminated_codes_data <- invalid_codes |>
      purrr::map(check_terminated_possibly) |>
      purrr::compact() |>
      purrr::map(tibble::as_tibble_row) |>
      purrr::list_rbind()
    if (nrow(terminated_codes_data) > 0L) {
      fixed_terminated_data <- fix_terminated(terminated_codes_data)
    } else {
      fixed_terminated_data <- NULL
    }
    if (!is.null(fixed_terminated_data)) {
      unfixed_codes <- invalid_codes |>
        setdiff(fixed_terminated_data[["postcode"]])
    } else {
      unfixed_codes <- invalid_codes
    }
    if (length(unfixed_codes) > 0L) {
      fixed_autocomplete_data <- fix_by_autocomplete(unfixed_codes)
    } else {
      fixed_autocomplete_data <- NULL
    }

    fixed_results <- list(
      `original code terminated` = fixed_terminated_data,
      `original code invalid` = fixed_autocomplete_data
    ) |>
      purrr::list_rbind(names_to = "notes")

    tibble::tibble(postcode = invalid_codes) |>
      dplyr::left_join(fixed_results, "postcode") |>
      dplyr::mutate(
        dplyr::across("notes", \(x) tidyr::replace_na(x, "unable to fix"))
      )
  }
}


fix_terminated <- function(dat) {
  geocoded_data <- bulk_reverse_geocode(dat)
  if (nrow(geocoded_data) > 0L) {
    fixed_terminated_codes <- geocoded_data |>
      dplyr::rename(new_postcode = "postcode")
    dat |>
      dplyr::rename_with(
        \(x) paste0("orig_", x),
        .cols = tidyselect::ends_with("tude")
      ) |>
      dplyr::inner_join(
        fixed_terminated_codes,
        c("orig_longitude", "orig_latitude")
      ) |>
      dplyr::select(!c("orig_longitude", "orig_latitude"))
  } else {
    NULL
  }
}


fix_by_autocomplete <- function(codes) {
  autocomplete_results <- purrr::map_chr(codes, autocomplete_possibly)

  ac_data <- tibble::tibble(
    postcode = codes,
    new_postcode = autocomplete_results
  )
  ac_codes <- purrr::discard(autocomplete_results, is.na)

  if (length(ac_codes) > 0L) {
    fixed_ac_data <- ac_codes |>
      batch_it(100L) |>
      purrr::map(bulk_lookup) |>
      tibblise_results_list() |>
      dplyr::rename(new_postcode = "postcode")
    dplyr::left_join(ac_data, fixed_ac_data, "new_postcode")
  } else {
    ac_data
  }
}


tibblise_results_list <- function(results_list) {
  results_list |>
    purrr::map("result") |>
    purrr::list_flatten() |>
    purrr::map("result") |>
    purrr::map(purrr::compact) |>
    purrr::map(\(x) purrr::list_flatten(x, name_spec = "{inner}_code")) |>
    purrr::map(tibble::as_tibble_row) |>
    purrr::list_rbind()
}
