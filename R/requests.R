base_request <- function(x = "postcodes") {
  req <- httr2::request("https://api.postcodes.io/")
  req |>
    httr2::req_user_agent("github.com/nhs-r-community // httr2") |>
    httr2::req_url_path_append(x)
}


validate_code <- function(code) {
  base_request() |>
    httr2::req_url_path_append(utils::URLencode(code)) |>
    httr2::req_url_path_append("validate") |>
    pluck_result()
}


check_terminated <- function(code) {
  base_request("terminated_postcodes") |>
    httr2::req_url_path_append(utils::URLencode(code)) |>
    pluck_result()
}
check_terminated_possibly <- purrr::possibly(check_terminated)


bulk_reverse_geocode <- function(dat, prev_data = NULL, curr_radius = 125L) {
  if (curr_radius > 2000L) {
    cli::cli_alert_info(paste0(
      "Geocoding searches have not found some replacement postcodes despite ",
      "searching up to 2km in radius from the original postcode location."
    ))
    prev_data # return early
  }

  geodata_return <- dat |>
    dplyr::select(c("longitude", "latitude")) |>
    dplyr::mutate(limit = 1L) |>
    dplyr::mutate(radius = curr_radius) |>
    dplyr::mutate(batch = ceiling(dplyr::row_number() / 100L)) |>
    # batch into groups of max 100 rows
    tidyr::nest(.by = "batch") |>
    dplyr::pull("data") |>
    purrr::map(get_geodata_return) |>
    purrr::list_flatten() # re-combine batches into a single list

  geodata_queries <- geodata_return |>
    purrr::map("query")

  geodata_query_results <- geodata_return |>
    purrr::map("result") |>
    purrr::list_flatten()

  nifty_bind_cols <- function(x, y) {
    x2 <- x |>
      tibble::as_tibble_row() |>
      dplyr::select(c("longitude", "latitude")) |>
      dplyr::rename_with(\(x) paste0("orig_", x))
    if (!is.null(y)) {
      y2 <- y |>
        purrr::compact() |>
        tibble::as_tibble() |>
        unnest_codes()
    } else {
      y2 <- NULL
    }
    dplyr::bind_cols(x2, y2)
  }

  data_out <- geodata_queries |>
    purrr::map2(geodata_query_results, nifty_bind_cols) |>
    purrr::list_rbind()

  if (!"postcode" %in% colnames(data_out)) {
    data_out <- data_out |>
      dplyr::mutate(postcode = NA_character_)
  }

  dat_done <- data_out |>
    dplyr::filter(!dplyr::if_any("postcode", is.na)) |>
    dplyr::bind_rows(prev_data)

  dat_missing <- data_out |>
    dplyr::filter(dplyr::if_any("postcode", is.na)) |>
    dplyr::select(tidyselect::starts_with("orig_")) |>
    # rename to "longitude" and "latitude" for resubmission
    dplyr::rename_with(\(x) sub("^orig_", "", x))

  if (nrow(dat_missing) > 0L) {
    bulk_reverse_geocode(dat_missing, dat_done, curr_radius * 2)
  } else {
    dat_done
  }
}


autocomplete <- function(code) {
  # Create incomplete postcode: If x ends in two letters, keep only the first
  # one. If x ends in a single letter (i.e. already incomplete), still keep it.
  # Rationale: if we have a "complete" but actually non-existent postcode like
  # HD1 1BC, we can create a valid ~nearby postcode by stripping out the last
  # letter and then using the API's autocomplete feature to supply a valid code
  code <- sub("([0-9])([[:alpha:]])[[:alpha:]]?$", "\\1\\2", code)
  base_request() |>
    httr2::req_url_path_append(utils::URLencode(code)) |>
    httr2::req_url_path_append("autocomplete") |>
    httr2::req_url_query(limit = 5L) |>
    pluck_result() |>
    unlist() |>
    sample(1L)
}
autocomplete_possibly <- purrr::possibly(autocomplete)


bulk_lookup <- function(x) {
  base_request() |>
    httr2::req_body_json(list(postcodes = x), auto_unbox = FALSE) |>
    get_json_data()
}


get_geodata_return <- function(x) {
  base_request() |>
    httr2::req_body_json(list(geolocations = x)) |>
    pluck_result()
}


pluck_result <- \(req) purrr::pluck(get_json_data(req), "result")


get_json_data <- function(req) {
  req |>
    httr2::req_perform() |>
    httr2::resp_check_status() |>
    httr2::resp_body_json()
}
