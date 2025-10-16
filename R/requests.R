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


get_geodata_return <- function(x) {
  base_request() |>
    httr2::req_body_json(list(geolocations = x)) |>
    pluck_result()
}


bulk_lookup <- function(x, filter_fields = filter_fields()) {
  if (is.null(filter_fields)) {
    req <- base_request()
  } else {
    req <- base_request() |>
      httr2::req_url_query(filter = filter_fields, .multi = "comma")
  }
  req |>
    httr2::req_body_json(list(postcodes = x), auto_unbox = FALSE) |>
    get_json_data()
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
autocomplete_possibly <- purrr::possibly(autocomplete, NA_character_)


pluck_result <- \(req) purrr::pluck(get_json_data(req), "result")


get_json_data <- function(req) {
  req |>
    httr2::req_perform() |>
    httr2::resp_check_status() |>
    httr2::resp_body_json()
}
