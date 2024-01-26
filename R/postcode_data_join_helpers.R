req_base <- function(x = "") {
  ua <- "github.com/nhs-r-community/NHSRpopulation // httr2" # nolint

  paste0("https://api.postcodes.io/", x, "postcodes/") |>
    httr2::request() |>
    httr2::req_user_agent(ua)
}



pluck_result <- function(req) {
  req |>
    httr2::req_perform() |>
    httr2::resp_body_json() |>
    purrr::pluck("result")
}



validate_code <- function(x) {
  req_base() |>
    httr2::req_url_path_append(URLencode(x)) |>
    httr2::req_url_path_append("/validate") |>
    pluck_result()
}



check_term <- function(x) {
  req_base("terminated_") |>
    httr2::req_url_path_append(URLencode(x)) |>
    pluck_result()
}
check_term_possibly <- purrr::possibly(check_term, otherwise = NULL)




bulk_reverse_geocode <- function(.data) {
  req_base() |>
    httr2::req_body_json(list(geolocations = .data)) |>
    httr2::req_url_query(limit = 1L) |>
    pluck_result() |>
    purrr::map_df("result")
}




unnest_codes <- function(.data) {
  .data |>
    dplyr::mutate(codes_names = names(.data[["codes"]])) |>
    dplyr::mutate(across("codes", unlist)) |>
    dplyr::rename(code = "codes") |>
    tidyr::pivot_wider(
      names_from = "codes_names",
      names_glue = "{codes_names}_{.value}",
      values_from = "code"
    )
}




autocomplete <- function(x) {
  # Create incomplete postcode: If x ends in two letters, keep only the first
  # one. If x ends in a single letter (i.e. already incomplete), still keep it.
  x <- stringr::str_replace(x, "([:alpha:]?)([:alpha:]?)$", "\\1") |>
    URLencode()
  req_base() |>
    httr2::req_url_path_append(x) |>
    httr2::req_url_path_append("/autocomplete") |>
    httr2::req_url_query(limit = 1L) |>
    pluck_result() |>
    unlist() |>
    sample(1L)
}
autocomplete_possibly <- purrr::possibly(autocomplete, otherwise = NULL)




bulk_lookup <- function(x) {
  req_base() |>
    httr2::req_body_json(list(postcodes = x), auto_unbox = FALSE) |>
    pluck_result() |>
    purrr::map_df("result")
}
