#' Use postcodes.io to get postcode data
#'
#' @param x A data frame with a column of postcodes, or a vector
#'  of postcodes.
#' @param var String or symbol. The name of the variable in the data frame that
#'  comprises the postcodes to be submitted. Should be acceptable as a symbol
#'  or as a standard string.
#' @param fix_invalid Boolean, default `TRUE`. Whether to try to fix any
#'  postcodes that are not found (potentially because they are terminated codes,
#'  or contain typos).
#'
#' @examples
#' postcodes <- c("HD1 2UT", "HD1 2UU", "HD1 2UV")
#' test_df1 <- dplyr::tibble(
#'   place = paste0("place_", 1:3),
#'   postcode = postcodes
#' )
#' postcode_data_join(test_df1, fix_invalid = TRUE)
#' @export
postcode_data_join <- function(x, var = "postcode", fix_invalid = TRUE) {
  valid_results <- NULL
  fixed_terminated_data <- NULL
  fixed_autocomp_data <- NULL
  terminated_codes <- NULL
  autocomp_codes <- NULL
  remainder <- NULL

# is_character (vector) ------------------------------------------------------


  var <- rlang::as_string(var)

  if (is.data.frame(x)) {
    assertthat::assert_that(
      var %in% names(x),
      msg = "That variable doesn't seem to exist in this data frame."
    )

    codes <- x |>
      dplyr::pull({{ var }}) |>
      unique()
  } else {
    assertthat::assert_that(rlang::is_character(x))
    codes <- unique(x)
  }

  assertthat::assert_that(
    length(codes) > 0L,
    msg = "No postcodes have been found."
  )

  codes <- toupper(codes)

  valid_codes <- codes |>
    purrr::keep(validate_code)
  invalid_codes <- setdiff(codes, valid_codes)


  # If we have some invalid codes then try to fix them by, firstly, seeing if
  # they are terminated codes, finding the lon/lat and then reverse geocoding
  # the lon/lat to get the current code;
  # then if that fails, returning a nearby code using the autocomplete feature
  # (assuming that codes with only single final character different are
  # geographically near each other - I think this is sound).

  if (length(invalid_codes)) {
    if (fix_invalid) {
      # filter out any invalid codes that match a terminated code...
      terminated_codes_data <- invalid_codes |>
        purrr::map_df(check_term_possibly)

      # ...and find the current nearest code for the same lon/lat
      if (nrow(terminated_codes_data) > 0L) {
        fixed_term_codes_data <- terminated_codes_data |>
          dplyr::select(all_of(c("longitude", "latitude"))) |>
          bulk_reverse_geocode() |>
          unnest_codes() |>
          dplyr::rename(new_postcode = "postcode") |>
          dplyr::select(!"distance")

        fixed_terminated_data <- terminated_codes_data |>
          dplyr::select("postcode") |>
          dplyr::bind_cols(fixed_term_codes_data)

        terminated_codes <- terminated_codes_data[["postcode"]]

        usethis::ui_info(paste0(
          "The following postcodes are terminated:\n",
          fixed_terminated_data[["postcode"]],
          "\nand have been replaced with these current postcodes:\n",
          fixed_terminated_data[["new_postcode"]]
        ))
      }

      invalid_codes <- invalid_codes |>
        setdiff(terminated_codes)

      if (length(invalid_codes)) {
        ac_results <- invalid_codes |>
          purrr::map(autocomplete_possibly) |>
          purrr::compact()

        if (length(ac_results)) {
          ac_wins <- ac_results |>
            purrr::map_lgl(purrr::negate(rlang::is_null)) |>
            which()
          autocomp_codes <- invalid_codes[ac_wins]

          fixed_ac_data <- ac_results |>
            purrr::list_c() |>
            batch_it(100L) |>
            purrr::map_df(bulk_lookup) |>
            unnest_codes() |>
            dplyr::rename(new_postcode = "postcode")

          assertthat::are_equal(length(autocomp_codes), nrow(fixed_ac_data))

          fixed_autocomp_data <- tibble::tibble(postcode = autocomp_codes) |>
            dplyr::bind_cols(fixed_ac_data)


          usethis::ui_info(paste0(
            "The following postcodes are invalid:\n",
            autocomp_codes,
            "\nand have been replaced with these nearby postcodes:\n",
            fixed_ac_data[["new_postcode"]]
          ))
        } else {
          autocomp_codes <- NULL
          fixed_autocomp_data <- NULL
        }
      }
    }

    remainder <- invalid_codes |>
      setdiff(c(terminated_codes, autocomp_codes))

    if (length(remainder)) {
      usethis::ui_info(paste0(
        "The following postcodes are invalid:\n",
        remainder,
        "\nbut have not been successfully replaced with valid codes."
      ))
    }
  }

  if (length(valid_codes)) {
    valid_results <- valid_codes |>
      batch_it(100L) |>
      purrr::map_df(bulk_lookup) |>
      unnest_codes() |>
      dplyr::mutate(new_postcode = .data[["postcode"]], .after = "postcode")
  }

  postcode_data <- dplyr::bind_rows(
    list(
      valid = valid_results,
      terminated = fixed_terminated_data,
      autocompleted = fixed_autocomp_data
    ),
    .id = "result_type"
  ) |>
    dplyr::relocate("result_type", .after = "new_postcode")

  if (is.data.frame(x)) {
    x |>
      dplyr::left_join(postcode_data,
        by = vctrs::vec_c({{ var }} := "postcode")
      )
  } else if (rlang::is_character(x)) {
    tibble::tibble({{ var }} := x) |>
      dplyr::left_join(postcode_data,
        by = vctrs::vec_c({{ var }} := "postcode")
      )
  } else {
    postcode_data
  }
}
