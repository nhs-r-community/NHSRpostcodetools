#' Helper function to check a vector of fields to filter
#' @param fields character vector. See [schema_table].
#' @export
filter_fields <- function(fields = NULL) {
  if (!is.null(fields)) {
    fields <- unique(rlang::arg_match(fields, schema_fields, multiple = TRUE))
  }
  fields
}

#' Provides a vector of fields to return, which excludes all ONS code fields
#' @export
exclude_codes <- \() purrr::discard(schema_fields, \(x) grepl("^codes", x))

#' Provides a minimal vector of fields to return
#' @export
minimal_fields <- \() c("postcode", "lsoa", "msoa", "admin_district")


#' Batch a vector or list into a list of elements with a maximum size
#'
#' @param x A vector or list
#' @param batch_size integer. The size (length) of batches to create
#' @examples batch_it(letters, 6L)
#' @returns A list
#' @export
batch_it <- function(x, batch_size) {
  assertthat::assert_that(
    rlang::is_vector(x),
    rlang::is_scalar_integerish(batch_size),
    batch_size >= 1L
  )
  bsize <- min(length(x), batch_size)

  # Create a vector of factors of length length(x), then pass this as the factor
  # argument to [split()].
  f <- rep(seq_len(ceiling(length(x) / bsize)), each = bsize)[seq_along(x)]
  unname(split(x, f))
}
