#' @keywords internal
"_PACKAGE"

#' @importFrom rlang .data `:=`
NULL


#' Batch a vector or list into a list of elements with a maximum size
#'
#' @param x A vector or list
#' @param batch_size integer. The size (length) of batches to create
#' @examples
#'   batch_it(letters, 6L)
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
