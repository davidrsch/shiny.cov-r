# Helper function for the multi-file test app

compute_bins <- function(x, n_bins) {
  seq(min(x), max(x), length.out = n_bins + 1)
}

format_label <- function(bin_count) {
  paste("Using", bin_count, "bins")
}
