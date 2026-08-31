skip_if_no_reflora <- function() {
  if (!identical(Sys.getenv("REFLORA_LIVE_TESTS"), "true")) {
    testthat::skip(
      "Live REFLORA integration test; set REFLORA_LIVE_TESTS=true to run."
    )
  }
}
