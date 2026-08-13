# These tests all fail validation before any network request is attempted

test_that("a file or a file_url is required", {
  expect_error(mistral_transcribe(), "either a local 'file' or a 'file_url'")
})


test_that("a file and a file_url cannot both be supplied", {
  expect_error(
    mistral_transcribe(file = "a.mp3", file_url = "https://example.com/a.mp3"),
    "only one of 'file' or 'file_url'"
  )
})


test_that("a missing file is reported clearly", {
  expect_error(
    mistral_transcribe("no_such_file_here.mp3"),
    "File not found: no_such_file_here.mp3"
  )
})


test_that("timestamp granularities are checked", {
  expect_error(
    mistral_check_timestamps("sentence"),
    "Invalid 'timestamps' value"
  )

  expect_equal(mistral_check_timestamps("word"), "word")
  expect_equal(
    mistral_check_timestamps(c("segment", "word", "segment")),
    c("segment", "word")
  )
  expect_null(mistral_check_timestamps(NULL))
  expect_null(mistral_check_timestamps(character(0)))
})


test_that("context bias terms cannot contain spaces or commas", {
  expect_error(
    mistral_check_context_bias(c("Quarto", "affordable health care")),
    "cannot contain spaces or commas"
  )

  expect_error(
    mistral_check_context_bias("one,two"),
    "cannot contain spaces or commas"
  )

  expect_equal(
    mistral_check_context_bias(c("Quarto", "affordable_health_care")),
    c("Quarto", "affordable_health_care")
  )
})


test_that("context bias is capped at 100 terms", {
  expect_error(
    mistral_check_context_bias(paste0("term", 1:101)),
    "at most 100 terms"
  )

  expect_length(mistral_check_context_bias(paste0("term", 1:100)), 100)
})


test_that("context bias must be character", {
  expect_error(mistral_check_context_bias(1:3), "must be a character vector")
})


test_that("array parameters build repeated form fields", {
  body <- list(model = "voxtral-mini-latest")
  body <- c(body, mistral_form_field("timestamp_granularities", "segment"))
  body <- c(body, mistral_form_field("timestamp_granularities", "word"))

  expect_equal(
    names(body),
    c("model", "timestamp_granularities", "timestamp_granularities")
  )
  expect_equal(unlist(body, use.names = FALSE), c("voxtral-mini-latest", "segment", "word"))
})


test_that("transcribing without a key asks the user to authenticate", {
  key <- .mistral_env$api_key
  .mistral_env$api_key <- NULL
  on.exit(.mistral_env$api_key <- key, add = TRUE)

  audio <- tempfile(fileext = ".mp3")
  writeBin(as.raw(0:9), audio)
  on.exit(unlink(audio), add = TRUE)

  expect_error(mistral_transcribe(audio), "authenticate first using mistral_auth")
})


test_that("language and timestamps together drop the timestamps", {
  key <- .mistral_env$api_key
  .mistral_env$api_key <- NULL
  on.exit(.mistral_env$api_key <- key, add = TRUE)

  audio <- tempfile(fileext = ".mp3")
  writeBin(as.raw(0:9), audio)
  on.exit(unlink(audio), add = TRUE)

  # The warning fires during validation, before the missing key stops the call
  expect_warning(
    try(mistral_transcribe(audio, language = "en", timestamps = "segment"), silent = TRUE),
    "does not accept 'language' and timestamps"
  )
})


test_that("diarize without timestamps warns once timestamps are dropped", {
  key <- .mistral_env$api_key
  .mistral_env$api_key <- NULL
  on.exit(.mistral_env$api_key <- key, add = TRUE)

  audio <- tempfile(fileext = ".mp3")
  writeBin(as.raw(0:9), audio)
  on.exit(unlink(audio), add = TRUE)

  # Setting a language drops the auto-added segment timestamps, which in turn
  # makes diarize pointless, so both warnings fire
  warnings <- character()
  withCallingHandlers(
    try(
      mistral_transcribe(audio, language = "en", diarize = TRUE, timestamps = NULL),
      silent = TRUE
    ),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  expect_true(any(grepl("'diarize' has no effect without timestamps", warnings)))
  expect_true(any(grepl("does not accept 'language' and timestamps", warnings)))
})


test_that("authentication helpers round-trip a key", {
  key <- .mistral_env$api_key
  on.exit(.mistral_env$api_key <- key, add = TRUE)

  .mistral_env$api_key <- NULL
  expect_false(mistral_is_authenticated())

  expect_message(mistral_auth("test_key_not_real"), "stored successfully")
  expect_true(mistral_is_authenticated())
  expect_equal(mistral_get_api_key(), "test_key_not_real")
})


test_that("authentication rejects an empty key", {
  key <- .mistral_env$api_key
  env_key <- Sys.getenv("MISTRAL_API_KEY")
  Sys.setenv(MISTRAL_API_KEY = "")
  on.exit({
    .mistral_env$api_key <- key
    Sys.setenv(MISTRAL_API_KEY = env_key)
  }, add = TRUE)

  expect_error(mistral_auth(""), "No API key provided")
  expect_error(mistral_auth(NULL), "No API key provided")
  expect_error(mistral_auth(), "No API key provided")
})
