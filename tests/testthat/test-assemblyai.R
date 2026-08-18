test_that("auth rejects a missing or unusable key", {
  saved <- .assemblyai_env$api_key
  on.exit(.assemblyai_env$api_key <- saved, add = TRUE)

  expect_error(assemblyai_auth(""), "No API key provided")
  expect_error(assemblyai_auth(NA_character_), "No API key provided")
  expect_error(assemblyai_auth(character(0)), "No API key provided")
  expect_error(assemblyai_auth(c("a", "b")), "No API key provided")
  expect_error(assemblyai_auth(123), "No API key provided")
})


test_that("auth stores a usable key", {
  saved <- .assemblyai_env$api_key
  on.exit(.assemblyai_env$api_key <- saved, add = TRUE)

  expect_message(assemblyai_auth("test-key-123"), "stored successfully")
  expect_true(assemblyai_is_authenticated())
  expect_equal(assemblyai_get_api_key(), "test-key-123")
})


test_that("an unset key is reported rather than returned empty", {
  saved <- .assemblyai_env$api_key
  on.exit(.assemblyai_env$api_key <- saved, add = TRUE)

  .assemblyai_env$api_key <- NULL
  expect_false(assemblyai_is_authenticated())
  expect_error(assemblyai_get_api_key(), "Please authenticate first")
})


test_that("polling gives up instead of spinning forever", {
  # A job that never leaves "processing" used to block the session with no
  # way out but Ctrl-C.
  still_going <- function(req) {
    httr2::response_json(status_code = 200, body = list(status = "processing"))
  }

  httr2::with_mocked_responses(still_going, {
    expect_error(
      assemblyai_get_transcript(
        "fake-id-abc", "test-key",
        verbose = FALSE, poll_interval = 0.05, timeout = 0.3
      ),
      "did not finish within"
    )
  })
})


test_that("the give-up message names the id and the last status", {
  queued <- function(req) {
    httr2::response_json(status_code = 200, body = list(status = "queued"))
  }

  err <- httr2::with_mocked_responses(queued, {
    tryCatch(
      assemblyai_get_transcript(
        "fake-id-abc", "test-key",
        verbose = FALSE, poll_interval = 0.05, timeout = 0.2
      ),
      error = function(e) conditionMessage(e)
    )
  })

  expect_match(err, "fake-id-abc", fixed = TRUE)
  expect_match(err, "queued", fixed = TRUE)
})


test_that("polling returns as soon as the job completes", {
  done <- function(req) {
    httr2::response_json(
      status_code = 200,
      body = list(status = "completed", text = "all done", id = "fake-id-abc")
    )
  }

  result <- httr2::with_mocked_responses(done, {
    assemblyai_get_transcript(
      "fake-id-abc", "test-key",
      verbose = FALSE, poll_interval = 0.05, timeout = 30
    )
  })

  expect_equal(result$status, "completed")
  expect_equal(result$text, "all done")
})


test_that("a failed job errors with the reason the API gave", {
  failed <- function(req) {
    httr2::response_json(
      status_code = 200,
      body = list(status = "error", error = "audio was unreadable")
    )
  }

  httr2::with_mocked_responses(failed, {
    expect_error(
      assemblyai_get_transcript(
        "fake-id-abc", "test-key",
        verbose = FALSE, poll_interval = 0.05, timeout = 30
      ),
      "audio was unreadable"
    )
  })
})
