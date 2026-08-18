test_that("auth rejects a missing or unusable key", {
  saved <- .elevenlabsr_env$api_key
  on.exit(.elevenlabsr_env$api_key <- saved, add = TRUE)

  expect_error(elevenlabs_auth(""), "No API key provided")
  expect_error(elevenlabs_auth(NA_character_), "No API key provided")
  expect_error(elevenlabs_auth(character(0)), "No API key provided")
  expect_error(elevenlabs_auth(c("a", "b")), "No API key provided")
  expect_error(elevenlabs_auth(123), "No API key provided")
})


test_that("auth stores a usable key", {
  saved <- .elevenlabsr_env$api_key
  on.exit(.elevenlabsr_env$api_key <- saved, add = TRUE)

  expect_message(elevenlabs_auth("test-key-123"), "stored successfully")
  expect_true(elevenlabs_is_authenticated())
  expect_equal(elevenlabs_get_api_key(), "test-key-123")
})


test_that("an unset key is reported rather than returned empty", {
  saved <- .elevenlabsr_env$api_key
  on.exit(.elevenlabsr_env$api_key <- saved, add = TRUE)

  .elevenlabsr_env$api_key <- NULL
  expect_false(elevenlabs_is_authenticated())
  expect_error(elevenlabs_get_api_key(), "Please authenticate first")
})


test_that("SRT uses word-level timings and labels speakers", {
  transcript <- list(
    text = "Hello there general",
    words = data.frame(
      text = c("Hello", "there", "general"),
      start = c(0, 0.5, 1.0),
      end = c(0.5, 1.0, 1.9996),
      type = "word",
      speaker_id = c("speaker_0", "speaker_0", "speaker_1"),
      stringsAsFactors = FALSE
    )
  )

  lines <- strsplit(elevenlabs_transcript_to_srt(transcript), "\n", fixed = TRUE)[[1]]

  expect_equal(lines[1], "1")
  expect_equal(lines[2], "00:00:00,000 --> 00:00:01,000")
  expect_equal(lines[3], "[speaker_0] Hello there")
  # The old formatter turned 1.9996 into "00:00:01,1000"
  expect_equal(lines[6], "00:00:01,000 --> 00:00:02,000")
  expect_equal(lines[7], "[speaker_1] general")
})


test_that("SRT needs word-level timing data", {
  expect_error(
    elevenlabs_transcript_to_srt(list(text = "no words here")),
    "does not contain word-level timing information"
  )
})
