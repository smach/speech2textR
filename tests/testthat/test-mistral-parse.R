test_that("segment responses parse into a normalized transcript", {
  transcript <- mistral_test_transcript()

  expect_type(transcript, "list")
  expect_named(
    transcript,
    c("text", "segments", "words", "language", "model", "usage")
  )

  expect_match(transcript$text, "^Welcome, everyone")
  expect_equal(transcript$language, "en")
  expect_equal(transcript$model, "voxtral-mini-latest")
  expect_equal(transcript$usage$prompt_audio_seconds, 13)
})


test_that("segments become a data frame with seconds, not milliseconds", {
  segments <- mistral_test_transcript()$segments

  expect_s3_class(segments, "data.frame")
  expect_equal(nrow(segments), 4)
  expect_named(segments, c("text", "start", "end", "speaker_id", "score"))

  expect_type(segments$start, "double")
  expect_type(segments$end, "double")

  # Times come back from the API already in seconds
  expect_equal(segments$start[1], 0.8)
  expect_equal(segments$end[4], 12.5)
})


test_that("speaker ids are preserved", {
  segments <- mistral_test_transcript()$segments

  expect_equal(
    segments$speaker_id,
    c("speaker_0", "speaker_1", "speaker_1", "speaker_0")
  )
})


test_that("a response without segments still parses", {
  response <- mistral_test_response()
  response$segments <- NULL

  transcript <- mistral_format_transcript_response(response)

  expect_null(transcript$segments)
  expect_null(transcript$words)
  expect_match(transcript$text, "Welcome")
})


test_that("word granularity chunks are reported as words", {
  transcript <- mistral_format_transcript_response(
    mistral_test_word_response(),
    timestamps = "word"
  )

  expect_s3_class(transcript$words, "data.frame")
  expect_equal(nrow(transcript$words), 9)
  expect_equal(transcript$words$text[1], "Welcome")

  # Only word timings were asked for, so there are no segments to report
  expect_null(transcript$segments)
})


test_that("leading spaces on word chunks are trimmed", {
  # The API sends " everyone", " to" and so on. Left alone, joining those
  # doubles every space in captions and inflates the line-length limit.
  transcript <- mistral_format_transcript_response(
    mistral_test_word_response(),
    timestamps = "word"
  )

  expect_equal(
    transcript$words$text[1:5],
    c("Welcome", "everyone", "to", "the", "meeting")
  )
  expect_false(any(grepl("^\\s", transcript$words$text)))
})


test_that("requesting both granularities keeps the chunks as segments", {
  transcript <- mistral_format_transcript_response(
    mistral_test_word_response(),
    timestamps = c("segment", "word")
  )

  expect_s3_class(transcript$segments, "data.frame")
  expect_s3_class(transcript$words, "data.frame")
})


test_that("missing optional fields become NA rather than erroring", {
  response <- list(
    text = "Hello there.",
    segments = list(list(text = "Hello there.", start = 0, end = 1))
  )

  transcript <- mistral_format_transcript_response(response, timestamps = "segment")

  expect_true(is.na(transcript$language))
  expect_true(is.na(transcript$model))
  expect_true(is.na(transcript$segments$speaker_id))
  expect_true(is.na(transcript$segments$score))
})


test_that("an unrecognized response shape is rejected", {
  expect_error(
    mistral_format_transcript_response(list(oops = TRUE)),
    "Unexpected transcript response format"
  )
})
