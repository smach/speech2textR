# Live round-trip against the Mistral API. Skipped unless a key is available,
# since it costs money and needs a network connection. The bundled clip runs
# about 94 seconds, so at $0.003 per minute each transcription here costs
# roughly half a cent.

test_that("a real file can be transcribed end to end", {
  skip_on_cran()
  skip_if(Sys.getenv("MISTRAL_API_KEY") == "", "MISTRAL_API_KEY not set")

  video <- system.file("extdata", "sample_video_snippet.mp4", package = "speech2textR")
  skip_if(video == "", "sample video not installed")

  mistral_auth()

  transcript <- mistral_transcribe(
    video,
    diarize = TRUE,
    timestamps = "segment",
    verbose = FALSE
  )

  expect_type(transcript$text, "character")
  expect_gt(nchar(transcript$text), 0)

  expect_s3_class(transcript$segments, "data.frame")
  expect_gt(nrow(transcript$segments), 0)
  expect_true(all(transcript$segments$end >= transcript$segments$start))

  srt <- mistral_transcript_to_srt(transcript)
  expect_match(srt, "-->", fixed = TRUE)
})


test_that("word timestamps come back as clean words", {
  skip_on_cran()
  skip_if(Sys.getenv("MISTRAL_API_KEY") == "", "MISTRAL_API_KEY not set")

  video <- system.file("extdata", "sample_video_snippet.mp4", package = "speech2textR")
  skip_if(video == "", "sample video not installed")

  mistral_auth()

  transcript <- mistral_transcribe(video, timestamps = "word", verbose = FALSE)

  # The API returns one chunk per word, so those are reported as words and
  # there are no segments to report
  expect_s3_class(transcript$words, "data.frame")
  expect_gt(nrow(transcript$words), 0)
  expect_null(transcript$segments)

  # The API pads words with a leading space; captions must not inherit it
  expect_false(any(grepl("^\\s", transcript$words$text)))
  expect_false(grepl("  ", mistral_transcript_to_srt(transcript), fixed = TRUE))
})


test_that("a bad key produces a readable error", {
  skip_on_cran()
  skip_if(Sys.getenv("MISTRAL_API_KEY") == "", "MISTRAL_API_KEY not set")

  key <- .mistral_env$api_key
  on.exit(.mistral_env$api_key <- key, add = TRUE)

  video <- system.file("extdata", "sample_video_snippet.mp4", package = "speech2textR")
  skip_if(video == "", "sample video not installed")

  mistral_auth("not_a_real_key")

  expect_error(mistral_transcribe(video, verbose = FALSE))
})
