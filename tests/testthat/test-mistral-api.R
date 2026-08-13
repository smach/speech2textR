# Live round-trip against the Mistral API. Skipped unless a key is available,
# since it costs money and needs a network connection.

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
