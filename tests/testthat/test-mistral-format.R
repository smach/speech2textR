test_that("SRT output is well formed", {
  srt <- mistral_transcript_to_srt(mistral_test_transcript())
  lines <- strsplit(srt, "\n", fixed = TRUE)[[1]]

  expect_equal(lines[1], "1")
  expect_equal(lines[2], "00:00:00,800 --> 00:00:04,440")
  expect_equal(lines[3], "[speaker_0] Welcome, everyone, to this second E round.")
  expect_equal(lines[4], "")

  # Four segments in, four subtitles out
  expect_equal(sum(grepl("-->", lines, fixed = TRUE)), 4)
})


test_that("SRT speaker labels can be turned off", {
  srt <- mistral_transcript_to_srt(mistral_test_transcript(), include_speakers = FALSE)

  expect_false(grepl("speaker_0", srt, fixed = TRUE))
  expect_true(grepl("Welcome, everyone", srt, fixed = TRUE))
})


test_that("SRT regroups words when word timings are available", {
  transcript <- mistral_format_transcript_response(
    mistral_test_word_response(),
    timestamps = "word"
  )

  srt <- mistral_transcript_to_srt(transcript, max_chars_per_line = 20)
  lines <- strsplit(srt, "\n", fixed = TRUE)[[1]]
  text_lines <- lines[grepl("^\\[speaker", lines)]

  expect_gt(length(text_lines), 2)
  expect_true(all(nchar(gsub("^\\[speaker_[01]\\] ", "", text_lines)) <= 20))
})


test_that("word-level captions do not pick up doubled spaces", {
  transcript <- mistral_format_transcript_response(
    mistral_test_word_response(),
    timestamps = "word"
  )

  segments <- create_subtitle_segments(
    transcript$words,
    max_chars = 42,
    max_duration = 7
  )

  expect_false(any(grepl("  ", segments$text, fixed = TRUE)))
  expect_equal(segments$text[1], "Welcome everyone to the meeting")

  srt <- mistral_transcript_to_srt(transcript)
  expect_false(grepl("  ", srt, fixed = TRUE))
})


test_that("word-level plain text does not pick up stray spaces", {
  transcript <- mistral_format_transcript_response(
    mistral_test_word_response(),
    timestamps = "word"
  )

  txt <- mistral_transcript_to_txt(transcript, include_timestamps = TRUE)

  expect_false(grepl("  ", txt, fixed = TRUE))
  expect_equal(
    strsplit(txt, "\n\n", fixed = TRUE)[[1]][1],
    "[speaker_0] (00:00) Welcome everyone to the meeting"
  )
})


test_that("SRT needs timing information", {
  transcript <- mistral_format_transcript_response(
    list(text = "No timings here."),
    timestamps = NULL
  )

  expect_error(
    mistral_transcript_to_srt(transcript),
    "does not contain timing information"
  )
})


test_that("SRT can be written to a file", {
  path <- tempfile(fileext = ".srt")
  on.exit(unlink(path), add = TRUE)

  expect_message(
    mistral_transcript_to_srt(mistral_test_transcript(), path),
    "SRT file saved to"
  )
  expect_true(file.exists(path))
  expect_match(paste(readLines(path), collapse = "\n"), "-->")
})


test_that("plain text output uses the text field when nothing extra is asked for", {
  transcript <- mistral_test_transcript()
  txt <- mistral_transcript_to_txt(transcript, include_speakers = FALSE)

  expect_equal(txt, transcript$text)
})


test_that("plain text output groups consecutive segments by speaker", {
  txt <- mistral_transcript_to_txt(mistral_test_transcript())
  blocks <- strsplit(txt, "\n\n", fixed = TRUE)[[1]]

  # speaker_0, then two speaker_1 segments joined, then speaker_0 again
  expect_equal(length(blocks), 3)
  expect_equal(
    blocks[2],
    "[speaker_1] Thanks for having me. I am glad to be here today."
  )
})


test_that("plain text output can include timestamps", {
  txt <- mistral_transcript_to_txt(
    mistral_test_transcript(),
    include_timestamps = TRUE
  )

  expect_match(txt, "[speaker_0] (00:00)", fixed = TRUE)

  seconds <- mistral_transcript_to_txt(
    mistral_test_transcript(),
    include_timestamps = TRUE,
    timestamp_format = "seconds"
  )

  expect_match(seconds, "(0.8s)", fixed = TRUE)
})


test_that("plain text output timestamps every segment when speakers are off", {
  txt <- mistral_transcript_to_txt(
    mistral_test_transcript(),
    include_timestamps = TRUE,
    include_speakers = FALSE
  )
  blocks <- strsplit(txt, "\n\n", fixed = TRUE)[[1]]

  expect_equal(length(blocks), 4)
  expect_equal(blocks[1], "(00:00) Welcome, everyone, to this second E round.")
})


test_that("plain text falls back to the text field without timing data", {
  transcript <- mistral_format_transcript_response(list(text = "Just words."))

  expect_warning(
    txt <- mistral_transcript_to_txt(transcript, include_timestamps = TRUE),
    "No timing or speaker data"
  )
  expect_equal(txt, "Just words.")
})


test_that("plain text can be written to a file", {
  path <- tempfile(fileext = ".txt")
  on.exit(unlink(path), add = TRUE)

  expect_message(
    mistral_transcript_to_txt(mistral_test_transcript(), path),
    "Text file saved to"
  )
  expect_true(file.exists(path))
})
