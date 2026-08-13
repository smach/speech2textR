test_that("SRT timestamps are formatted correctly", {
  expect_equal(mistral_format_srt_timestamp(0), "00:00:00,000")
  expect_equal(mistral_format_srt_timestamp(0.8), "00:00:00,800")
  expect_equal(mistral_format_srt_timestamp(61.25), "00:01:01,250")
  expect_equal(mistral_format_srt_timestamp(3661.5), "01:01:01,500")
  expect_equal(mistral_format_srt_timestamp(NA), "00:00:00,000")
})


test_that("readable timestamps drop the hour when there is none", {
  expect_equal(mistral_format_time_timestamp(0), "00:00")
  expect_equal(mistral_format_time_timestamp(123), "02:03")
  expect_equal(mistral_format_time_timestamp(3723), "01:02:03")
  expect_equal(mistral_format_time_timestamp(NA), "00:00:00")
})


test_that("speaker labels avoid stuttering on Mistral ids", {
  expect_equal(mistral_speaker_label("speaker_0"), "[speaker_0]")
  expect_equal(mistral_speaker_label("A"), "[Speaker A]")
})


test_that("subtitle segments split on the character limit", {
  words <- data.frame(
    text = c("one", "two", "three", "four", "five"),
    start = c(0, 1, 2, 3, 4),
    end = c(1, 2, 3, 4, 5),
    speaker_id = NA_character_,
    stringsAsFactors = FALSE
  )

  segments <- mistral_create_subtitle_segments(words, max_chars = 10, max_duration = 100)

  expect_gt(nrow(segments), 1)
  expect_true(all(nchar(segments$text) <= 10))
  expect_equal(paste(segments$text, collapse = " "), "one two three four five")
})


test_that("subtitle segments split on the duration limit", {
  words <- data.frame(
    text = c("a", "b", "c", "d"),
    start = c(0, 5, 10, 15),
    end = c(5, 10, 15, 20),
    speaker_id = NA_character_,
    stringsAsFactors = FALSE
  )

  segments <- mistral_create_subtitle_segments(words, max_chars = 1000, max_duration = 7)

  expect_gt(nrow(segments), 1)
  expect_true(all(segments$end - segments$start <= 7))
})


test_that("subtitle segments split when the speaker changes", {
  words <- data.frame(
    text = c("hi", "there", "hello", "back"),
    start = c(0, 0.5, 1, 1.5),
    end = c(0.5, 1, 1.5, 2),
    speaker_id = c("speaker_0", "speaker_0", "speaker_1", "speaker_1"),
    stringsAsFactors = FALSE
  )

  segments <- mistral_create_subtitle_segments(words, max_chars = 1000, max_duration = 100)

  expect_equal(nrow(segments), 2)
  expect_equal(segments$text, c("hi there", "hello back"))
  expect_equal(segments$speaker_id, c("speaker_0", "speaker_1"))
})


test_that("words without timings are skipped", {
  words <- data.frame(
    text = c("kept", "dropped", "kept"),
    start = c(0, NA, 2),
    end = c(1, NA, 3),
    speaker_id = NA_character_,
    stringsAsFactors = FALSE
  )

  segments <- mistral_create_subtitle_segments(words, max_chars = 1000, max_duration = 100)

  expect_equal(segments$text, "kept kept")
})


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
