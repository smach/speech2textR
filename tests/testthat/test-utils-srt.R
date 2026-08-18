test_that("SRT timestamps are formatted correctly", {
  expect_equal(format_srt_timestamp(0), "00:00:00,000")
  expect_equal(format_srt_timestamp(0.8), "00:00:00,800")
  expect_equal(format_srt_timestamp(61.25), "00:01:01,250")
  expect_equal(format_srt_timestamp(3661.5), "01:01:01,500")
  expect_equal(format_srt_timestamp(NA), "00:00:00,000")
})


test_that("SRT milliseconds never overflow into a fourth digit", {
  # Rounding the fractional part on its own used to give "00:00:01,1000",
  # which is not valid SRT. Every field must stay in range.
  expect_equal(format_srt_timestamp(1.9996), "00:00:02,000")
  expect_equal(format_srt_timestamp(0.9995), "00:00:01,000")
  expect_equal(format_srt_timestamp(59.9999), "00:01:00,000")
  expect_equal(format_srt_timestamp(3599.9999), "01:00:00,000")

  # Sweep a range that would have tripped the old code
  stamps <- vapply(seq(0, 20, by = 0.0007), format_srt_timestamp, character(1))
  expect_true(all(grepl("^[0-9]{2}:[0-9]{2}:[0-9]{2},[0-9]{3}$", stamps)))
})


test_that("readable timestamps drop the hour when there is none", {
  expect_equal(format_time_timestamp(0), "00:00")
  expect_equal(format_time_timestamp(123), "02:03")
  expect_equal(format_time_timestamp(3723), "01:02:03")
  expect_equal(format_time_timestamp(NA), "00:00:00")
})


test_that("speaker labels only add the prefix to bare ids", {
  # AssemblyAI numbers speakers "A", "B" and needs the prefix
  expect_equal(speaker_label("A"), "[Speaker A]")
  expect_equal(speaker_label("0"), "[Speaker 0]")
  expect_equal(speaker_label("12"), "[Speaker 12]")

  # ElevenLabs and Mistral ids already read as labels
  expect_equal(speaker_label("speaker_0"), "[speaker_0]")
  expect_equal(speaker_label("Speaker 2"), "[Speaker 2]")

  # ElevenLabs returns these when detect_speaker_roles is on
  expect_equal(speaker_label("agent"), "[agent]")
  expect_equal(speaker_label("customer"), "[customer]")
})


test_that("subtitle segments split on the character limit", {
  words <- data.frame(
    text = c("one", "two", "three", "four", "five"),
    start = c(0, 1, 2, 3, 4),
    end = c(1, 2, 3, 4, 5),
    speaker_id = NA_character_,
    stringsAsFactors = FALSE
  )

  segments <- create_subtitle_segments(words, max_chars = 10, max_duration = 100)

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

  segments <- create_subtitle_segments(words, max_chars = 1000, max_duration = 7)

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

  segments <- create_subtitle_segments(words, max_chars = 1000, max_duration = 100)

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

  segments <- create_subtitle_segments(words, max_chars = 1000, max_duration = 100)

  expect_equal(segments$text, "kept kept")
})


test_that("no words with timings gives NULL", {
  words <- data.frame(
    text = "nope", start = NA_real_, end = NA_real_,
    speaker_id = NA_character_, stringsAsFactors = FALSE
  )

  expect_null(create_subtitle_segments(words, max_chars = 42, max_duration = 7))
})


test_that("the speaker column name is honoured", {
  # AssemblyAI calls it "speaker"; ElevenLabs and Mistral use "speaker_id".
  # Same input under either name must segment identically.
  base <- data.frame(
    text = c("hi", "there", "hello", "back"),
    start = c(0, 0.5, 1, 1.5),
    end = c(0.5, 1, 1.5, 2),
    stringsAsFactors = FALSE
  )

  id_style <- cbind(base, speaker_id = c("A", "A", "B", "B"))
  aa_style <- cbind(base, speaker = c("A", "A", "B", "B"))

  by_id <- create_subtitle_segments(id_style, 1000, 100, speaker_col = "speaker_id")
  by_aa <- create_subtitle_segments(aa_style, 1000, 100, speaker_col = "speaker")

  expect_equal(names(by_id)[4], "speaker_id")
  expect_equal(names(by_aa)[4], "speaker")
  expect_equal(unname(by_id), unname(by_aa))
})
