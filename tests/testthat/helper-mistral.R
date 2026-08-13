# Shared fixtures for the Mistral tests

mistral_test_response <- function() {
  jsonlite::fromJSON(
    testthat::test_path("fixtures", "mistral_transcription_response.json"),
    simplifyVector = FALSE
  )
}

# A parsed transcript with segment-level timings and two speakers
mistral_test_transcript <- function() {
  mistral_format_transcript_response(
    mistral_test_response(),
    timestamps = "segment"
  )
}

# A minimal word-level response, as returned when only "word" granularity
# is requested and the API reports one chunk per word
mistral_test_word_response <- function() {
  words <- list(
    list(text = "Welcome", start = 0.8, end = 1.2, speaker_id = "speaker_0"),
    list(text = "everyone", start = 1.2, end = 1.9, speaker_id = "speaker_0"),
    list(text = "to", start = 1.9, end = 2.1, speaker_id = "speaker_0"),
    list(text = "the", start = 2.1, end = 2.3, speaker_id = "speaker_0"),
    list(text = "meeting", start = 2.3, end = 3.0, speaker_id = "speaker_0"),
    list(text = "Thanks", start = 3.4, end = 3.9, speaker_id = "speaker_1"),
    list(text = "for", start = 3.9, end = 4.1, speaker_id = "speaker_1"),
    list(text = "having", start = 4.1, end = 4.5, speaker_id = "speaker_1"),
    list(text = "me", start = 4.5, end = 4.8, speaker_id = "speaker_1")
  )

  list(
    model = "voxtral-mini-latest",
    text = "Welcome everyone to the meeting Thanks for having me",
    language = "en",
    segments = words,
    usage = list(prompt_audio_seconds = 5)
  )
}
