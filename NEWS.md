# speech2textR 0.2.0

## New features

* Added support for the Mistral AI transcription API, which runs the Voxtral
  Mini Transcribe 2 model: `mistral_auth()`, `mistral_is_authenticated()`,
  `mistral_transcribe()`, `mistral_transcript_to_srt()`, and
  `mistral_transcript_to_txt()`. Voxtral handles recordings up to about three
  hours in a single request, so long files no longer need to be split, and at
  $0.003 per minute it is the least expensive of the three services. It also
  supports speaker diarization, segment- and word-level timestamps, and
  context biasing to help the model spell names and jargon correctly.

## Other changes

* Added a test suite using testthat.
* Added the LICENSE file that DESCRIPTION referred to but that was missing.
* Dropped the unused shiny dependency and moved jsonlite from Imports to
  Suggests, where it is now used only by the tests.

# speech2textR 0.1.0

* First release, with wrappers for the ElevenLabs and AssemblyAI
  speech-to-text APIs and a standalone app for editing .srt caption files
  alongside a video.
