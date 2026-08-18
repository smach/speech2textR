# speech2textR 0.2.1

## Bug fixes

* Fixed SRT timestamps that could come out with a four-digit millisecond
  field, like `00:00:01,1000`, which is not valid SRT. Any timing whose
  fraction rounded up to a full second was affected. This hit all three
  services, because the formatting code had been copied into each one.

* `assemblyai_transcribe()` no longer waits forever on a job that never
  finishes. It now gives up after an hour by default and reports the
  transcript ID so a slow job can still be collected later. Both the wait
  and the interval between checks are adjustable with the new `timeout` and
  `poll_interval` arguments.

* `assemblyai_auth(validate = TRUE)` now stops on a bad API key instead of
  only warning about it. The check was unreachable, because httr2 turns an
  HTTP 401 into an R error before the status code is ever examined.

* `elevenlabs_auth()` and `assemblyai_auth()` now give the same "here is how
  to set your key" message that `mistral_auth()` does when the key is
  missing, empty, or not a single string.

## Other changes

* Moved the subtitle and timestamp helpers that had been copied into all
  three service files into one shared set, so a fix like the timestamp one
  above only has to be made once.

* `assemblyai_transcribe()` now streams the audio file from disk when
  uploading instead of reading the whole recording into memory first.

* Added tests for the AssemblyAI authentication and polling code, which had
  none.

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
