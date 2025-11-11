#' Edit Transcript Text File with Audio Sync
#'
#' Launch a standalone transcript editor with audio playback and text synchronization.
#' This allows you to edit plain text transcripts while listening to the audio,
#' with bidirectional sync between audio position and text location.
#'
#' Requires Node.js to be installed on your system.
#'
#' @param audio_file Character string. Path to the audio file (e.g., .mp3, .wav, .m4a, .webm)
#' @param transcript Character. Either a transcript object (from elevenlabs_transcribe()
#'   or assemblyai_transcribe()) OR a path to a .txt file. If providing a .txt file,
#'   you must also provide the transcript object in transcript_data parameter.
#' @param transcript_data Optional. The transcript object containing word-level timestamps.
#'   Required if transcript parameter is a file path instead of a transcript object.
#'
#' @return Launches the Node.js server. Press Ctrl+C in the console to stop.
#'
#' @details
#' This editor displays your transcript as clean, readable text (not SRT format) while
#' maintaining synchronization with audio playback. Features include:
#' \itemize{
#'   \item Click anywhere in the audio player to jump to that position - text will sync
#'   \item Click on any word in the text to jump audio to that position
#'   \item Edit the text inline and save changes
#'   \item Current position is highlighted as audio plays
#' }
#'
#' The editor uses word-level timestamps from your transcription API to enable sync.
#'
#' @examples
#' \dontrun{
#' # Transcribe audio and edit the transcript
#' transcript <- elevenlabs_transcribe("my_audio.mp3")
#' edit_transcript_standalone("my_audio.mp3", transcript)
#'
#' # Or with AssemblyAI
#' transcript <- assemblyai_transcribe("interview.mp3")
#' edit_transcript_standalone("interview.mp3", transcript)
#'
#' # Edit an existing .txt file (requires original transcript object)
#' edit_transcript_standalone("my_audio.mp3", "transcript.txt", transcript)
#' }
#'
#' @export
edit_transcript_standalone <- function(audio_file, transcript, transcript_data = NULL) {

  # Check if files exist
  if (!file.exists(audio_file)) {
    stop("Audio file not found: ", audio_file)
  }

  # Handle different transcript input types
  if (is.character(transcript) && length(transcript) == 1 && file.exists(transcript)) {
    # transcript is a file path
    txt_file <- transcript
    if (is.null(transcript_data)) {
      stop("When providing a .txt file path, you must also provide the transcript_data parameter with the original transcript object containing word timestamps.")
    }
    transcript_obj <- transcript_data
  } else if (is.list(transcript)) {
    # transcript is a transcript object
    transcript_obj <- transcript
    txt_file <- NULL
  } else {
    stop("transcript parameter must be either a transcript object or a path to a .txt file")
  }

  # Validate transcript object has required data
  if (is.null(transcript_obj$words) || nrow(transcript_obj$words) == 0) {
    stop("Transcript object does not contain word-level timing information.\n",
         "Make sure you transcribed with elevenlabs_transcribe() or assemblyai_transcribe().")
  }

  # Check if Node.js is available
  node_check <- system("node --version", ignore.stdout = TRUE, ignore.stderr = TRUE)
  if (node_check != 0) {
    stop("Node.js is required for the transcript editor.\n",
         "Please install Node.js from https://nodejs.org/ and try again.")
  }

  # Get absolute paths
  audio_file <- normalizePath(audio_file)

  # Create a temporary JSON file with transcript data
  temp_json <- tempfile(fileext = ".json")

  # Prepare the data structure for the editor
  words_data <- transcript_obj$words[transcript_obj$words$type == "word", ]

  editor_data <- list(
    text = if (!is.null(txt_file)) {
      paste(readLines(txt_file, warn = FALSE), collapse = "\n")
    } else if (!is.null(transcript_obj$text)) {
      transcript_obj$text
    } else {
      paste(words_data$text, collapse = " ")
    },
    words = lapply(1:nrow(words_data), function(i) {
      list(
        text = words_data$text[i],
        start = words_data$start[i],
        end = words_data$end[i]
      )
    })
  )

  # Write JSON file
  jsonlite::write_json(editor_data, temp_json, auto_unbox = TRUE, pretty = TRUE)

  # If txt_file was provided, use it as the save target; otherwise create one
  if (is.null(txt_file)) {
    txt_file <- tempfile(fileext = ".txt")
    writeLines(editor_data$text, txt_file)
  }
  txt_file <- normalizePath(txt_file)

  # Find the transcript-editor directory in the installed package
  editor_dir <- system.file("transcript-editor", package = "speech2textR")

  if (editor_dir == "" || !dir.exists(editor_dir)) {
    stop("Transcript editor not found in package installation.\n",
         "Please reinstall the package or check the transcript-editor/ directory.")
  }

  server_path <- file.path(editor_dir, "server.js")

  if (!file.exists(server_path)) {
    stop("server.js not found at: ", server_path)
  }

  # Build the command
  cmd <- sprintf('node "%s" "%s" "%s" "%s"',
                 server_path, audio_file, temp_json, txt_file)

  message("\n=====================================================")
  message("Starting Transcript Editor with Audio Sync")
  message("=====================================================")
  message("Audio: ", basename(audio_file))
  message("Transcript: ", if(!is.null(transcript_obj$text)) {
    paste0(substr(transcript_obj$text, 1, 50), "...")
  } else {
    "Loaded"
  })
  message("\nThe editor will open in your browser automatically.")
  message("Click in audio to sync text, click text to sync audio.")
  message("Press Ctrl+C in this console to stop the server.")
  message("=====================================================\n")

  # Run the server (blocking)
  system(cmd)

  # Clean up temp JSON file
  if (file.exists(temp_json)) {
    unlink(temp_json)
  }

  return(invisible(NULL))
}
