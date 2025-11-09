#' Authenticate to the AssemblyAI API
#'
#' This function sets up authentication for the AssemblyAI API by storing
#' your API key for use in subsequent API calls. The API key is stored
#' in a package-level environment for the duration of your R session.
#'
#' @param api_key Character string containing your AssemblyAI API key.
#'   If not provided, the function will look for the ASSEMBLYAI_API_KEY
#'   environment variable.
#' @param validate Logical. If TRUE, validates the API key by making a test
#'   request to the AssemblyAI API. Default is FALSE.
#'
#' @return Invisibly returns TRUE if authentication is successful. Prints
#'   a confirmation message to the console.
#'
#' @details
#' You can obtain an API key from your AssemblyAI account dashboard.
#' It's recommended to store your API key in an environment variable
#' (ASSEMBLYAI_API_KEY) rather than hardcoding it in your scripts.
#'
#' The API key is stored in memory for the current R session and will
#' need to be set again if you restart R.
#'
#' @examples
#' \dontrun{
#' # Authenticate with API key directly
#' assemblyai_auth(api_key = "your_api_key_here")
#'
#' # Or set environment variable first
#' Sys.setenv(ASSEMBLYAI_API_KEY = "your_api_key_here")
#' assemblyai_auth()
#'
#' # Validate the API key
#' assemblyai_auth(api_key = "your_api_key_here", validate = TRUE)
#' }
#'
#' @export
assemblyai_auth <- function(api_key = Sys.getenv("ASSEMBLYAI_API_KEY"), validate = FALSE) {
  # Try to get API key from parameter or environment variable
  if (is.null(api_key)) {
    api_key <- Sys.getenv("ASSEMBLYAI_API_KEY")
    if (api_key == "") {
      stop(
        "No API key provided. Please either:\n",
        "  1. Pass api_key parameter: assemblyai_auth(api_key = 'your_key')\n",
        "  2. Set ASSEMBLYAI_API_KEY environment variable: ",
        "Sys.setenv(ASSEMBLYAI_API_KEY = 'your_key')",
        call. = FALSE
      )
    }
  }

  # Validate API key format (basic check)
  if (!is.character(api_key) || nchar(api_key) == 0) {
    stop("API key must be a non-empty character string.", call. = FALSE)
  }

  # Store API key in package environment
  .assemblyai_env$api_key <- api_key

  # Optionally validate the API key by making a test request
  if (validate) {
    tryCatch({
      # Make a simple request to validate credentials
      response <- httr2::request("https://api.assemblyai.com/v2/transcript") |>
        httr2::req_headers(authorization = api_key) |>
        httr2::req_method("GET") |>
        httr2::req_perform()

      if (httr2::resp_status(response) == 401) {
        stop("Invalid API key. Please check your credentials.", call. = FALSE)
      } else if (httr2::resp_status(response) >= 400) {
        warning(
          "Could not validate API key (HTTP ",
          httr2::resp_status(response),
          "). The key has been stored but may be invalid."
        )
      } else {
        message("API key validated successfully!")
      }
    }, error = function(e) {
      warning(
        "Could not validate API key: ", e$message,
        "\nThe key has been stored but validation failed."
      )
    })
  } else {
    message("AssemblyAI API key stored successfully!")
  }

  invisible(TRUE)
}


#' Get stored AssemblyAI API key
#'
#' Internal function to retrieve the stored API key. This is used by
#' other functions in the package to access the authenticated credentials.
#'
#' @return Character string containing the API key.
#' @keywords internal
assemblyai_get_api_key <- function() {
  api_key <- .assemblyai_env$api_key

  if (is.null(api_key)) {
    stop(
      "No API key found. Please authenticate first using assemblyai_auth().",
      call. = FALSE
    )
  }

  return(api_key)
}


#' Check if authenticated to AssemblyAI API
#'
#' Check whether an API key has been set for the current session.
#'
#' @return Logical value indicating whether an API key is stored.
#'
#' @examples
#' \dontrun{
#' assemblyai_is_authenticated()
#' }
#'
#' @export
assemblyai_is_authenticated <- function() {
  !is.null(.assemblyai_env$api_key)
}


#' Upload audio file to AssemblyAI
#'
#' Internal function to upload an audio file to AssemblyAI's servers.
#'
#' @param file_path Character string. Path to the audio file.
#' @param api_key Character string. AssemblyAI API key.
#' @param verbose Logical. If TRUE, prints progress messages.
#'
#' @return Character string containing the upload URL.
#' @keywords internal
assemblyai_upload_file <- function(file_path, api_key = Sys.getenv("ASSEMBLYAI_API_KEY"), verbose = TRUE) {
  if (verbose) {
    message("Uploading file...")
  }

  # Read the audio file as binary
  audio_data <- readBin(file_path, "raw", file.info(file_path)$size)

  # Upload to AssemblyAI
  response <- httr2::request("https://api.assemblyai.com/v2/upload") |>
    httr2::req_headers(authorization = api_key) |>
    httr2::req_body_raw(audio_data) |>
    httr2::req_perform()

  # Extract the upload URL from response
  result <- httr2::resp_body_json(response)
  return(result$upload_url)
}


#' Submit transcription request to AssemblyAI
#'
#' Internal function to submit a transcription request.
#'
#' @param audio_url Character string. URL of the uploaded audio file.
#' @param api_key Character string. AssemblyAI API key.
#' @param speaker_labels Logical. Enable speaker diarization.
#' @param verbose Logical. If TRUE, prints progress messages.
#'
#' @return Character string containing the transcript ID.
#' @keywords internal
assemblyai_submit_transcription <- function(audio_url, api_key, speaker_labels = TRUE, verbose = TRUE) {
  if (verbose) {
    message("Submitting transcription request...")
  }

  # Create request body with configuration
  body <- list(
    audio_url = audio_url,
    speaker_labels = speaker_labels
  )

  # Submit transcription request
  response <- httr2::request("https://api.assemblyai.com/v2/transcript") |>
    httr2::req_headers(authorization = api_key) |>
    httr2::req_body_json(body) |>
    httr2::req_perform()

  # Extract the transcript ID
  result <- httr2::resp_body_json(response)
  return(result$id)
}


#' Poll for transcription completion
#'
#' Internal function to poll the AssemblyAI API until transcription is complete.
#'
#' @param transcript_id Character string. The transcript ID.
#' @param api_key Character string. AssemblyAI API key.
#' @param verbose Logical. If TRUE, prints progress messages.
#'
#' @return List containing the completed transcript data.
#' @keywords internal
assemblyai_get_transcript <- function(transcript_id, api_key, verbose = TRUE) {
  if (verbose) {
    message("Waiting for transcription to complete...")
  }

  # Poll until transcription is complete
  while (TRUE) {
    response <- httr2::request(paste0("https://api.assemblyai.com/v2/transcript/", transcript_id)) |>
      httr2::req_headers(authorization = api_key) |>
      httr2::req_perform()

    result <- httr2::resp_body_json(response)

    # Check status
    if (result$status == "completed") {
      if (verbose) {
        message("Transcription completed!")
      }
      return(result)
    } else if (result$status == "error") {
      stop(paste("Transcription error:", result$error), call. = FALSE)
    }

    # Wait before polling again
    Sys.sleep(3)
  }
}


#' Transcribe audio or video file using AssemblyAI API
#'
#' Upload an audio or video file to AssemblyAI for speech-to-text transcription.
#'
#' @param file Character string. Path to the audio or video file to transcribe.
#' @param speaker_labels Logical. If TRUE, identifies different speakers in the audio.
#'   Default is FALSE.
#' @param verbose Logical. If TRUE, prints progress messages. Default is TRUE.
#'
#' @return A list containing the transcription results:
#'   \item{text}{Full transcript as plain text}
#'   \item{utterances}{List of utterances with speaker information (if speaker_labels = TRUE)}
#'   \item{words}{List of words with timing information}
#'   \item{id}{Transcript ID}
#'   \item{status}{Transcription status}
#'
#' @details
#' This function uploads an audio file to AssemblyAI, submits it for transcription,
#' and polls the API until the transcription is complete.
#'
#' You must authenticate first using \code{assemblyai_auth()}.
#'
#' @examples
#' \dontrun{
#' # Simple transcription
#' transcript <- assemblyai_transcribe("interview.mp3")
#'
#' # With speaker diarization
#' transcript <- assemblyai_transcribe(
#'   file = "meeting.mp4",
#'   speaker_labels = TRUE
#' )
#'
#' # Access the text
#' cat(transcript$text)
#'
#' # Access speaker utterances
#' if (!is.null(transcript$utterances)) {
#'   for (utterance in transcript$utterances) {
#'     cat(sprintf("Speaker %s: %s\n", utterance$speaker, utterance$text))
#'   }
#' }
#' }
#'
#' @export
assemblyai_transcribe <- function(file, speaker_labels = FALSE, verbose = TRUE) {
  # Validate inputs
  if (!file.exists(file)) {
    stop("File not found: ", file, call. = FALSE)
  }

  # Get API key
  api_key <- assemblyai_get_api_key()

  # Step 1: Upload audio file
  upload_url <- assemblyai_upload_file(file, api_key, verbose = verbose)

  # Step 2: Submit transcription request
  transcript_id <- assemblyai_submit_transcription(
    upload_url,
    api_key,
    speaker_labels = speaker_labels,
    verbose = verbose
  )

  # Step 3: Poll for completion and get results
  transcript <- assemblyai_get_transcript(transcript_id, api_key, verbose = verbose)

  if (verbose) {
    message("Transcription complete!")
  }

  return(transcript)
}


#' Convert AssemblyAI transcript to SRT subtitle format
#'
#' Converts an AssemblyAI transcript object to SubRip (SRT) subtitle format,
#' suitable for use in video players and editing software.
#'
#' @param transcript A transcript object returned by assemblyai_transcribe().
#' @param file_path Character string. Path where the SRT file should be saved.
#'   If NULL, returns the SRT content as a string without saving.
#' @param max_chars_per_line Integer. Maximum characters per subtitle line.
#'   Default is 42 (standard for subtitles).
#' @param max_duration Numeric. Maximum duration in seconds for a single
#'   subtitle. Default is 7.
#' @param include_speakers Logical. If TRUE and speaker information is available,
#'   includes speaker labels in subtitles. Default is TRUE.
#'
#' @return If file_path is provided, invisibly returns the file path.
#'   Otherwise returns the SRT content as a character string.
#'
#' @details
#' The SRT format includes:
#' - Sequential subtitle numbers
#' - Timestamps in format: HH:MM:SS,mmm --> HH:MM:SS,mmm
#' - Subtitle text (with optional speaker labels)
#' - Blank lines between subtitles
#'
#' @examples
#' \dontrun{
#' transcript <- assemblyai_transcribe("video.mp4", speaker_labels = TRUE)
#'
#' # Save as SRT file
#' assemblyai_transcript_to_srt(transcript, "subtitles.srt")
#'
#' # Get SRT content as string
#' srt_content <- assemblyai_transcript_to_srt(transcript)
#' }
#'
#' @export
assemblyai_transcript_to_srt <- function(transcript,
                                         file_path = NULL,
                                         max_chars_per_line = 42,
                                         max_duration = 7,
                                         include_speakers = TRUE) {

  if (is.null(transcript$words) || length(transcript$words) == 0) {
    stop("Transcript does not contain word-level timing information.", call. = FALSE)
  }

  words <- transcript$words

  # Convert AssemblyAI word list to data frame
  words_df <- data.frame(
    text = sapply(words, function(w) w$text),
    start = sapply(words, function(w) w$start / 1000),  # Convert ms to seconds
    end = sapply(words, function(w) w$end / 1000),      # Convert ms to seconds
    speaker = sapply(words, function(w) {
      if (!is.null(w$speaker)) w$speaker else NA_character_
    }),
    stringsAsFactors = FALSE
  )

  if (nrow(words_df) == 0) {
    stop("No words found in transcript.", call. = FALSE)
  }

  # Group words into subtitle segments
  segments <- assemblyai_create_subtitle_segments(
    words = words_df,
    max_chars = max_chars_per_line,
    max_duration = max_duration
  )

  # Build SRT content
  srt_lines <- character()

  for (i in seq_len(nrow(segments))) {
    # Subtitle number
    srt_lines <- c(srt_lines, as.character(i))

    # Timestamp line
    start_time <- assemblyai_format_srt_timestamp(segments$start[i])
    end_time <- assemblyai_format_srt_timestamp(segments$end[i])
    srt_lines <- c(srt_lines, paste(start_time, "-->", end_time))

    # Subtitle text
    text <- segments$text[i]
    if (include_speakers && !is.na(segments$speaker[i])) {
      text <- paste0("[Speaker ", segments$speaker[i], "] ", text)
    }
    srt_lines <- c(srt_lines, text)

    # Blank line separator
    srt_lines <- c(srt_lines, "")
  }

  srt_content <- paste(srt_lines, collapse = "\n")

  # Save to file or return content
  if (!is.null(file_path)) {
    writeLines(srt_content, file_path, useBytes = TRUE)
    message("SRT file saved to: ", file_path)
    invisible(file_path)
  } else {
    return(srt_content)
  }
}


#' Convert AssemblyAI transcript to plain text format
#'
#' Converts an AssemblyAI transcript object to plain text with optional
#' timestamps and speaker labels.
#'
#' @param transcript A transcript object returned by assemblyai_transcribe().
#' @param file_path Character string. Path where the text file should be saved.
#'   If NULL, returns the text content as a string without saving.
#' @param include_timestamps Logical. If TRUE, includes timestamps for each
#'   speaker section. Default is FALSE.
#' @param include_speakers Logical. If TRUE and speaker information is available,
#'   includes speaker labels. Default is TRUE.
#' @param timestamp_format Character string. Format for timestamps: "seconds"
#'   (e.g., 123.45s) or "time" (e.g., 00:02:03). Default is "time".
#'
#' @return If file_path is provided, invisibly returns the file path.
#'   Otherwise returns the text content as a character string.
#'
#' @examples
#' \dontrun{
#' transcript <- assemblyai_transcribe("interview.mp3", speaker_labels = TRUE)
#'
#' # Save as plain text
#' assemblyai_transcript_to_txt(transcript, "transcript.txt")
#'
#' # With timestamps and speakers
#' assemblyai_transcript_to_txt(
#'   transcript,
#'   "transcript_detailed.txt",
#'   include_timestamps = TRUE,
#'   include_speakers = TRUE
#' )
#'
#' # Get as string
#' text <- assemblyai_transcript_to_txt(transcript)
#' }
#'
#' @export
assemblyai_transcript_to_txt <- function(transcript,
                                         file_path = NULL,
                                         include_timestamps = FALSE,
                                         include_speakers = TRUE,
                                         timestamp_format = "time") {

  # Simple case - just use the text field
  if (!include_timestamps && !include_speakers) {
    text_content <- transcript$text
  } else {
    # Use utterances if available and speakers/timestamps requested
    if (!is.null(transcript$utterances) && length(transcript$utterances) > 0 &&
        (include_speakers || include_timestamps)) {
      text_content <- assemblyai_format_text_from_utterances(
        utterances = transcript$utterances,
        include_timestamps = include_timestamps,
        timestamp_format = timestamp_format
      )
    } else if (!is.null(transcript$words) && length(transcript$words) > 0) {
      # Fall back to words if no utterances
      words_df <- data.frame(
        text = sapply(transcript$words, function(w) w$text),
        start = sapply(transcript$words, function(w) w$start / 1000),
        speaker = sapply(transcript$words, function(w) {
          if (!is.null(w$speaker)) w$speaker else NA_character_
        }),
        stringsAsFactors = FALSE
      )
      text_content <- assemblyai_format_text_from_words(
        words = words_df,
        include_timestamps = include_timestamps,
        include_speakers = include_speakers,
        timestamp_format = timestamp_format
      )
    } else {
      warning("No word-level or utterance data available. Using plain text only.")
      text_content <- transcript$text
    }
  }

  # Save to file or return content
  if (!is.null(file_path)) {
    writeLines(text_content, file_path, useBytes = TRUE)
    message("Text file saved to: ", file_path)
    invisible(file_path)
  } else {
    return(text_content)
  }
}


#' Create subtitle segments from words (AssemblyAI version)
#'
#' Internal function to group words into appropriate subtitle segments.
#'
#' @keywords internal
assemblyai_create_subtitle_segments <- function(words, max_chars, max_duration) {

  segments <- list()
  current_segment <- list(
    text = character(),
    start = NA,
    end = NA,
    speaker = NA_character_
  )

  for (i in seq_len(nrow(words))) {
    word <- words[i, ]

    # Skip if no timing info
    if (is.na(word$start) || is.na(word$end)) {
      next
    }

    # Initialize first segment
    if (is.na(current_segment$start)) {
      current_segment$start <- word$start
      current_segment$speaker <- word$speaker
    }

    # Calculate what the segment would be with this word added
    new_text <- paste(c(current_segment$text, word$text), collapse = " ")
    new_duration <- word$end - current_segment$start

    # Check if we need to start a new segment
    speaker_changed <- !is.na(word$speaker) &&
                      !is.na(current_segment$speaker) &&
                      word$speaker != current_segment$speaker

    if (nchar(new_text) > max_chars ||
        new_duration > max_duration ||
        speaker_changed) {

      # Save current segment if it has content
      if (length(current_segment$text) > 0) {
        segments[[length(segments) + 1]] <- list(
          text = paste(current_segment$text, collapse = " "),
          start = current_segment$start,
          end = current_segment$end,
          speaker = current_segment$speaker
        )
      }

      # Start new segment
      current_segment <- list(
        text = word$text,
        start = word$start,
        end = word$end,
        speaker = word$speaker
      )
    } else {
      # Add word to current segment
      current_segment$text <- c(current_segment$text, word$text)
      current_segment$end <- word$end
    }
  }

  # Add final segment
  if (length(current_segment$text) > 0) {
    segments[[length(segments) + 1]] <- list(
      text = paste(current_segment$text, collapse = " "),
      start = current_segment$start,
      end = current_segment$end,
      speaker = current_segment$speaker
    )
  }

  # Convert to data frame
  do.call(rbind, lapply(segments, function(s) {
    data.frame(
      text = s$text,
      start = s$start,
      end = s$end,
      speaker = s$speaker,
      stringsAsFactors = FALSE
    )
  }))
}


#' Format timestamp for SRT format (AssemblyAI version)
#'
#' @keywords internal
assemblyai_format_srt_timestamp <- function(seconds) {
  if (is.na(seconds)) {
    return("00:00:00,000")
  }

  hours <- floor(seconds / 3600)
  minutes <- floor((seconds %% 3600) / 60)
  secs <- floor(seconds %% 60)
  millis <- round((seconds - floor(seconds)) * 1000)

  sprintf("%02d:%02d:%02d,%03d", hours, minutes, secs, millis)
}


#' Format text from utterances
#'
#' @keywords internal
assemblyai_format_text_from_utterances <- function(utterances,
                                                    include_timestamps,
                                                    timestamp_format) {
  lines <- character()

  for (utt in utterances) {
    prefix <- paste0("[Speaker ", utt$speaker, "]")

    if (include_timestamps && !is.null(utt$start)) {
      start_sec <- utt$start / 1000  # Convert ms to seconds
      if (timestamp_format == "seconds") {
        prefix <- paste0(prefix, " (", round(start_sec, 2), "s)")
      } else {
        prefix <- paste0(prefix, " (", assemblyai_format_time_timestamp(start_sec), ")")
      }
    }

    lines <- c(lines, paste(prefix, utt$text))
  }

  paste(lines, collapse = "\n\n")
}


#' Format text from words
#'
#' @keywords internal
assemblyai_format_text_from_words <- function(words,
                                               include_timestamps,
                                               include_speakers,
                                               timestamp_format) {
  lines <- character()
  current_speaker <- NA_character_
  current_line <- character()

  for (i in seq_len(nrow(words))) {
    word <- words[i, ]

    # Check if speaker changed
    speaker_changed <- include_speakers &&
                      !is.na(word$speaker) &&
                      !is.na(current_speaker) &&
                      word$speaker != current_speaker

    if (speaker_changed) {
      # Save current line
      if (length(current_line) > 0) {
        lines <- c(lines, paste(current_line, collapse = " "))
        current_line <- character()
      }
    }

    # Add speaker label at start of new speaker section
    if (include_speakers &&
        !is.na(word$speaker) &&
        (is.na(current_speaker) || speaker_changed)) {

      current_speaker <- word$speaker

      # Add timestamp if requested
      prefix <- paste0("[Speaker ", current_speaker, "]")
      if (include_timestamps && !is.na(word$start)) {
        if (timestamp_format == "seconds") {
          prefix <- paste0(prefix, " (", round(word$start, 2), "s)")
        } else {
          prefix <- paste0(prefix, " (", assemblyai_format_time_timestamp(word$start), ")")
        }
      }

      current_line <- c(current_line, paste0(prefix, " ", word$text))
    } else {
      current_line <- c(current_line, word$text)
    }
  }

  # Add final line
  if (length(current_line) > 0) {
    lines <- c(lines, paste(current_line, collapse = " "))
  }

  paste(lines, collapse = "\n\n")
}


#' Format timestamp for readable time format (AssemblyAI version)
#'
#' @keywords internal
assemblyai_format_time_timestamp <- function(seconds) {
  if (is.na(seconds)) {
    return("00:00:00")
  }

  hours <- floor(seconds / 3600)
  minutes <- floor((seconds %% 3600) / 60)
  secs <- floor(seconds %% 60)

  if (hours > 0) {
    sprintf("%02d:%02d:%02d", hours, minutes, secs)
  } else {
    sprintf("%02d:%02d", minutes, secs)
  }
}


#' Package environment to store API credentials
#' @keywords internal
.assemblyai_env <- new.env(parent = emptyenv())
