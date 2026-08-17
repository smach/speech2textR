#' Authenticate to the Mistral AI API
#'
#' This function sets up authentication for the Mistral AI API by storing
#' your API key for use in subsequent API calls. The API key is stored
#' in a package-level environment for the duration of your R session.
#'
#' @param api_key Character string containing your Mistral API key.
#'   If not provided, the function will look for the MISTRAL_API_KEY
#'   environment variable.
#' @param validate Logical. If TRUE, validates the API key by making a test
#'   request to the Mistral API. Default is FALSE.
#'
#' @return Invisibly returns TRUE if authentication is successful. Prints
#'   a confirmation message to the console.
#'
#' @details
#' You can obtain an API key from your Mistral AI console at
#' \url{https://console.mistral.ai/}. It's recommended to store your API key
#' in an environment variable (MISTRAL_API_KEY) rather than hardcoding it
#' in your scripts.
#'
#' The API key is stored in memory for the current R session and will
#' need to be set again if you restart R.
#'
#' @examples
#' \dontrun{
#' # Authenticate with API key directly
#' mistral_auth(api_key = "your_api_key_here")
#'
#' # Or set environment variable first
#' Sys.setenv(MISTRAL_API_KEY = "your_api_key_here")
#' mistral_auth()
#'
#' # Validate the API key
#' mistral_auth(api_key = "your_api_key_here", validate = TRUE)
#' }
#'
#' @export
mistral_auth <- function(api_key = Sys.getenv("MISTRAL_API_KEY"), validate = FALSE) {
  # Try to get API key from parameter or environment variable
  if (is.null(api_key)) {
    api_key <- Sys.getenv("MISTRAL_API_KEY")
  }

  if (!is.character(api_key) || length(api_key) != 1 || is.na(api_key) || nchar(api_key) == 0) {
    stop(
      "No API key provided. Please either:\n",
      "  1. Pass api_key parameter: mistral_auth(api_key = 'your_key')\n",
      "  2. Set MISTRAL_API_KEY environment variable: ",
      "Sys.setenv(MISTRAL_API_KEY = 'your_key')",
      call. = FALSE
    )
  }

  # Store API key in package environment
  .mistral_env$api_key <- api_key

  # Optionally validate the API key by making a test request
  if (validate) {
    tryCatch({
      # Make a simple request to validate credentials
      response <- httr2::request("https://api.mistral.ai/v1/models") |>
        httr2::req_headers(Authorization = paste("Bearer", api_key)) |>
        httr2::req_method("GET") |>
        httr2::req_error(is_error = function(resp) FALSE) |>
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
    message("Mistral API key stored successfully!")
  }

  invisible(TRUE)
}


#' Get stored Mistral API key
#'
#' Internal function to retrieve the stored API key. This is used by
#' other functions in the package to access the authenticated credentials.
#'
#' @return Character string containing the API key.
#' @keywords internal
mistral_get_api_key <- function() {
  api_key <- .mistral_env$api_key

  if (is.null(api_key)) {
    stop(
      "No API key found. Please authenticate first using mistral_auth().",
      call. = FALSE
    )
  }

  return(api_key)
}


#' Check if authenticated to Mistral API
#'
#' Check whether an API key has been set for the current session.
#'
#' @return Logical value indicating whether an API key is stored.
#'
#' @examples
#' \dontrun{
#' mistral_is_authenticated()
#' }
#'
#' @export
mistral_is_authenticated <- function() {
  !is.null(.mistral_env$api_key)
}


#' Transcribe audio or video file using the Mistral AI API
#'
#' Send an audio or video file to Mistral's transcription endpoint, which
#' runs the Voxtral Mini Transcribe 2 model.
#'
#' @param file Character string. Path to a local audio or video file to
#'   transcribe. Supply either \code{file} or \code{file_url}, not both.
#' @param file_url Character string. Publicly reachable URL of a file to
#'   transcribe, as an alternative to uploading one.
#' @param model Character string. Model to use. Default is
#'   \code{"voxtral-mini-latest"}, which currently runs Voxtral Mini
#'   Transcribe 2.
#' @param language Character string. Two-letter language code such as
#'   \code{"en"} or \code{"es"}. The default (NULL) lets the model detect the
#'   language. Note that the API does not accept a language together with
#'   timestamps; see Details.
#' @param diarize Logical. If TRUE, identifies different speakers in the
#'   audio. Default is FALSE.
#' @param timestamps Character vector. Timestamp granularities to request:
#'   \code{"segment"}, \code{"word"}, both, or NULL for none. Default is
#'   \code{"segment"}, which is what the caption formatters need.
#' @param context_bias Character vector. Up to 100 words to steer spelling of
#'   names, jargon, or other unusual vocabulary. Individual entries may not
#'   contain spaces or commas, so join multi-word phrases with underscores
#'   (for example \code{"affordable_health_care"}).
#' @param temperature Numeric. Sampling temperature. The default (NULL) uses
#'   the API default.
#' @param verbose Logical. If TRUE, prints progress messages. Default is TRUE.
#'
#' @return A list containing the transcription results:
#'   \item{text}{Full transcript as plain text}
#'   \item{segments}{Data frame of timed segments with columns text, start,
#'     end, speaker_id and score, or NULL if segment timestamps were not
#'     requested. Times are in seconds and text is whitespace-trimmed.}
#'   \item{words}{Data frame of timed words in the same shape as
#'     \code{segments}, or NULL if word timestamps were not requested}
#'   \item{language}{Detected or supplied language code}
#'   \item{model}{Model that produced the transcript}
#'   \item{usage}{List of token and audio-duration counts returned by the API}
#'
#' @details
#' You must authenticate first using \code{mistral_auth()}.
#'
#' Voxtral Mini Transcribe 2 handles recordings up to roughly three hours in
#' a single request, so long files do not need to be split.
#'
#' Two API quirks are worth knowing:
#' \itemize{
#'   \item \code{language} and \code{timestamps} cannot be sent in the same
#'     request. If you set both, the timestamps are dropped and a warning is
#'     issued. Leave \code{language} at NULL to get timed segments back.
#'   \item Speaker labels are attached to timed segments, so \code{diarize}
#'     needs timestamps to be useful. If you set \code{diarize = TRUE} without
#'     timestamps, segment timestamps are requested for you.
#' }
#'
#' @examples
#' \dontrun{
#' # Simple transcription
#' transcript <- mistral_transcribe("interview.mp3")
#'
#' # With speaker diarization
#' transcript <- mistral_transcribe("meeting.mp4", diarize = TRUE)
#'
#' # Word-level timestamps
#' transcript <- mistral_transcribe("interview.mp3", timestamps = "word")
#'
#' # Help the model spell unusual terms
#' transcript <- mistral_transcribe(
#'   "rstats_talk.mp4",
#'   context_bias = c("Quarto", "tidyverse", "data_frame")
#' )
#'
#' # Transcribe a file already on the web
#' transcript <- mistral_transcribe(file_url = "https://example.com/talk.mp3")
#'
#' # Access the text
#' cat(transcript$text)
#' }
#'
#' @export
mistral_transcribe <- function(file = NULL,
                               file_url = NULL,
                               model = "voxtral-mini-latest",
                               language = NULL,
                               diarize = FALSE,
                               timestamps = "segment",
                               context_bias = NULL,
                               temperature = NULL,
                               verbose = TRUE) {

  # Validate inputs
  if (is.null(file) && is.null(file_url)) {
    stop("Please supply either a local 'file' or a 'file_url'.", call. = FALSE)
  }

  if (!is.null(file) && !is.null(file_url)) {
    stop("Please supply only one of 'file' or 'file_url', not both.", call. = FALSE)
  }

  if (!is.null(file) && !file.exists(file)) {
    stop("File not found: ", file, call. = FALSE)
  }

  timestamps <- mistral_check_timestamps(timestamps)
  context_bias <- mistral_check_context_bias(context_bias)

  # Speaker labels ride along with timed segments
  if (isTRUE(diarize) && is.null(timestamps)) {
    timestamps <- "segment"
  }

  # The API rejects a language and timestamps in the same request
  if (!is.null(language) && !is.null(timestamps)) {
    warning(
      "The Mistral API does not accept 'language' and timestamps in the same ",
      "request, so timestamps were dropped. Leave 'language' as NULL to get ",
      "timed segments back.",
      call. = FALSE
    )
    timestamps <- NULL
  }

  if (isTRUE(diarize) && is.null(timestamps)) {
    warning(
      "Speaker labels are attached to timed segments, so 'diarize' has no ",
      "effect without timestamps.",
      call. = FALSE
    )
  }

  # Get API key
  api_key <- mistral_get_api_key()

  if (verbose) {
    message("Sending audio to Mistral for transcription...")
  }

  # Build the multipart form. Array parameters are sent as repeated fields.
  body <- list(model = model)

  if (!is.null(file)) {
    body$file <- curl::form_file(file)
  } else {
    body$file_url <- file_url
  }

  if (!is.null(language)) {
    body$language <- language
  }

  if (isTRUE(diarize)) {
    body$diarize <- "true"
  }

  if (!is.null(temperature)) {
    body$temperature <- as.character(temperature)
  }

  for (granularity in timestamps) {
    body <- c(body, mistral_form_field("timestamp_granularities", granularity))
  }

  for (term in context_bias) {
    body <- c(body, mistral_form_field("context_bias", term))
  }

  request <- httr2::request("https://api.mistral.ai/v1/audio/transcriptions") |>
    httr2::req_headers(Authorization = paste("Bearer", api_key)) |>
    httr2::req_timeout(1800) |>
    httr2::req_error(body = mistral_error_body)

  response <- do.call(httr2::req_body_multipart, c(list(request), body)) |>
    httr2::req_perform()

  result <- httr2::resp_body_json(response)

  if (verbose) {
    message("Transcription complete!")
  }

  mistral_format_transcript_response(result, timestamps = timestamps)
}


#' Build a single named multipart form field
#'
#' Internal helper. Array parameters such as timestamp_granularities and
#' context_bias are sent as repeated form fields, which means the body list
#' needs duplicate names.
#'
#' @param name Character string. Form field name.
#' @param value Character string. Form field value.
#' @return A one-element named list.
#' @keywords internal
mistral_form_field <- function(name, value) {
  field <- list(value)
  names(field) <- name
  field
}


#' Extract a readable message from a Mistral API error response
#'
#' Internal function passed to \code{httr2::req_error()} so that failures
#' report what the API objected to rather than just a status code.
#'
#' @param response An httr2 response object.
#' @return Character vector of message lines, or NULL if nothing useful
#'   could be extracted.
#' @keywords internal
mistral_error_body <- function(response) {
  parsed <- tryCatch(
    httr2::resp_body_json(response),
    error = function(e) NULL
  )

  if (is.null(parsed)) {
    return(NULL)
  }

  # Mistral reports errors as {"message": ...}, {"detail": ...} or
  # {"error": {"message": ...}} depending on which layer rejected the request
  detail <- parsed$error$message %||% parsed$message %||% parsed$detail

  if (is.null(detail)) {
    return(NULL)
  }

  if (is.list(detail)) {
    detail <- vapply(
      detail,
      function(d) as.character(d$msg %||% d$message %||% paste(unlist(d), collapse = " ")),
      character(1)
    )
  }

  as.character(detail)
}


#' Validate requested timestamp granularities
#'
#' @param timestamps Character vector or NULL.
#' @return The validated vector, or NULL.
#' @keywords internal
mistral_check_timestamps <- function(timestamps) {
  if (is.null(timestamps) || length(timestamps) == 0) {
    return(NULL)
  }

  valid <- c("segment", "word")
  bad <- setdiff(timestamps, valid)

  if (length(bad) > 0) {
    stop(
      "Invalid 'timestamps' value(s): ", paste(bad, collapse = ", "),
      ". Use \"segment\", \"word\", both, or NULL.",
      call. = FALSE
    )
  }

  unique(timestamps)
}


#' Validate context biasing terms
#'
#' @param context_bias Character vector or NULL.
#' @return The validated vector, or NULL.
#' @keywords internal
mistral_check_context_bias <- function(context_bias) {
  if (is.null(context_bias) || length(context_bias) == 0) {
    return(NULL)
  }

  if (!is.character(context_bias)) {
    stop("'context_bias' must be a character vector.", call. = FALSE)
  }

  if (length(context_bias) > 100) {
    stop(
      "'context_bias' accepts at most 100 terms, but ", length(context_bias),
      " were supplied.",
      call. = FALSE
    )
  }

  bad <- context_bias[grepl("[,[:space:]]", context_bias)]

  if (length(bad) > 0) {
    stop(
      "'context_bias' terms cannot contain spaces or commas: ",
      paste0("\"", bad, "\"", collapse = ", "),
      ". Join multi-word phrases with underscores, for example ",
      "\"affordable_health_care\".",
      call. = FALSE
    )
  }

  context_bias
}


#' Format Mistral transcript API response
#'
#' Internal function to convert the API response into a standardized format.
#'
#' @param result Raw API response (parsed JSON).
#' @param timestamps Character vector of granularities that were requested,
#'   used to decide whether the returned chunks are segments or words.
#' @return Formatted transcript list.
#' @keywords internal
mistral_format_transcript_response <- function(result, timestamps = NULL) {

  if (is.null(result$text)) {
    stop("Unexpected transcript response format.", call. = FALSE)
  }

  segments_df <- mistral_chunks_to_df(result$segments)
  words_df <- mistral_chunks_to_df(result$words)

  # When only word timestamps are requested the API returns one chunk per
  # word in the same field, so report those chunks as words
  if (is.null(words_df) && "word" %in% timestamps && !is.null(segments_df)) {
    words_df <- segments_df
    if (!("segment" %in% timestamps)) {
      segments_df <- NULL
    }
  }

  list(
    text = result$text,
    segments = segments_df,
    words = words_df,
    language = result$language %||% NA,
    model = result$model %||% NA,
    usage = result$usage
  )
}


#' Convert Mistral transcription chunks to a data frame
#'
#' Internal function shared by the segment and word paths. Start and end
#' times are already in seconds, so no conversion is needed.
#'
#' Word chunks arrive with a leading space (" everyone"), which is how the
#' model tokenizes rather than anything meaningful, so text is trimmed. That
#' keeps the data frame consistent with the ElevenLabs and AssemblyAI ones
#' and stops joined text from picking up doubled spaces. The untouched
#' transcript is still available in the \code{text} element.
#'
#' @param chunks List of chunks from the API response.
#' @return Data frame with text, start, end, speaker_id and score columns,
#'   or NULL if there are no chunks.
#' @keywords internal
mistral_chunks_to_df <- function(chunks) {
  if (is.null(chunks) || length(chunks) == 0) {
    return(NULL)
  }

  do.call(rbind, lapply(chunks, function(chunk) {
    data.frame(
      text = trimws(chunk$text %||% ""),
      start = as.numeric(chunk$start %||% NA),
      end = as.numeric(chunk$end %||% NA),
      speaker_id = as.character(chunk$speaker_id %||% NA),
      score = as.numeric(chunk$score %||% NA),
      stringsAsFactors = FALSE
    )
  }))
}


#' Convert Mistral transcript to SRT subtitle format
#'
#' Converts a Mistral transcript object to SRT format for use as video
#' subtitles or closed captions.
#'
#' @param transcript A transcript object returned by mistral_transcribe().
#' @param file_path Character string. Path where the SRT file should be saved.
#'   If NULL, returns the SRT content as a string without saving.
#' @param max_chars_per_line Integer. Maximum characters per subtitle line.
#'   Default is 42. Only used when word-level timings are available.
#' @param max_duration Numeric. Maximum duration in seconds for each subtitle.
#'   Default is 7. Only used when word-level timings are available.
#' @param include_speakers Logical. If TRUE and speaker information is
#'   available, includes speaker labels. Default is TRUE.
#'
#' @return If file_path is provided, invisibly returns the file path.
#'   Otherwise returns the SRT content as a character string.
#'
#' @details
#' When the transcript has word-level timings, words are regrouped into
#' subtitles that respect \code{max_chars_per_line} and \code{max_duration}.
#' Otherwise the segments returned by the API are used as-is; they are
#' already sentence-shaped and timed, so they make reasonable captions.
#'
#' @examples
#' \dontrun{
#' transcript <- mistral_transcribe("video.mp4", diarize = TRUE)
#'
#' # Save as SRT file
#' mistral_transcript_to_srt(transcript, "subtitles.srt")
#'
#' # Get SRT content as string
#' srt_content <- mistral_transcript_to_srt(transcript)
#' }
#'
#' @export
mistral_transcript_to_srt <- function(transcript,
                                      file_path = NULL,
                                      max_chars_per_line = 42,
                                      max_duration = 7,
                                      include_speakers = TRUE) {

  if (!is.null(transcript$words) && nrow(transcript$words) > 0) {
    # Regroup words into subtitle-sized chunks
    segments <- mistral_create_subtitle_segments(
      words = transcript$words,
      max_chars = max_chars_per_line,
      max_duration = max_duration
    )
  } else if (!is.null(transcript$segments) && nrow(transcript$segments) > 0) {
    # Use the segments the API already produced
    segments <- transcript$segments
  } else {
    stop(
      "Transcript does not contain timing information. Transcribe with ",
      "timestamps = \"segment\" or timestamps = \"word\".",
      call. = FALSE
    )
  }

  # Build SRT content
  srt_lines <- character()

  for (i in seq_len(nrow(segments))) {
    # Subtitle number
    srt_lines <- c(srt_lines, as.character(i))

    # Timestamp line
    start_time <- mistral_format_srt_timestamp(segments$start[i])
    end_time <- mistral_format_srt_timestamp(segments$end[i])
    srt_lines <- c(srt_lines, paste(start_time, "-->", end_time))

    # Subtitle text
    text <- segments$text[i]
    if (include_speakers && !is.na(segments$speaker_id[i])) {
      text <- paste0(mistral_speaker_label(segments$speaker_id[i]), " ", text)
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


#' Convert Mistral transcript to plain text format
#'
#' Converts a Mistral transcript object to plain text with optional
#' timestamps and speaker labels.
#'
#' @param transcript A transcript object returned by mistral_transcribe().
#' @param file_path Character string. Path where the text file should be saved.
#'   If NULL, returns the text content as a string without saving.
#' @param include_timestamps Logical. If TRUE, includes timestamps for each
#'   speaker section. Default is FALSE.
#' @param include_speakers Logical. If TRUE and speaker information is
#'   available, includes speaker labels. Default is TRUE.
#' @param timestamp_format Character string. Format for timestamps: "seconds"
#'   (e.g., 123.45s) or "time" (e.g., 00:02:03). Default is "time".
#'
#' @return If file_path is provided, invisibly returns the file path.
#'   Otherwise returns the text content as a character string.
#'
#' @examples
#' \dontrun{
#' transcript <- mistral_transcribe("interview.mp3", diarize = TRUE)
#'
#' # Save as plain text
#' mistral_transcript_to_txt(transcript, "transcript.txt")
#'
#' # With timestamps and speakers
#' mistral_transcript_to_txt(
#'   transcript,
#'   "transcript_detailed.txt",
#'   include_timestamps = TRUE,
#'   include_speakers = TRUE
#' )
#'
#' # Get as string
#' text <- mistral_transcript_to_txt(transcript)
#' }
#'
#' @export
mistral_transcript_to_txt <- function(transcript,
                                      file_path = NULL,
                                      include_timestamps = FALSE,
                                      include_speakers = TRUE,
                                      timestamp_format = "time") {

  # Segments read better than words here, so prefer them
  chunks <- transcript$segments
  if (is.null(chunks) || nrow(chunks) == 0) {
    chunks <- transcript$words
  }

  has_speakers <- !is.null(chunks) &&
    nrow(chunks) > 0 &&
    any(!is.na(chunks$speaker_id))

  # Simple case - just use the text field
  if (!include_timestamps && !(include_speakers && has_speakers)) {
    text_content <- transcript$text
  } else if (is.null(chunks) || nrow(chunks) == 0) {
    warning("No timing or speaker data available. Using plain text only.")
    text_content <- transcript$text
  } else {
    text_content <- mistral_format_text_with_metadata(
      chunks = chunks,
      include_timestamps = include_timestamps,
      include_speakers = include_speakers && has_speakers,
      timestamp_format = timestamp_format
    )
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


#' Create subtitle segments from words (Mistral version)
#'
#' Internal function to group words into appropriate subtitle segments.
#'
#' @keywords internal
mistral_create_subtitle_segments <- function(words, max_chars, max_duration) {

  segments <- list()
  current_segment <- list(
    text = character(),
    start = NA,
    end = NA,
    speaker_id = NA_character_
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
      current_segment$speaker_id <- word$speaker_id
    }

    # Calculate what the segment would be with this word added
    new_text <- paste(c(current_segment$text, word$text), collapse = " ")
    new_duration <- word$end - current_segment$start

    # Check if we need to start a new segment
    speaker_changed <- !is.na(word$speaker_id) &&
                      !is.na(current_segment$speaker_id) &&
                      word$speaker_id != current_segment$speaker_id

    if (nchar(new_text) > max_chars ||
        new_duration > max_duration ||
        speaker_changed) {

      # Save current segment if it has content
      if (length(current_segment$text) > 0) {
        segments[[length(segments) + 1]] <- list(
          text = paste(current_segment$text, collapse = " "),
          start = current_segment$start,
          end = current_segment$end,
          speaker_id = current_segment$speaker_id
        )
      }

      # Start new segment
      current_segment <- list(
        text = word$text,
        start = word$start,
        end = word$end,
        speaker_id = word$speaker_id
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
      speaker_id = current_segment$speaker_id
    )
  }

  # Convert to data frame
  do.call(rbind, lapply(segments, function(s) {
    data.frame(
      text = s$text,
      start = s$start,
      end = s$end,
      speaker_id = s$speaker_id,
      stringsAsFactors = FALSE
    )
  }))
}


#' Format a speaker label for display
#'
#' Mistral speaker ids look like "speaker_0", so avoid printing
#' "[Speaker speaker_0]".
#'
#' @keywords internal
mistral_speaker_label <- function(speaker_id) {
  if (grepl("^speaker", speaker_id, ignore.case = TRUE)) {
    paste0("[", speaker_id, "]")
  } else {
    paste0("[Speaker ", speaker_id, "]")
  }
}


#' Format timestamp for SRT format (Mistral version)
#'
#' @keywords internal
mistral_format_srt_timestamp <- function(seconds) {
  if (is.na(seconds)) {
    return("00:00:00,000")
  }

  hours <- floor(seconds / 3600)
  minutes <- floor((seconds %% 3600) / 60)
  secs <- floor(seconds %% 60)
  millis <- round((seconds - floor(seconds)) * 1000)

  sprintf("%02d:%02d:%02d,%03d", hours, minutes, secs, millis)
}


#' Format timestamp for readable time format (Mistral version)
#'
#' @keywords internal
mistral_format_time_timestamp <- function(seconds) {
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


#' Format text with metadata (timestamps and speakers)
#'
#' Internal function. Consecutive chunks from the same speaker are joined
#' into one paragraph. Without speaker labels, each chunk becomes its own
#' timestamped line.
#'
#' @keywords internal
mistral_format_text_with_metadata <- function(chunks,
                                              include_timestamps,
                                              include_speakers,
                                              timestamp_format) {

  if (is.null(chunks) || nrow(chunks) == 0) {
    return("")
  }

  format_start <- function(start) {
    if (timestamp_format == "seconds") {
      paste0("(", round(start, 2), "s)")
    } else {
      paste0("(", mistral_format_time_timestamp(start), ")")
    }
  }

  if (!include_speakers) {
    # One timestamped line per chunk
    lines <- vapply(seq_len(nrow(chunks)), function(i) {
      paste(format_start(chunks$start[i]), chunks$text[i])
    }, character(1))

    return(paste(lines, collapse = "\n\n"))
  }

  # Group consecutive chunks by speaker
  speakers <- chunks$speaker_id
  keys <- ifelse(is.na(speakers), "", speakers)
  starts_block <- c(TRUE, keys[-1] != keys[-length(keys)])
  block_ids <- cumsum(starts_block)

  lines <- vapply(unique(block_ids), function(block) {
    rows <- chunks[block_ids == block, ]
    speaker <- rows$speaker_id[1]

    prefix <- if (is.na(speaker)) "" else mistral_speaker_label(speaker)

    if (include_timestamps) {
      prefix <- trimws(paste(prefix, format_start(rows$start[1])))
    }

    body <- paste(rows$text, collapse = " ")

    if (nchar(prefix) == 0) body else paste(prefix, body)
  }, character(1))

  paste(lines, collapse = "\n\n")
}


#' Mistral package environment
#'
#' Internal environment for storing the API key.
#' @keywords internal
.mistral_env <- new.env(parent = emptyenv())
