#' Transcribe audio or video file using ElevenLabs API
#'
#' Upload an audio or video file to ElevenLabs for speech-to-text transcription.
#' Returns data frame with individual words, can be transformed into .txt or .srt
#'
#' @param file Character string. Path to the audio or video file to transcribe.
#' @param model_id Character string. Model to use for transcription.
#'   Default is "scribe_v1" (most accurate, supports 99 languages).
#'   Alternative: "scribe_v1_experimental" (may be better for multi-language audio).
#' @param language_code Character string or NULL. Language code for transcription
#'   (e.g., "en", "es", "fr"). If NULL, language is auto-detected.
#' @param diarize Logical. If TRUE, identifies different speakers in the audio.
#'   Default is FALSE.
#' @param num_speakers Integer or NULL. Expected number of speakers for diarization.
#'   Only used when diarize = TRUE and total number of speakers is known.
#' @param timestamps_granularity Character string. Level of timestamp detail:
#'   "word" (default), "character", or "none".
#' @param delete_after Logical. If TRUE (default), automatically deletes the
#'   transcript from ElevenLabs after successful download.
#' @param verbose Logical. If TRUE, prints progress messages. Default is TRUE.
#' @param ... Additional parameters passed to the API (e.g., temperature, seed).
#'
#' @return A list containing the transcription results:
#'   \item{text}{Full transcript as plain text}
#'   \item{words}{Data frame with word-level details (text, timing, speaker)}
#'   \item{language_code}{Detected or specified language}
#'   \item{language_probability}{Confidence in language detection}
#'   \item{transcription_id}{Unique ID for this transcript}
#'   \item{model_id}{Model used for transcription}
#'
#' @details
#' The function uses synchronous processing by default, which works for files
#' of any size. The API has a 10-minute timeout for uploads and processing.
#'
#' Asynchronous mode (async=TRUE) is available for advanced users who have
#' configured webhooks in their ElevenLabs workspace, but is not required.
#'
#' After successful transcription, the transcript is automatically deleted
#' from ElevenLabs servers unless delete_after=FALSE.
#'
#' @examples
#' \dontrun{
#' # Simple transcription
#' transcript <- elevenlabs_transcribe("interview.mp3")
#'
#' # With speaker diarization
#' transcript <- elevenlabs_transcribe(
#'   file = "meeting.mp4",
#'   diarize = TRUE,
#'   num_speakers = 3
#' )
#'
#' }
#'
#' @export
elevenlabs_transcribe <- function(file,
                                   model_id = "scribe_v1",
                                   language_code = NULL,
                                   diarize = FALSE,
                                   num_speakers = NULL,
                                   timestamps_granularity = "word",
                                   delete_after = TRUE,
                                   verbose = TRUE,
                                   ...) {

  # Validate inputs
  if (!file.exists(file)) {
    stop("File not found: ", file, call. = FALSE)
  }

  # Get API key
  api_key <- elevenlabs_get_api_key()

  # Build request body
  body_list <- list(
    model_id = model_id,
    file = httr::upload_file(file)
  )

  if (!is.null(language_code)) {
    body_list$language_code <- language_code
  }

  if (diarize) {
    body_list$diarize <- "true"
    if (!is.null(num_speakers)) {
      body_list$num_speakers <- as.character(num_speakers)
    }
  }

  if (timestamps_granularity != "word") {
    body_list$timestamps_granularity <- timestamps_granularity
  }


  # Add any additional parameters
  extra_params <- list(...)
  if (length(extra_params) > 0) {
    body_list <- c(body_list, extra_params)
  }

  # Make API request
  if (verbose) {
    message("Uploading file and starting transcription...")
  }

  response <- httr::POST(
    url = "https://api.elevenlabs.io/v1/speech-to-text",
    httr::add_headers("xi-api-key" = api_key),
    body = body_list,
    encode = "multipart",
    httr::timeout(600)  # 10 minute timeout for upload
  )

  # Check for errors
  if (httr::http_error(response)) {
    error_content <- httr::content(response, as = "text", encoding = "UTF-8")

    # Special handling for webhook configuration error
    if (grepl("no_webhooks_configured|webhook", error_content, ignore.case = TRUE)) {
      stop(
        "Async mode requires webhooks configured in your ElevenLabs workspace.\n",
        "Please configure webhooks at https://elevenlabs.io/app/speech-to-text/webhooks\n",
        "or use the default synchronous mode (async = FALSE).",
        call. = FALSE
      )
    }

    stop(
      sprintf(
        "API request failed with status %d:\n%s",
        httr::status_code(response),
        error_content
      ),
      call. = FALSE
    )
  }

  # Parse response
  result <- httr::content(response, as = "parsed", encoding = "UTF-8")


  # Format the result
  transcript <- elevenlabs_format_transcript_response(result)

  # Auto-delete if requested
  if (delete_after && !is.null(transcript$transcription_id)) {
    tryCatch({
      elevenlabs_delete_transcript(transcript$transcription_id, verbose = FALSE)
      if (verbose) {
        message("Transcript deleted from ElevenLabs servers.")
      }
    }, error = function(e) {
      warning("Could not delete transcript: ", e$message, call. = FALSE)
    })
  }

  if (verbose) {
    message("Transcription complete!")
  }

  return(transcript)
}




#' Format transcript API response
#'
#' Internal function to convert API response into a standardized format.
#'
#' @param result Raw API response (parsed JSON).
#' @return Formatted transcript list.
#' @keywords internal
elevenlabs_format_transcript_response <- function(result) {

  # Handle single channel response
  if (!is.null(result$text)) {
    words_df <- NULL
    if (!is.null(result$words) && length(result$words) > 0) {
      words_df <- do.call(rbind, lapply(result$words, function(w) {
        data.frame(
          text = w$text %||% "",
          start = w$start %||% NA,
          end = w$end %||% NA,
          type = w$type %||% "word",
          speaker_id = w$speaker_id %||% NA,
          stringsAsFactors = FALSE
        )
      }))
    }

    return(list(
      text = result$text,
      words = words_df,
      language_code = result$language_code %||% NA,
      language_probability = result$language_probability %||% NA,
      transcription_id = result$transcription_id %||% NA,
      model_id = "scribe_v1"
    ))
  }

  # Handle multi-channel response
  if (!is.null(result$transcripts)) {
    # Combine all channel transcripts
    all_text <- sapply(result$transcripts, function(t) t$text %||% "")
    combined_text <- paste(all_text, collapse = "\n\n")

    # Combine all words
    all_words <- do.call(rbind, lapply(seq_along(result$transcripts), function(i) {
      channel <- result$transcripts[[i]]
      if (!is.null(channel$words) && length(channel$words) > 0) {
        words_df <- do.call(rbind, lapply(channel$words, function(w) {
          data.frame(
            text = w$text %||% "",
            start = w$start %||% NA,
            end = w$end %||% NA,
            type = w$type %||% "word",
            speaker_id = w$speaker_id %||% NA,
            channel = i,
            stringsAsFactors = FALSE
          )
        }))
        return(words_df)
      }
      return(NULL)
    }))

    return(list(
      text = combined_text,
      words = all_words,
      language_code = result$transcripts[[1]]$language_code %||% NA,
      language_probability = result$transcripts[[1]]$language_probability %||% NA,
      transcription_id = result$transcription_id %||% NA,
      model_id = "scribe_v1",
      channels = length(result$transcripts)
    ))
  }

  stop("Unexpected transcript response format.", call. = FALSE)
}


#' Null-coalescing operator
#'
#' @keywords internal
`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}
