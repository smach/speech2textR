#' Retrieve a transcript by ID
#'
#' Fetch a previously generated transcript from ElevenLabs using its
#' transcription ID. Useful for retrieving async transcriptions or
#' re-downloading existing transcripts.
#'
#' @param transcription_id Character string. The unique ID of the transcript
#'   to retrieve.
#' @param delete_after Logical. If TRUE, automatically deletes the transcript
#'   from ElevenLabs after successful retrieval. Default is TRUE.
#' @param verbose Logical. If TRUE, prints status messages. Default is TRUE.
#'
#' @return A list containing the transcription results (same format as
#'   elevenlabs_transcribe()).
#'
#' @examples
#' \dontrun{
#' # Retrieve a transcript
#' transcript <- elevenlabs_get_transcript("transcription_id_here")
#'
#' # Retrieve without auto-deleting
#' transcript <- elevenlabs_get_transcript("transcription_id_here", delete_after = FALSE)
#' }
#'
#' @export
elevenlabs_get_transcript <- function(transcription_id,
                          delete_after = TRUE,
                          verbose = TRUE) {

  if (missing(transcription_id) || is.null(transcription_id) || transcription_id == "") {
    stop("transcription_id is required.", call. = FALSE)
  }

  # Get the raw transcript
  result <- elevenlabs_get_transcript_raw(transcription_id)

  # Format the result
  transcript <- elevenlabs_format_transcript_response(result)

  if (verbose) {
    message("Transcript retrieved successfully.")
  }

  # Auto-delete if requested
  if (delete_after) {
    tryCatch({
      elevenlabs_delete_transcript(transcription_id, verbose = FALSE)
      if (verbose) {
        message("Transcript deleted from ElevenLabs servers.")
      }
    }, error = function(e) {
      warning("Could not delete transcript: ", e$message, call. = FALSE)
    })
  }

  return(transcript)
}


#' Retrieve raw transcript from API
#'
#' Internal function to make the GET request for a transcript.
#'
#' @param transcription_id Character string. The transcription ID.
#' @return Raw parsed API response.
#' @keywords internal
elevenlabs_get_transcript_raw <- function(transcription_id) {

  # Get API key
  api_key <- elevenlabs_get_api_key()

  # Make API request
  response <- httr::GET(
    url = sprintf(
      "https://api.elevenlabs.io/v1/speech-to-text/transcripts/%s",
      transcription_id
    ),
    httr::add_headers("xi-api-key" = api_key),
    httr::timeout(30)
  )

  # Check for errors
  if (httr::http_error(response)) {
    error_content <- httr::content(response, as = "text", encoding = "UTF-8")
    stop(
      sprintf(
        "Failed to retrieve transcript (status %d):\n%s",
        httr::status_code(response),
        error_content
      ),
      call. = FALSE
    )
  }

  # Parse and return response
  result <- httr::content(response, as = "parsed", encoding = "UTF-8")
  return(result)
}


#' Delete a transcript from ElevenLabs
#'
#' Remove a transcript from ElevenLabs servers. This is useful for managing
#' storage quota and ensuring privacy.
#'
#' @param transcription_id Character string. The unique ID of the transcript
#'   to delete.
#' @param verbose Logical. If TRUE, prints confirmation message. Default is TRUE.
#'
#' @return Invisibly returns TRUE if deletion was successful.
#'
#' @examples
#' \dontrun{
#' # Delete a transcript
#' elevenlabs_delete_transcript("transcription_id_here")
#' }
#'
#' @export
elevenlabs_delete_transcript <- function(transcription_id, verbose = TRUE) {

  if (missing(transcription_id) || is.null(transcription_id) || transcription_id == "") {
    stop("transcription_id is required.", call. = FALSE)
  }

  # Get API key
  api_key <- elevenlabs_get_api_key()

  # Make DELETE request
  response <- httr::DELETE(
    url = sprintf(
      "https://api.elevenlabs.io/v1/speech-to-text/transcripts/%s",
      transcription_id
    ),
    httr::add_headers("xi-api-key" = api_key),
    httr::timeout(30)
  )

  # Check for errors
  if (httr::http_error(response)) {
    error_content <- httr::content(response, as = "text", encoding = "UTF-8")
    stop(
      sprintf(
        "Failed to delete transcript (status %d):\n%s",
        httr::status_code(response),
        error_content
      ),
      call. = FALSE
    )
  }

  if (verbose) {
    message("Transcript deleted successfully.")
  }

  invisible(TRUE)
}
