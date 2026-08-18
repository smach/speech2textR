#' Convert transcript to SRT subtitle format
#'
#' Converts a transcript object to SubRip (SRT) subtitle format, suitable
#' for use in video players and editing software.
#'
#' @param transcript A transcript object returned by elevenlabs_transcribe()
#'   or elevenlabs_get_transcript().
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
#' transcript <- elevenlabs_transcribe("video.mp4")
#'
#' # Save as SRT file
#' elevenlabs_transcript_to_srt(transcript, "subtitles.srt")
#'
#' # Get SRT content as string
#' srt_content <- elevenlabs_transcript_to_srt(transcript)
#' }
#'
#' @export
elevenlabs_transcript_to_srt <- function(transcript,
                              file_path = NULL,
                              max_chars_per_line = 42,
                              max_duration = 7,
                              include_speakers = TRUE) {

  if (is.null(transcript$words) || nrow(transcript$words) == 0) {
    stop("Transcript does not contain word-level timing information.", call. = FALSE)
  }

  words <- transcript$words

  # Remove non-word elements (spacing, audio events) for cleaner subtitles
  words <- words[words$type == "word", ]

  if (nrow(words) == 0) {
    stop("No words found in transcript.", call. = FALSE)
  }

  # Group words into subtitle segments
  segments <- create_subtitle_segments(
    words = words,
    max_chars = max_chars_per_line,
    max_duration = max_duration,
    speaker_col = "speaker_id"
  )

  # Build SRT content
  srt_lines <- character()

  for (i in seq_len(nrow(segments))) {
    # Subtitle number
    srt_lines <- c(srt_lines, as.character(i))

    # Timestamp line
    start_time <- format_srt_timestamp(segments$start[i])
    end_time <- format_srt_timestamp(segments$end[i])
    srt_lines <- c(srt_lines, paste(start_time, "-->", end_time))

    # Subtitle text
    text <- segments$text[i]
    if (include_speakers && !is.na(segments$speaker_id[i])) {
      text <- paste0(speaker_label(segments$speaker_id[i]), " ", text)
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


#' Convert transcript to plain text format
#'
#' Converts a transcript object to plain text with optional timestamps
#' and speaker labels.
#'
#' @param transcript A transcript object returned by elevenlabs_transcribe()
#'   or elevenlabs_get_transcript().
#' @param file_path Character string. Path where the text file should be saved.
#'   If NULL, returns the text content as a string without saving.
#' @param include_timestamps Logical. If TRUE, includes timestamps for each
#'   sentence or paragraph. Default is FALSE.
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
#' transcript <- elevenlabs_transcribe("interview.mp3")
#'
#' # Save as plain text
#' elevenlabs_transcript_to_txt(transcript, "transcript.txt")
#'
#' # With timestamps and speakers
#' elevenlabs_transcript_to_txt(
#'   transcript,
#'   "transcript_detailed.txt",
#'   include_timestamps = TRUE,
#'   include_speakers = TRUE
#' )
#'
#' # Get as string
#' text <- elevenlabs_transcript_to_txt(transcript)
#' }
#'
#' @export
elevenlabs_transcript_to_txt <- function(transcript,
                              file_path = NULL,
                              include_timestamps = FALSE,
                              include_speakers = TRUE,
                              timestamp_format = "time") {

  # Simple case - just use the text field
  if (!include_timestamps && !include_speakers) {
    text_content <- transcript$text
  } else {
    # Need word-level data for timestamps/speakers
    if (is.null(transcript$words) || nrow(transcript$words) == 0) {
      warning("No word-level data available. Using plain text only.")
      text_content <- transcript$text
    } else {
      text_content <- elevenlabs_format_text_with_metadata(
        words = transcript$words,
        include_timestamps = include_timestamps,
        include_speakers = include_speakers,
        timestamp_format = timestamp_format
      )
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


#' Format text with metadata (timestamps and speakers)
#'
#' @keywords internal
elevenlabs_format_text_with_metadata <- function(words,
                                     include_timestamps,
                                     include_speakers,
                                     timestamp_format) {

  # Filter to actual words
  words <- words[words$type == "word", ]

  if (nrow(words) == 0) {
    return("")
  }

  lines <- character()
  current_speaker <- NA
  current_line <- character()

  for (i in seq_len(nrow(words))) {
    word <- words[i, ]

    # Check if speaker changed
    speaker_changed <- include_speakers &&
                      !is.na(word$speaker_id) &&
                      !is.na(current_speaker) &&
                      word$speaker_id != current_speaker

    if (speaker_changed) {
      # Save current line
      if (length(current_line) > 0) {
        lines <- c(lines, paste(current_line, collapse = " "))
        current_line <- character()
      }
    }

    # Add speaker label at start of new speaker section
    if (include_speakers &&
        !is.na(word$speaker_id) &&
        (is.na(current_speaker) || speaker_changed)) {

      current_speaker <- word$speaker_id

      # Add timestamp if requested
      prefix <- speaker_label(current_speaker)
      if (include_timestamps && !is.na(word$start)) {
        if (timestamp_format == "seconds") {
          prefix <- paste0(prefix, " (", round(word$start, 2), "s)")
        } else {
          prefix <- paste0(prefix, " (", format_time_timestamp(word$start), ")")
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
