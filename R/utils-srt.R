# Subtitle and timestamp helpers shared by all providers.
#
# These were originally copy-pasted into each provider file, which meant a
# timestamp-rounding bug had to be fixed three times. Providers differ only in
# what they call the speaker column, so that is passed in rather than forked.


#' Format seconds as an SRT timestamp
#'
#' Produces the \code{HH:MM:SS,mmm} form the SRT format requires.
#'
#' @param seconds Numeric. Offset in seconds. NA yields "00:00:00,000".
#'
#' @return Character string.
#'
#' @details
#' Rounds to whole milliseconds first and then splits the total, rather than
#' rounding the fractional part on its own. Doing it the other way lets a value
#' like 1.9996 round its milliseconds up to 1000 and print "00:00:01,1000",
#' which is a four-digit field and not valid SRT.
#'
#' @keywords internal
format_srt_timestamp <- function(seconds) {
  if (is.na(seconds)) {
    return("00:00:00,000")
  }

  total_ms <- round(seconds * 1000)

  sprintf(
    "%02d:%02d:%02d,%03d",
    total_ms %/% 3600000,
    (total_ms %% 3600000) %/% 60000,
    (total_ms %% 60000) %/% 1000,
    total_ms %% 1000
  )
}


#' Format seconds as a readable timestamp
#'
#' Produces \code{MM:SS}, or \code{HH:MM:SS} once the offset passes an hour.
#'
#' @param seconds Numeric. Offset in seconds. NA yields "00:00:00".
#'
#' @return Character string.
#' @keywords internal
format_time_timestamp <- function(seconds) {
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


#' Format a speaker label for display
#'
#' The "Speaker" prefix is there to make a bare identifier readable, so it is
#' only added when the id needs it. AssemblyAI numbers its speakers "A", "B",
#' which reads badly on its own. ElevenLabs and Mistral return "speaker_0", and
#' ElevenLabs returns "agent" or "customer" when speaker roles are detected —
#' those already read fine, and prefixing them gives "[Speaker speaker_0]".
#'
#' @param speaker_id Character string. The id from the API.
#'
#' @return Character string, e.g. "[speaker_0]", "[agent]", or "[Speaker A]".
#' @keywords internal
speaker_label <- function(speaker_id) {
  bare <- grepl("^[0-9]+$", speaker_id) || grepl("^[A-Za-z]$", speaker_id)

  if (bare) {
    paste0("[Speaker ", speaker_id, "]")
  } else {
    paste0("[", speaker_id, "]")
  }
}


#' Group words into subtitle segments
#'
#' Walks the words in order and starts a new subtitle whenever adding the next
#' word would pass the character or duration limit, or whenever the speaker
#' changes. Words with no timing information are skipped.
#'
#' @param words Data frame with \code{text}, \code{start}, \code{end}, and a
#'   speaker column named by \code{speaker_col}.
#' @param max_chars Integer. Maximum characters in one subtitle.
#' @param max_duration Numeric. Maximum seconds one subtitle may span.
#' @param speaker_col Character string. Name of the speaker column: providers
#'   variously call it "speaker_id" or "speaker". Default "speaker_id".
#'
#' @return Data frame with \code{text}, \code{start}, \code{end}, and a speaker
#'   column named to match \code{speaker_col}. NULL if no words had timings.
#' @keywords internal
create_subtitle_segments <- function(words, max_chars, max_duration,
                                     speaker_col = "speaker_id") {

  speakers <- words[[speaker_col]]

  segments <- list()
  current <- list(text = character(), start = NA, end = NA, speaker = NA)

  for (i in seq_len(nrow(words))) {
    if (is.na(words$start[i]) || is.na(words$end[i])) {
      next
    }

    # First word of a segment sets its start time and speaker
    if (is.na(current$start)) {
      current$start <- words$start[i]
      current$speaker <- speakers[i]
    }

    new_text <- paste(c(current$text, words$text[i]), collapse = " ")
    new_duration <- words$end[i] - current$start

    speaker_changed <- !is.na(speakers[i]) &&
      !is.na(current$speaker) &&
      speakers[i] != current$speaker

    if (nchar(new_text) > max_chars ||
        new_duration > max_duration ||
        speaker_changed) {

      # Close out the segment in progress, if it has anything in it
      if (length(current$text) > 0) {
        segments[[length(segments) + 1]] <- current
      }

      current <- list(
        text = words$text[i],
        start = words$start[i],
        end = words$end[i],
        speaker = speakers[i]
      )
    } else {
      current$text <- c(current$text, words$text[i])
      current$end <- words$end[i]
    }
  }

  if (length(current$text) > 0) {
    segments[[length(segments) + 1]] <- current
  }

  if (length(segments) == 0) {
    return(NULL)
  }

  out <- data.frame(
    text = vapply(segments, function(s) paste(s$text, collapse = " "), character(1)),
    start = vapply(segments, function(s) as.numeric(s$start), numeric(1)),
    end = vapply(segments, function(s) as.numeric(s$end), numeric(1)),
    speaker = vapply(segments, function(s) as.character(s$speaker), character(1)),
    stringsAsFactors = FALSE
  )
  names(out)[4] <- speaker_col

  out
}
