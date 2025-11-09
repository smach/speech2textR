#' Edit .srt Caption File with Standalone Node.js App
#'
#' Launch a standalone subtitle editor with full sync to video player.
#' This uses a Node.js server with proper HTTP range request support.
#'
#' Requires Node.js to be installed on your system.
#'
#' @param video_file Character string. Path to the video file (e.g., .mp4, .mov, .webm)
#' @param srt_file Character string. Full path to an .srt caption file
#'
#' @return Launches the Node.js server. Press Ctrl+C in the console to stop.
#'
#' @examples
#' \dontrun{
#' # Edit subtitles for a video
#' edit_subtitles_standalone("my_video.mp4", "my_subtitles.srt")
#' }
#'
#' @export
edit_subtitles_standalone <- function(video_file, srt_file) {

  # Check if files exist
  if (!file.exists(video_file)) {
    stop("Video file not found: ", video_file)
  }
  if (!file.exists(srt_file)) {
    stop("SRT file not found: ", srt_file)
  }

  # Check if Node.js is available
  node_check <- system("node --version", ignore.stdout = TRUE, ignore.stderr = TRUE)
  if (node_check != 0) {
    stop("Node.js is required for the standalone subtitle editor.\n",
         "Please install Node.js from https://nodejs.org/ and try again.\n",
         "Alternatively, use edit_subtitles() for the Shiny version (requires Python 3).")
  }

  # Get absolute paths
  video_file <- normalizePath(video_file)
  srt_file <- normalizePath(srt_file)

  # Find the subtitle-editor directory in the installed package
  editor_dir <- system.file("subtitle-editor", package = "elevenlabsr")

  if (editor_dir == "" || !dir.exists(editor_dir)) {
    stop("Subtitle editor not found in package installation.\n",
         "Please reinstall the package or check the subtitle-editor/ directory.")
  }

  server_path <- file.path(editor_dir, "server.js")

  if (!file.exists(server_path)) {
    stop("server.js not found at: ", server_path)
  }

  # Build the command
  cmd <- sprintf('node "%s" "%s" "%s"', server_path, video_file, srt_file)

  message("\n=====================================================")
  message("Starting Standalone Subtitle Caption File Editor")
  message("=====================================================")
  message("Video: ", basename(video_file))
  message("Subtitles: ", basename(srt_file))
  message("\nThe editor will open in your browser automatically.")
  message("Press Ctrl+C in this console to stop the server.")
  message("=====================================================\n")

  # Run the server (blocking)
  system(cmd)

  return(invisible(NULL))
}
