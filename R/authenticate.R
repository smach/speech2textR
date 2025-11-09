# Package environment to store API credentials
.elevenlabsr_env <- new.env(parent = emptyenv())

#' Authenticate to the ElevenLabs API
#'
#' This function sets up authentication for the ElevenLabs API by storing
#' your API key for use in subsequent API calls. The API key is stored
#' in a package-level environment for the duration of your R session.
#'
#' @param api_key Character string containing your ElevenLabs API key.
#'   If not provided, the function will look for the ELEVENLABS_API_KEY
#'   environment variable.
#' @param validate Logical. If TRUE, validates the API key by making a test
#'   request to the ElevenLabs API. Default is FALSE.
#'
#' @return Invisibly returns TRUE if authentication is successful. Prints
#'   a confirmation message to the console.
#'
#' @details
#' You can obtain an API key from your ElevenLabs account dashboard.
#' It's recommended to store your API key in an environment variable
#' (ELEVENLABS_API_KEY) rather than hardcoding it in your scripts.
#'
#' The API key is stored in memory for the current R session and will
#' need to be set again if you restart R.
#'
#' @examples
#' \dontrun{
#' # Authenticate with API key directly
#' elevenlabs_auth(api_key = "your_api_key_here")
#'
#' # Or set environment variable first
#' Sys.setenv(ELEVENLABS_API_KEY = "your_api_key_here")
#' elevenlabs_auth()
#'
#' # Validate the API key
#' elevenlabs_auth(api_key = "your_api_key_here", validate = TRUE)
#' }
#'
#' @export
elevenlabs_auth <- function(api_key = NULL, validate = FALSE) {
  # Try to get API key from parameter or environment variable
  if (is.null(api_key)) {
    api_key <- Sys.getenv("ELEVENLABS_API_KEY")
    if (api_key == "") {
      stop(
        "No API key provided. Please either:\n",
        "  1. Pass api_key parameter: elevenlabs_auth(api_key = 'your_key')\n",
        "  2. Set ELEVENLABS_API_KEY environment variable: ",
        "Sys.setenv(ELEVENLABS_API_KEY = 'your_key')",
        call. = FALSE
      )
    }
  }

  # Validate API key format (basic check)
  if (!is.character(api_key) || nchar(api_key) == 0) {
    stop("API key must be a non-empty character string.", call. = FALSE)
  }

  # Store API key in package environment
  .elevenlabsr_env$api_key <- api_key

  # Optionally validate the API key by making a test request
  if (validate) {
    tryCatch({
      # Make a simple HEAD request to the API to validate credentials
      response <- httr::GET(
        url = "https://api.elevenlabs.io/v1/user",
        httr::add_headers("xi-api-key" = api_key)
      )

      if (httr::status_code(response) == 401) {
        stop("Invalid API key. Please check your credentials.", call. = FALSE)
      } else if (httr::status_code(response) >= 400) {
        warning(
          "Could not validate API key (HTTP ",
          httr::status_code(response),
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
    message("ElevenLabs API key stored successfully!")
  }

  invisible(TRUE)
}

#' Get stored ElevenLabs API key
#'
#' Internal function to retrieve the stored API key. This is used by
#' other functions in the package to access the authenticated credentials.
#'
#' @return Character string containing the API key.
#' @keywords internal
elevenlabs_get_api_key <- function() {
  api_key <- .elevenlabsr_env$api_key

  if (is.null(api_key)) {
    stop(
      "No API key found. Please authenticate first using elevenlabs_auth().",
      call. = FALSE
    )
  }

  return(api_key)
}

#' Check if authenticated to ElevenLabs API
#'
#' Check whether an API key has been set for the current session.
#'
#' @return Logical value indicating whether an API key is stored.
#'
#' @examples
#' \dontrun{
#' elevenlabs_is_authenticated()
#' }
#'
#' @export
elevenlabs_is_authenticated <- function() {
  !is.null(.elevenlabsr_env$api_key)
}
