

#' Load isodat scan
#'
#' @param path file path, can be a vector for loading multiple scan files
#' @param quiet whether to suppress the output of informational messages
#' @export
load_isodat_scan <- function (path, quiet = FALSE) {

  # checks
  if (!all(na <- file.exists(path)))
    stop("Cannot load isodat scans, missing file(s): ", path[!na] %>% paste(collapse = ", "), call. = FALSE)

  # load isodat scan files
  files <- path %>%
    lapply(function(p) {
      tryCatch({
        isoread(p, type = "SCAN", quiet = quiet)
      }, error = function(e){
        if (!quiet) message("Problem: ", e$message)
        return(p)
      })
    })

  # if errors, stop
  if (any(failed <- files %>% sapply(class) == "character")) {
    stop(
      paste0("failed to load the following isodat scan file(s):\n",
             files[failed] %>% paste(collapse = "\n")),
      call. = FALSE
    )
  }

  return(files)
}

#' Combine mass data from loaded scan files (of all types)
#'
#' This function combines all the mass data from scan files
#'
#' @param files list of loaded file
#' @param quiet whether the function should output information messages or be quiet (default is to output)
#' @param format whether to provide the mass data in wide or long format
#'    (if long format, introduces columns "mass" and "intensity")
#' @export
get_mass_data <- function(files, quiet = FALSE, format = c("wide", "long")) {

  # checks
  if (!is.list(files)) files <- list(files)
  if (missing(format)) format <- "wide"
  if (!format %in% c("wide", "long")) stop("unknown format: ", format, call. = F)

  out <-
    files %>%
    # for each file type, pull out the data
    lapply(function(file) {
      if (is(file, "IsodatScanFile")) {
        # isodat scan file
        file$get_mass_data() %>%
          mutate(path = file$filepath, filename = file$filename)
      } else {
        # unknown file type (for future use)
        stop("unsupported file type: ", class(file), call. = FALSE)
      }
    }) %>%
    # combine
    bind_rows()

  # info
  if (!quiet)
    sprintf(
      "INFO: aggregated mass data from %s file(s) with %s masses (%s) and a total number of %s measurements per mass",
      length(files),  grepl("mass", names(out)) %>% sum(),
      sub("mass(\\d+)", "\\1", grep("mass", out  %>% names(), value = TRUE)) %>% paste(collapse = ", "),
      nrow(out)) %>% message()

  # wide or long
  if (format == "long")
    out <- out %>% gather(mass, intensity, starts_with("mass"))

  return(as_data_frame(out))
}
