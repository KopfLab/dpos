#' Load tuning scans
#'
#' Supports loading both isodat scan (.scn) and range scans (.bff), calling
#' load_isodat_scan and load_range_scan internally.
#'
#' @param path file path, can be a vector for loading multiple files
#' (including different file types and folders that will be read with all compatible
#' file types)
#' @return list of scan objects
#' @note folder loading not implemented yet
load_scans <- function(paths, quiet = FALSE) {

  # load scans
  files <- paths %>%
    lapply(function(p) {
      tryCatch({
        ext <- stringr::str_match(basename(p), "\\.(\\w+$)")[1,2]
        if (!is.na(ext) && ext == "scn") {
          return(load_isodat_scan(p, quiet = quiet))
        } else if (!is.na(ext) && ext == "bff") {
          return(load_range_scan(p, quiet = quiet))
        } else {
          stop("unknown extension: ", ext, call. = FALSE)
        }
      }, error = function(e){
        if (!quiet) message("Problem: ", e$message)
        return(p)
      })
    })

  # if errors, stop
  if (any(failed <- files %>% sapply(class) == "character")) {
    stop(
      paste0("failed to load the following scan file(s):\n",
             files[failed] %>% paste(collapse = "\n")),
      call. = FALSE
    )
  }

  return(files)
}


#' Load range scan
#'
#' @param path file path
#' @param quiet whether to suppress the output of informational messages
#' @family tuning scans
#' @export
load_range_scan <- function (path, quiet = FALSE) {
  # checks
  if(!file.exists(path))
    stop("Cannot load range scan, missing file: ", path, call. = FALSE)

  # load scan file
  if (!quiet) sprintf("Reading file %s", path) %>% message()

  scan_file <- readLines(path) %>%
    # find each value
    str_match_all("([^=,]+)=(([0-9.]+)|[^,]+)(,|$)") %>%
    # pull out values broken out by numbers and characters
    lapply(
      function(entry) {
        numbers <- entry[,4] != ""
        c(as.list(entry[,3][!numbers]), as.list(as.numeric(entry[,3][numbers]))) %>%
          setNames(c(entry[,2][!numbers], entry[,2][numbers]))
      }
    ) %>%
    # make a data frame out it all
    bind_rows() %>%
    # wrap in a structure for easy identification and plotting
    structure(
      filepath = dirname(path),
      filename = basename(path),
      class = "RangeScanFile"
    )
  names(scan_file) <- sub(".mV", "", names(scan_file), fixed = T)
  return(scan_file)
}

#' Load isodat scan
#'
#' @param path file path
#' @param quiet whether to suppress the output of informational messages
#' @family tuning scans
#' @export
load_isodat_scan <- function (path, quiet = FALSE) {

  # checks
  if(!file.exists(path))
    stop("Cannot load isodat scan, missing file: ", path, call. = FALSE)

  # load isodat scan files
  return(isoread(path, type = "SCAN", quiet = quiet))
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
  if (class(files) %in% c("IsodatScanFile", "RangeScanFile")) files <- list(files)
  if (missing(format)) format <- "wide"
  if (!format %in% c("wide", "long")) stop("unknown format: ", format, call. = F)

  out <-
    files %>%
    # for each file type, pull out the data
    lapply(function(file) {
      if (is(file, "IsodatScanFile")) {
        # isodat scan file
        file$get_mass_data() %>%
          mutate(path = file$filepath, filename = file$filename,
                 scan_type = "HighVoltage")  %>%
          rename(scan_value = step) # FIXME: should this be changed in isoread instead?
      } else if (is(file, "RangeScanFile")) {
        file %>% as_data_frame() %>%
          mutate(path = attr(file, "filepath"), filename = attr(file, "filename"))
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
