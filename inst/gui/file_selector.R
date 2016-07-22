library(shinyBS)

#' fileSelector
#' @param allow_upload whether to allow upload
fileSelectorInput <- function(id, allow_upload = FALSE, upload_label = NULL) {
  ns <- NS(id)
  dialog_tags <-
    tagList(

      uiOutput(ns("path")),
      uiOutput(ns("folder")),
      if (allow_upload) fileInput(ns("upload"), upload_label),

      # reset selection when switching tabs
      tags$script(sprintf(
        " $('#%s').on('click', function(){
        Shiny.onInputChange('%s', '');
        })",
        ns("path"), ns("new_folder"))),

      # enable sub folder selection on double click
      tags$script(sprintf(
        " $('#%s').on('dblclick', function(){
        var obj = $('select#%s')[0];
        Shiny.onInputChange('%s', obj.options[obj.selectedIndex].value);
        })",
        ns("folder"), ns("folder"), ns("new_folder")))
    )

  return(dialog_tags)
}


#' @param root directory
#' @param root_name the root directory name
#' @param folders_sort_desc whether to sort in descending order
#' @param size number of rows in the selection box
fileSelector <- function(input, output, session,
                         root, root_name = basename(root),
                         folders_sort_desc = FALSE,
                         size = 8) {

  # namespace
  ns <- session$ns

  # uploads
  values <- reactiveValues(upload_counter = 1)
  observe({
    # upload (expand zip)
    upload <- input$upload
    if (is.null(upload)) return()
    ext <- stringr::str_match(basename(upload$name), "\\.(\\w+$)")[1,2]
    target <- isolate(get_current_dir())
    if (!is.na(ext) && ext == "zip") upload$datapath %>% unzip(exdir = target)
    else upload$datapath %>% file.copy(to = file.path(target, upload$name))

    # info and update trigger
    counter <- isolate(values$upload_counter)
    sprintf("Upload #%d complete: %s", counter + 1, file.path(target, upload$name)) %>%
      message()
    values$upload_counter <- counter + 1
  })

  # current base folder
  get_current_dir <- reactive({

    # refresh whenever upload increases
    values$upload_counter

    # base path
    path <- input$path
    if (is.null(input$path)) {
      path <- root
    }

    # response to javascript trigger of a subfolder selection
    if (!is.null(input$new_folder) && input$new_folder != "" &&
        dirname(input$new_folder) == path && # make sure it's a subdirectory
        dir.exists(input$new_folder) # make sure it's a directory
      ) path <- input$new_folder

    # safety checks
    stopifnot(file.exists(path))
    if (!R.utils::isAbsolutePath(path)) stop("not an absolute path: ", path)
    return(path)
  })

  # current folder list
  get_contents_list <- reactivePoll(
    1000, session,
    checkFunc = function() list.files(isolate(get_current_dir()), recursive = F),
    valueFunc = function() {

      # get folder and file contnet
      folders <-  list.dirs(get_current_dir(), recursive = F, full.names = T)
      files <- setdiff(list.files(get_current_dir(), recursive = F, full.names = T), folders)

      # sort in descending order if asked for
      if (folders_sort_desc) {
        folders <- rev(folders)
        files <- rev(files)
      }

      # combine
      setNames(
        c(folders, files),
        c(sprintf("[ %s ]", folders %>% sapply(basename)), files %>% sapply(basename))
      )
    })

  # generate path tabs
  output$path <- renderUI({
    tmp_path <- get_current_dir()
    parents <- list(id = ns("path"), selected = tmp_path)
    while (tmp_path != dirname(root)){
      if (tmp_path == root)
        parent <- root_name
      else
        parent <- basename(tmp_path)
      parents <- c(parents, list(tabPanel(parent, value = tmp_path)))
      tmp_path <- dirname(tmp_path)
    }
    do.call(tabsetPanel, args = parents[length(parents):1])
  })

  # generate folders listing
  output$folder <- renderUI({
    selectInput(ns("folder"), NULL, width = "100%",
                size = size, selectize = F, multiple = T,# selected = NULL,
                get_contents_list())
  })

  # return both the current path and the selected folder contents
  list(
    path = reactive(input$path),
    selection = reactive(input$folder)
  )
}

