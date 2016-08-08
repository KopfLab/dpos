
# main settings
SIDEBAR_WIDTH = 200 #px

# Define UI that plots the isotope label enrichment
ui <- dashboardPage(

  # SKIN ----
  skin = "red",

  # HEADER ----
  dashboardHeader(title = "DPOS Toolbox"),

  # SIDEBAR ----
  sidebarMenu(
    menuItem("Welcome", tabName = "welcome", icon = icon("info")),
    menuItem(
      "Tuning", tabName = "tuning", icon = icon("music"),
      menuSubItem("Select files", tabName = "tuning_files", icon = icon("files-o"), selected = TRUE),
      menuSubItem("Analysis", tabName = "tuning_analyze", icon = icon("bar-chart"))),
    menuItem("Data", tabName = "data", icon = icon("database")),

    # STYLESHEET ----
    tags$head(
      tags$style(HTML(".shiny-output-error-validation { color: red; font-size: 16px; }")),
      tags$style(type = "text/css", ".sidebar {height:1300px}") # FIXME: make this dynamically long enough
    ),


    # USE SHINY JS ---
    shinyjs::useShinyjs()

  ) %>% dashboardSidebar(width = SIDEBAR_WIDTH),

  # BODY ----
  tabItems(

    # WELCOME ----
    tabItem(tabName = "welcome",
            h1("Welcome to the DPOS Toolbox")),

    # TUNING: File selection ----
    tabItem(
      tabName = "tuning_files",
      column(
        width = 12,
        fileSelectorInput(
          id = "tuning_files_local", allow_upload = TRUE,
          upload_label = 'Upload tuning files (individual or .zip archives)')) %>%
        fluidRow(),

      # TUNING: File preview box ----
      box(
        plotDownloadLink(id = "tuning_file_download"),

        tabsetPanel(id = "tuning_file_plot_tabs",
          tabPanel("Static Plot", value = "gg",
                   plotOutput("tuning_file_plot", height="500px", width = "100%")),
          tabPanel("Interactive Plot", value = "i",
                   plotlyOutput("tuning_file_iplot", height="500px", width = "100%"))
        ),

        title = "Tuning file quick view",
        status = "info", solidHeader = TRUE, width = 12),

      # TUNING: File preview code ----
      box(
        aceEditor("tuning_plot_code", mode = "r",
                  theme="ambiance", readOnly = TRUE,
                  height = "200px"),
        title = "Code preview",
        status = "success", solidHeader = TRUE, width = 12)
    ), # / tabItem

    # TUNING: Analysis ----
    tabItem(
      tabName = "tuning_analyze", h2("hello")),


    # DATA ----

    tabItem(
      tabName = "data",
      column(
        width = 12,
        fileSelectorInput(
          id = "data_files_local", allow_upload = TRUE,
          upload_label = 'Upload tuning files (individual or .zip archives)')) %>%
        fluidRow(),

      # DATA: File preview code ----
      box(
        aceEditor("data_plot_code", mode = "r",
                  theme="ambiance", readOnly = TRUE,
                  height = "200px"),
        title = "Code preview",
        status = "success", solidHeader = TRUE, width = 12),

      # DATA: File preview box ----
      box(
        plotDownloadLink(id = "data_file_download"),

        tabsetPanel(id = "data_file_plot_tabs",
                    tabPanel("Static Plot", value = "gg",
                             plotOutput("data_file_plot", height="500px", width = "100%")),
                    tabPanel("Interactive Plot", value = "i",
                             plotlyOutput("data_file_iplot", height="500px", width = "100%"))
        ),

        title = "Data file quick view",
        status = "info", solidHeader = TRUE, width = 12)


    ) # / tabItem

  ) %>% dashboardBody()

)
