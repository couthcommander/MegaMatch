library(shiny)

shinyUI(
  navbarPage("MegaMatch", id = 'meganav',
  tabPanel("Covariate Matrix",
    verticalLayout(
      tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "mm.css")),
      tags$div(id = 'getCov',
        h4("Upload your covariate matrix:"),
        fileInput('fh_covmat', 'Choose CSV File', accept = c('text/csv', 'text/comma-separated-values', 'text/plain', 'application/zip')),
        tags$div(class = "info",
"The covariate matrix expects the first row to contain header information.", tags$br(),
"If you upload a covariate matrix, quality of match diagnostics will be generated.", tags$br(),
"Files are expected to be CSV files. You may use a zip file instead if the only content of the file is a CSV file."
        )
      ),
      htmlOutput("previewTbl"),
      shinyjs::hidden(actionButton('subCM', 'Submit')),
      shinyjs::hidden(actionButton('resetOpts', 'Modify Options')),
      shinyjs::hidden(htmlOutput("covOut"))
    )
  ),
  tabPanel("Distance Matrix",
    verticalLayout(
      tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "mm.css")),
      tags$div(id = 'getDM',
        h4("Upload your distance matrix:"),
        fileInput('fh_distmat', 'Choose CSV File', accept = c('text/csv', 'text/comma-separated-values', 'text/plain', 'application/zip')),
        tags$div(class = "info", "Files are expected to be CSV files. You may use a zip file instead if the only content of the file is a CSV file.")
      ),
      htmlOutput("dmInfo")
    )
  ),
  tabPanel("Randomize Pairs",
    verticalLayout(
      tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "mm.css")),
      tags$div(id = 'getRP',
        h4("Upload your full pairs:"),
        fileInput('fh_pairs', 'Choose CSV File', accept = c('text/csv', 'text/comma-separated-values', 'text/plain', 'application/zip')),
        tags$div(class = "info", "Files are expected to be CSV files. You may use a zip file instead if the only content of the file is a CSV file.")
      ),
      tags$label('Randomization seed', title = 'Specify a seed for the randomization'),
      numericInput('pairSeed', '', value = NA, min = 1, step = 1, width='75px'),
      htmlOutput("rpInfo")
    )
  ),
  tabPanel("Quality of Matches",
    tags$div(
tags$p('Quality of matches show how well matched pairs differ. For each variable the average distance is generated. Each item in a pair is assigned a group and after 10,000 iterations the quantile of these average distances is returned.'),
tags$p('This is useful for determining the effectiveness of your weights (when generating a distance matrix). Weighting a variable more will lower the average distance, but it could penalize the distance of the other variables.')
    ),
    htmlOutput("qmInfo")
  ),
  tabPanel("Download Data",
    tags$div('Data can be downloaded through individual tabs, as well as below.'),
    htmlOutput("downloader")
  ),
  tabPanel("Contact Us",
    tags$div('If you have any questions or comments, please send an email to'),
    tags$br('cole.beck@gmail.com')
  ),
  header = shinyjs::useShinyjs(),
  footer = tags$div(id = 'footer', 'Copyright 2011-2022 - Vanderbilt University')
))
