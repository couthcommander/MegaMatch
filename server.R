library(shiny)
library(shinyjs)
library(nbpMatching)

shinyServer(function(input, output, session) {
  observeEvent(input$run_dm, {
    updateNavbarPage(session, 'meganav', 'Distance Matrix')
  })
  observeEvent(input$run_rp, {
    updateNavbarPage(session, 'meganav', 'Randomize Pairs')
  })
  observeEvent(input$run_qm, {
    updateNavbarPage(session, 'meganav', 'Quality of Matches')
  })
  v <- reactiveValues(covFile = NULL, covArgs = NULL, covColumns = NULL, td = tempdir(),
    dm = NULL, dmFile = NULL, matchFile = NULL, redmatFile = NULL, matchDat = NULL, grpFile = NULL, finalFile = NULL
  )
  output$previewTbl <- renderUI({
    shinyjs::hide('subCM')
    inFile <- input$fh_covmat
    out <- 'no data available'
    ndiscard <- 0
    nrecords <- 5
    headerrow <- FALSE
    badout <- NULL
    covmat <- NULL
    if(!is.null(inFile)) {
      fp <- loadFile(inFile, v$td)
      if(file.access(fp) != -1) {
        nrecords <- as.numeric(sub("[ ]+.*$", "", (system(paste("wc -l ", fp, sep=""), intern=TRUE))))
        numlines <- min(nrecords, 11)
        preview <- scan(fp, sep=',', nlines=11, what='character', quiet=TRUE, na.strings=c("NA",""))
        # save path of covariate file
        v$covFile <- fp
        firstlines <- matrix(preview, nrow = numlines, byrow = TRUE)
        # if there are character values, the first line is a header
        if(any(is.na(suppressWarnings(as.numeric(firstlines[1,]))))) {
          headerrow <- TRUE
          nrecords <- nrecords - 1
          covmat <- data.frame(firstlines[-1,], stringsAsFactors = FALSE)
          names(covmat) = firstlines[1,]
        } else {
          headerrow <- FALSE
          covmat <- data.frame(firstlines, stringsAsFactors = FALSE)
        }
        out <- preview_help(covmat, ndiscard, nrecords, headerrow)
        # subset the data to only the columns with numeric values
        badcol <- which(sapply(covmat, FUN=function(x) { length(setdiff(x, suppressWarnings(as.numeric(x))))>0 }))
        if(length(badcol)) {
          badout <- paste(sprintf('Column %s will be dropped because it contains non-numeric characters', badcol), collapse = '<br/>\n')
        }
        # if all values in a column are the same, need to remove the column
        badcol <- which(sapply(covmat, FUN=function(x) { length(unique(x))==1 }))
        if(length(badcol)) {
          bad2 <- sprintf('Column %s will likely be dropped because all values are the same', badcol)
          badout <- paste(c(badout, bad2), collapse = "<br/>\n")
        }
        # add submit button
        shinyjs::show('subCM')
      } else {
        out <- 'there was a problem'
      }
      if(is.null(badout)) badout <- ''
      out1 <- sprintf("<div id='file_head' style='width:1000px;height:500px;overflow:auto;border: 2px solid;'>
%s
</div>
<div class='warn'>%s</div>", out, badout)
    } else {
      out1 <- "<h2>waiting for file upload...</h2>"
    }
    HTML(out1)
#      isolate(jnk)
  })
  output$dmInfo <- renderUI({
    inFile <- input$fh_distmat
    # alternative is to select current DM
    if(!is.null(inFile)) {
      fp <- loadFile(inFile, v$td)
      if(file.access(fp) != -1) {
        # save path of distancematrix file
        v$dmFile <- fp
        dat <- read.csv(fp, header = FALSE)
        # convert to numeric matrix and save header as `dimnames`
        if((nrow(dat) - 1) == ncol(dat)) {
          dim_n <- dat[1,]
          dat <- sapply(dat[-1,], as.numeric)
          dimnames(dat) <- list(dim_n, dim_n)
        }
      } else {
        return(tags$h2('there was a problem'))
      }
      baseFileName <- sub('_dm[.]csv$', '', basename(v$dmFile))
      # create distancematrix
      v$dm <- nbpMatching::distancematrix(dat)
      # create matched pairs
      v$matchDat <- nbpMatching::nonbimatch(v$dm)
      # assign "treatment.grp" to each pair
      seed <- input$pairSeed
      if(is.na(seed) || length(seed)==0) seed <- 68
      ag <- nbpMatching::assign.grp(v$matchDat, seed)
      # save file names
      v$matchFile <- file.path(v$td, sprintf('full_%s.csv', baseFileName))
      v$redmatFile <- file.path(v$td, sprintf('reduced_%s.csv', baseFileName))
      v$grpFile <- file.path(v$td, sprintf('randomized_%s.csv', baseFileName))
      # create downloadable data sets
      write.csv(v$matchDat$matches, file = v$matchFile, row.names = FALSE)
      write.csv(v$matchDat$halves, file = v$redmatFile, row.names = FALSE)
      write.csv(ag, file = v$grpFile, row.names = FALSE)
    }
    if(is.null(v$dm)) {
      return(tags$h2('waiting for file upload...'))
    }
    out1 <- tags$div('Optimal pairs have been generated with your distance matrix. You may now randomize these pairs into groups',
      tags$br(),
      'by clicking ',
      actionLink('run_rp', 'Randomize Pairs'),
      '. You may upload your matching pair matrix ',
      downloadLink("downloadRM", "(download)"),
      ' from there at any time.'
    )
    if(!is.null(v$covFile)) {
      out1 <- paste0(out1, tags$div('You may also view ', actionLink('run_qm', 'Quality of Matches'), '.'))
    }
    out2 <- tags$div(
      'Total distance: ', round(v$matchDat$total, 2), tags$br(),
      'Average distance: ', round(v$matchDat$mean, 2)
    )
    out3 <- viewMatchTable(v$matchDat)
    out <- paste0(out1, tags$br(), out2, tags$br(), out3)
    shinyjs::hide('getDM')
    HTML(out)
  })
  output$rpInfo <- renderUI({
    inFile <- input$fh_pairs
    pairs <- NULL
    if(!is.null(inFile)) {
      fp <- loadFile(inFile, v$td)
      if(file.access(fp) != -1) {
        # save path of pair file
        v$matchFile <- fp
      } else {
        return(tags$h2('there was a problem'))
      }
      # read the CSV file into pairs
      pairs <- read.csv(v$matchFile)
      # there should be either 3 or 5 columns in the pairs file
      if(!(ncol(pairs) %in% c(3,5))) {
        return(tags$h2('There was a problem loading the pairs file'))
      }
    }
    if(is.null(v$matchFile)) {
      return(tags$h2('waiting for file upload...'))
    }
    shinyjs::hide('getRP')
    if(is.null(pairs)) {
      pairs <- v$matchDat$matches
    }

    seed <- input$pairSeed
    if(is.na(seed) || length(seed)==0) seed <- 68
    ag <- nbpMatching::assign.grp(pairs, seed)
    v$grpFile <- file.path(v$td, sub('full_', 'randomized_', basename(v$matchFile)))
    write.csv(ag, file = v$grpFile, row.names = FALSE)

    out1 <- viewAssignment(ag)
    rm_dl <- paste0(tags$div('Download ', downloadLink("downloadAG", "treatment group assignments")), tags$br())
    if(!is.null(v$covFile)) {
      dat <- read.csv(v$covFile)
      nr <- min(nrow(dat), nrow(ag))
      dat[seq(nr), 'treatment.grp'] <- ag[seq(nr), 'treatment.grp']
      ffn <- file.path(v$td, sub('full_', 'grouped_', basename(v$matchFile)))
      write.csv(dat, file = ffn, row.names = FALSE)
      v$finalFile <- ffn
      rm_dl <- paste0(rm_dl, tags$div('Download ', downloadLink("downloadFD", "covariate data with assignments")), tags$br())
    }
    out <- paste0(rm_dl, tags$br(), out1)
    HTML(out)
  })
  output$qmInfo <- renderUI({
    if(is.null(v$covFile) || is.null(v$matchDat)) {
      return(tags$h3('This is unavailable unless a covariate matrix and full pairing are provided.'))
    }
    covar <- read.csv(v$covFile)
    # remove non-numeric columns
    # consider ID column as well
    ignorecols <- which(sapply(covar, FUN=function(x) { length(setdiff(x, suppressWarnings(as.numeric(x)))) > 0 }))
    if(length(ignorecols)) {
      covar <- covar[,-ignorecols]
    }
    match <- v$matchDat$matches
    qual <- nbpMatching::qom(covar, match, use.se = FALSE)
    out <- viewQuality(qual)
    HTML(as.character(out))
  })
  output$downloader <- renderUI({
    if(is.null(input$fh_covmat)) NULL
    d1 <- 'distance matrix'
    d2 <- 'matched pairs'
    d2a <- 'reduced matched pairs'
    d3 <- 'treatment group assignments'
    d4 <- 'covariate data with assignments'
    if(!is.null(v$dmFile)) d1 <- downloadLink("downloadDMalt", d1)
    if(!is.null(v$matchFile)) d2 <- downloadLink("downloadRMalt", d2)
    if(!is.null(v$redmatFile)) d2a <- downloadLink("downloadRdMalt", d2a)
    if(!is.null(v$grpFile)) d3 <- downloadLink("downloadAGalt", d3)
    if(!is.null(v$finalFile)) d4 <- downloadLink("downloadFDalt", d4)
    tags$div(
      tags$div('Download:', d1),
      tags$div('Download:', d2),
      tags$div('Download:', d2a),
      tags$div('Download:', d3),
      tags$div('Download:', d4),
    )
  })
# user clicks "submit" to read covariate data -- generate distancematrix
  observeEvent(input$subCM, {
    shinyjs::hide('getCov')
    shinyjs::hide('previewTbl')
    shinyjs::hide('subCM')
    shinyjs::show('resetOpts')
    shinyjs::show('covOut')
    dat <- read.csv(v$covFile)
    # number of covariates
    nc <- length(grep('^weight_', names(input)))
    nd <- input[['ndiscard']]
    mw <- input[['missingness']]
    isId <- vapply(seq(nc), function(i) input[[sprintf('idcol_%s', i)]], logical(1))
    idcol <- which(isId)
    if(length(idcol) <= 1) {
      isRank <- vapply(seq(nc), function(i) input[[sprintf('rank_%s', i)]], logical(1))
      isPrev <- vapply(seq(nc), function(i) input[[sprintf('prev_%s', i)]], logical(1))
      covWgt <- vapply(seq(nc), function(i) input[[sprintf('weight_%s', i)]], numeric(1))
      args <- list(idcol = idcol, rankcols = which(isRank), weights = covWgt, prevent = which(isPrev), missing.weight = mw, ndiscard = nd)
      v$covArgs <- args
      args$covariate <- dat
      v$covColumns <- names(dat)
      dist.info <- tryCatch(do.call(nbpMatching::gendistance, args), error = function(e) { NULL })
      if(!is.null(dist.info)) {
        baseFileName <- sub('[.]csv$', '', basename(v$covFile))
        # create distancematrix
        v$dm <- nbpMatching::distancematrix(dist.info$dist)
        # create matched pairs
        v$matchDat <- nbpMatching::nonbimatch(v$dm)
        # assign "treatment.grp" to each pair
        seed <- input$pairSeed
        if(is.na(seed) || length(seed)==0) seed <- 68
        ag <- nbpMatching::assign.grp(v$matchDat, seed)
        # add "treatment.grp" to covariate data
        nr <- min(nrow(dat), nrow(ag))
        dat[seq(nr), 'treatment.grp'] <- ag[seq(nr), 'treatment.grp']
        # save file names
        v$dmFile <- file.path(v$td, paste0(baseFileName, '_dm.csv'))
        v$matchFile <- file.path(v$td, sprintf('full_%s.csv', baseFileName))
        v$redmatFile <- file.path(v$td, sprintf('reduced_%s.csv', baseFileName))
        v$grpFile <- file.path(v$td, sprintf('randomized_%s.csv', baseFileName))
        v$finalFile <- file.path(v$td, sprintf('grouped_%s.csv', baseFileName))
        # create downloadable data sets
        write.csv(v$dm, file = v$dmFile, row.names = FALSE)
        write.csv(v$matchDat$matches, file = v$matchFile, row.names = FALSE)
        write.csv(v$matchDat$halves, file = v$redmatFile, row.names = FALSE)
        write.csv(ag, file = v$grpFile, row.names = FALSE)
        write.csv(dat, file = v$finalFile, row.names = FALSE)
      }
    }
  })
  observeEvent(input$resetOpts, {
    shinyjs::show('previewTbl')
    shinyjs::show('subCM')
    shinyjs::hide('resetOpts')
    shinyjs::hide('covOut')
  })
  output$covOut <- renderUI({
    if(is.null(v$dm)) {
      out <- tags$h2('Something went wrong - please start over')
    } else {
      out1 <- wrapup.covariate(v)
      out2 <- tags$div('A distance matrix has been generated with your covariate matrix. You may now submit your distance matrix through the matching',
        tags$br(),
        'program by clicking the ',
        actionLink('run_dm', 'Distance Matrix'),
        ' tab. If you download your distance matrix, you may also upload it from the same tab.'
      )
      out <- paste0(out1, out2)
    }
    HTML(out)
  })
  output$downloadDM <- downloadHandler(
    filename = function() basename(v$dmFile),
    content = function(file) file.copy(v$dmFile, file)
  )
  output$downloadRM <- downloadHandler(
    filename = function() basename(v$matchFile),
    content = function(file) file.copy(v$matchFile, file)
  )
  output$downloadAG <- downloadHandler(
    filename = function() basename(v$grpFile),
    content = function(file) file.copy(v$grpFile, file)
  )
  output$downloadFD <- downloadHandler(
    filename = function() basename(v$finalFile),
    content = function(file) file.copy(v$finalFile, file)
  )
  output$downloadDMalt <- downloadHandler(
    filename = function() basename(v$dmFile),
    content = function(file) file.copy(v$dmFile, file)
  )
  output$downloadRMalt <- downloadHandler(
    filename = function() basename(v$matchFile),
    content = function(file) file.copy(v$matchFile, file)
  )
  output$downloadRdMalt <- downloadHandler(
    filename = function() basename(v$redmatFile),
    content = function(file) file.copy(v$redmatFile, file)
  )
  output$downloadAGalt <- downloadHandler(
    filename = function() basename(v$grpFile),
    content = function(file) file.copy(v$grpFile, file)
  )
  output$downloadFDalt <- downloadHandler(
    filename = function() basename(v$finalFile),
    content = function(file) file.copy(v$finalFile, file)
  )
})

loadFile <- function(mf, td = tempdir()) {
  if(mf$type == 'application/zip') {
    zd <- file.path(td, 'zip')
    unzip(mf$datapath, exdir = zd)
    a <- list.files(td, '[.]csv$', full.names = TRUE)
    if(length(a) == 1) {
      fn <- basename(a)
      fp <- file.path(td, fn)
      if(file.exists(a)) file.rename(a, fp)
    }
    unlink(zd, recursive = TRUE)
  } else if(mf$type %in% c('text/csv', 'text/comma-separated-values', 'text/plain')) {
    fn <- mf$name
    fp <- file.path(td, fn)
    if(file.exists(mf$datapath)) file.rename(mf$datapath, fp)
  }
  fp
}

preview_help <- function(covmat, d, r, header) {
  if(is.null(covmat)) {
    covmat <- matrix(seq(25), nrow = 5)
  }
  nc <- ncol(covmat)
  nr <- nrow(covmat)
  colseq <- seq(nc)
  if(is.null(colnames(covmat))) {
    colnames(covmat) <- colseq
  }
  pdf <- data.frame(cbind('', '', covmat), stringsAsFactors = FALSE)
  ldf <- lapply(seq(nr), function(j) {
    do.call(tags$tr, lapply(seq(ncol(pdf)), function(i) tags$td(pdf[j,i])))
  })
  colnames <- colnames(covmat)
  colwidth <- nchar(colnames) * 15
  colwidth[colwidth < 75] <- 75

  id.help <- "Select the column with the ID variable. The IDs may be text or numeric. If no ID column is specified, rows will be identified by row number starting with the number 1."
  rank.help <- "Check any columns with variables you wish to transform into ranks. For example, suppose you wished to match on a highly skewed variable such as income. Matching on rank(income) may be preferable."
  prevent.help <- "Select a column or columns with variables identifying matches to be prevented. These variables must be numeric. Any two units that have identical values for a prevent matches variable will have the distance between them set to the maximum distance. Example 1: any units may be matched except for rows 1 and 4. Solution: create a prevent matches variable 1, 2, 3, 1, 5, 6, 7, .... Example 2: match only treated units with controls units, i.e. perform optimal bipartite matching. Solution: use an indicator variable for treatment as a prevent matches variable."
  weight.help <- "Provide weights to reweight the Mahalanobis distance between units. Greater weights have a proportionally greater impact on the overall distance between units. Variables for which higher quality matching is desired should get larger weights. Weights may range from 0 to infinity, with weights from 0 to 10 having performed well in the tests run to date."
  missingness.help <- "Weight to be given to allow for informative missingness. Indicator variables for the missingness in variables with missing values are automatically created and the missing values are automatically filled in with single imputation via the transcan function in R. A larger weight will emphasize matching units with the same missingness patterns with each other. A smaller weight will emphasize matching units with similar imputed or observed values with each other."
  discard.help <- 'Select the number of units to discard; usually zero. Suppose you have 25 potential units for a study, but only need 20. To optimally select the 5 units to drop, upload the covariate matrix for all 25 units and enter 5 for the number of units to discard. The distance matrix will be supplemented with 5 "phantom" units. The phantom units will optimally match to the 5 real units that have the lowest quality matches. When a dataset has an odd number of units and the number of units to discard is set to zero, one phantom will be created to optimally select the one unit that cannot be matched.'

  r1attr <- c(list(list(width = 150), list(width = 20)), lapply(colwidth, function(i) list(width = i, align = 'center')))
  r1dat <- c('Column Number', '', colseq)
  r1 <- do.call(tags$tr, lapply(seq_along(r1dat), function(i) {
    do.call(tags$td, c(r1dat[i], r1attr[[i]]))
  }))

  id_col <- lapply(colseq, function(i) { checkboxInput(sprintf('idcol_%s', i), NULL, width='50px') })
  rank_cols <- lapply(colseq, function(i) { checkboxInput(sprintf('rank_%s', i), NULL, width='50px') })
  prev_cols <- lapply(colseq, function(i) { checkboxInput(sprintf('prev_%s', i), NULL, width='50px') })
  wght_cols <- lapply(colseq, function(i) { numericInput(sprintf('weight_%s', i), NULL, 1, min=0, width='75px', step='any') })
  miss_wght <- list(numericInput('missingness', NULL, 0.1, min=0, width='100px', step='any'))
  n_discard <- list(tags$span(
    numericInput('ndiscard', NULL, d, min=0, step=1, width='100px'),
    sprintf('(Population Size: %s)', r)
  ))

  r2dat <- c('ID Column', list(tags$img(src='help.png', title=id.help)), id_col)
  r2attr <- c(list(list(align = 'left')), lapply(seq(nc+1), function(i) list(align = 'center')))
  r2 <- do.call(tags$tr, c(list(bgcolor='#cccccc'), lapply(seq_along(r2dat), function(i) {
    do.call(tags$td, c(r2dat[i], r2attr[[i]]))
  })))

  r3dat <- c('Rank Columns', list(tags$img(src='help.png', title=rank.help)), rank_cols)
  r3attr <- r2attr
  r3 <- do.call(tags$tr, lapply(seq_along(r3dat), function(i) {
    do.call(tags$td, c(r3dat[i], r3attr[[i]]))
  }))

  r4dat <- c('Prevent Matches', list(tags$img(src='help.png', title=prevent.help)), prev_cols)
  r4attr <- r2attr
  r4 <- do.call(tags$tr, c(list(bgcolor='#cccccc'), lapply(seq_along(r4dat), function(i) {
    do.call(tags$td, c(r4dat[i], r4attr[[i]]))
  })))

  r5dat <- c('Column Weight', list(tags$img(src='help.png', title=weight.help)), wght_cols)
  r5attr <- r2attr
  r5 <- do.call(tags$tr, lapply(seq_along(r5dat), function(i) {
    do.call(tags$td, c(r5dat[i], r5attr[[i]]))
  }))

  r6dat <- c('Missingness Weight', list(tags$img(src='help.png', title=missingness.help)), miss_wght)
  r6attr <- c(list(list(align = 'left')), list(list(align = 'left')), list(list(colspan=nc, align = 'center')))
  r6 <- do.call(tags$tr, c(list(bgcolor='#cccccc'), lapply(seq_along(r6dat), function(i) {
    do.call(tags$td, c(r6dat[i], r6attr[[i]]))
  })))

  r7dat <- c('Units to Discard', list(tags$img(src='help.png', title=discard.help)), n_discard)
  r7attr <- r6attr
  r7 <- do.call(tags$tr, lapply(seq_along(r7dat), function(i) {
    do.call(tags$td, c(r7dat[i], r7attr[[i]]))
  }))

  bag <- list(r1, r2, r3, r4, r5, r6, r7)
  if(header) {
    rhdat <- c('Column Names', '', colnames)
    rhattr <- lapply(seq(nc+2), function(i) list(fonttype = 'bold'))
    rh <- do.call(tags$tr, c(list(bgcolor='#cccccc'), lapply(seq_along(rhdat), function(i) {
      do.call(tags$td, c(rhdat[i], rhattr[[i]]))
    })))
    bag <- c(bag, list(rh))
  }
  do.call(tags$table, c(cellspacing = 0, cellpadding = 0.5, bag, ldf))
}

wrapup.covariate <- function(v) {
  filename <- basename(v$covFile)
  idcol <- v$covArgs$idcol
  rankcols <- v$covArgs$rankcols
  prevent <- v$covArgs$prevent
  colnames <- v$covColumns
  dmfile <- v$dmFile

  tablesize <- length(colnames)
  blcol <- logical(tablesize)
  cov.info <- data.frame("Column Name" = colnames,
    "Weight" = v$covArgs$weights,
    "ID Column" = blcol,
    "Rank Column" = blcol,
    "Prevent Matching" = blcol,
    check.names=FALSE
  )
  if(length(idcol) == 1 && !is.na(idcol) && idcol > 0 && idcol <= tablesize) {
    cov.info[idcol, "ID Column"] <- TRUE
  }
  cov.info[rankcols[rankcols %in% 1:tablesize], "Rank Column"] <- TRUE
  cov.info[prevent[prevent %in% 1:tablesize], "Prevent Matching"] <- TRUE
  ldf <- lapply(seq(tablesize), function(j) {
    do.call(tags$tr, lapply(seq(ncol(cov.info)), function(i) tags$td(cov.info[j,i])))
  })
  rhdat <- names(cov.info)
  rhattr <- lapply(seq(ncol(cov.info)), function(i) list(fonttype = 'bold', width = '120px'))
  rh <- do.call(tags$tr, c(list(bgcolor='#cccccc'), lapply(seq_along(rhdat), function(i) {
    do.call(tags$td, c(rhdat[i], rhattr[[i]]))
  })))
  tdf <- do.call(tags$table, c(list(rh), ldf))

  file.size <- NA
  if(file.access(dmfile, 4) == 0) {
    file.size <- round(file.info(dmfile)$size/1000, 2)
  }
  md_mw <- sprintf('Missingness Weight: %s', v$covArgs$missing.weight)
  md_nd <- sprintf('Units to Discard: %s', v$covArgs$ndiscard)
  md_fn <- sprintf('Uploaded Filename: %s', filename)
  md_dim <- sprintf('Dimensions: %s', paste(dim(v$dm), collapse='*'))
  md_fs <- sprintf('File size: %s (KB)', file.size)
  md_dl <- downloadLink("downloadDM", "Download distance matrix")
  tags$div(
    tags$fieldset(tags$legend('Covariate Matrix'), tags$p(tdf), tags$p(md_mw), tags$p(md_nd), tags$p(md_fn)),
    tags$fieldset(tags$legend('Distance Matrix'), tags$p(md_dim), tags$p(md_fs), tags$p(md_dl))
  )
}

viewMatchTable <- function(m) {
  m1 <- m$matches
  m2 <- m$halves
  m1[,5] <- sprintf('%0.3f', m1[,5])
  m2[,5] <- sprintf('%0.3f', m2[,5])
  hasId <- any(m1[,1] != m1[,2])
  # remove redundant columns if no id
  if(!hasId) {
    m1 <- m1[,c(2,4,5)]
    m2 <- m2[,c(2,4,5)]
    colwid <- rep('80px', 3)
  } else {
    # missing will be set appropriately
    colwid <- c('', '80px', '', '80px', '80px')
  }
  h1 <- names(m1)
  h2 <- names(m2)
  th1 <- do.call(tags$tr, lapply(seq_along(h1), function(i) {
    tags$td(h1[i], align = 'center', width = colwid[i])
  }))
  th2 <- do.call(tags$tr, lapply(seq_along(h2), function(i) {
    tags$td(h2[i], align = 'center', width = colwid[i])
  }))
  ldf1 <- lapply(seq(nrow(m1)), function(j) {
    do.call(tags$tr, lapply(seq(ncol(m1)), function(i) tags$td(m1[j,i], align = 'center')))
  })
  ldf2 <- lapply(seq(nrow(m2)), function(j) {
    do.call(tags$tr, lapply(seq(ncol(m2)), function(i) tags$td(m2[j,i], align = 'center')))
  })
  t1 <- do.call(tags$table, c(list(th1), ldf1))
  t2 <- do.call(tags$table, c(list(th2), ldf2))
  tags$table(tags$tr(tags$th('Match Table'), tags$th('Reduced Match Table')), tags$tr(tags$td(t1), tags$td(t2, valign = 'top')), border = '1')
}

viewAssignment <- function(m1) {
  m1[,5] <- sprintf('%0.3f', m1[,5])
  hasId <- any(m1[,1] != m1[,2])
  size <- '100px'
  # remove redundant columns if no id
  if(!hasId) {
    m1 <- m1[,c(2,4,5,6)]
    colwid <- rep(size, 4)
  } else {
    # missing will be set appropriately
    colwid <- c('', size, '', size, size, size)
  }
  h1 <- names(m1)
  th1 <- do.call(tags$tr, lapply(seq_along(h1), function(i) {
    tags$th(h1[i], width = colwid[i])
  }))
  ldf1 <- lapply(seq(nrow(m1)), function(j) {
    do.call(tags$tr, lapply(seq(ncol(m1)), function(i) tags$td(m1[j,i])))
  })
  do.call(tags$table, c(list(th1), ldf1))
}

viewQuality <- function(x) {
  xx <- cbind(variable = rownames(x$q), as.data.frame(cbind(x$q, sd = x$sd)))
  h1 <- names(xx)
  th1 <- do.call(tags$tr, lapply(seq_along(h1), function(i) {
    tags$th(h1[i], align = 'center', width = '75px')
  }))
  ldf1 <- lapply(seq(nrow(xx)), function(j) {
    do.call(tags$tr, lapply(seq(ncol(xx)), function(i) {
      patt <- c('%s', '%0.3f')[as.numeric(i > 1) + 1]
      tags$td(sprintf(patt, xx[j,i]))
    }))
  })
  do.call(tags$table, c(list(th1), ldf1))
}
