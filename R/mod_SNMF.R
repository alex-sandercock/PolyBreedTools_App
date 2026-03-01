#' SNMF UI Function
#'
#' @description A Shiny module implementing LEA::snmf() for unsupervised ancestry estimation.
#'
#' @param id Module id.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import shinydisconnect
#' @importFrom DT DTOutput renderDT
#' @importFrom bs4Dash valueBoxOutput
mod_SNMF_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shinydisconnect::disconnectMessage(
        text = "An unexpected error occurred, please reload the application and check the input file(s).",
        refresh = "Reload now",
        background = "white",
        colour = "grey",
        overlayColour = "grey",
        overlayOpacity = 0.3,
        refreshColour = "purple"
      ),
      shiny::column(
        width = 3,
        bs4Dash::box(
          title = "Inputs",
          width = 12,
          collapsible = TRUE,
          collapsed = FALSE,
          status = "info",
          solidHeader = TRUE,
          shiny::fileInput(
            ns("snmf_file"),
            "Genotypes (.vcf, .vcf.gz, .geno)",
            accept = c(".vcf", ".gz", ".geno")
          ),
          shiny::numericInput(ns("snmf_ploidy"), "Ploidy", value = 2, min = 1, step = 1),
          shiny::fluidRow(
            shiny::column(6, shiny::numericInput(ns("snmf_k_min"), "K min", value = 1, min = 1, step = 1)),
            shiny::column(6, shiny::numericInput(ns("snmf_k_max"), "K max", value = 10, min = 1, step = 1))
          ),
          shiny::numericInput(ns("snmf_repetitions"), "Repetitions", value = 5, min = 1, step = 1),
          shiny::numericInput(ns("snmf_alpha"), "Alpha", value = 100, min = 0),
          shiny::numericInput(ns("snmf_iterations"), "Iterations", value = 200, min = 1, step = 1),
          shiny::numericInput(ns("snmf_tolerance"), "Tolerance", value = 1e-4, min = 0),
          shiny::numericInput(ns("snmf_percentage"), "Percentage", value = 0.05, min = 0, max = 1, step = 0.01),
          shiny::numericInput(ns("snmf_cpu"), "CPU", value = 1, min = 1, step = 1),
          shiny::numericInput(ns("snmf_seed"), "Seed", value = 123, min = 1, step = 1),
          shiny::radioButtons(
            ns("snmf_select_mode"),
            "Selection mode",
            choices = c(
              "Auto-pick best K (cross-entropy)" = "auto_entropy",
              "Manual K/run (cross-entropy)" = "manual_entropy",
              "No cross-entropy (manual)" = "no_entropy"
            ),
            selected = "auto_entropy"
          ),
          shiny::actionButton(ns("snmf_run"), "Run SNMF"),
          shiny::hr(),
          shiny::uiOutput(ns("snmf_selectors_ui")),
          shiny::hr(),
          shiny::downloadButton(ns("download_q_csv"), "Download Q (CSV)"),
          shiny::downloadButton(ns("download_ce_csv"), "Download cross-entropy (CSV)"),
          shiny::downloadButton(ns("download_project_zip"), "Download project (zip)")
        )
      ),
      shiny::column(
        width = 6,
        bs4Dash::box(
          title = "Results",
          status = "info",
          solidHeader = FALSE,
          width = 12,
          height = 550,
          maximizable = TRUE,
          bs4Dash::tabsetPanel(
            id = ns("snmf_results_tabs"),
            type = "tabs",
            shiny::tabPanel(
              "Instructions",
              shiny::HTML(
                paste0(
                  "<p>This tab runs <code>LEA::snmf()</code> to estimate ancestry proportions (Q-matrix) across K.</p>",
                  "<ul>",
                  "<li>Upload a <code>.vcf</code> / <code>.vcf.gz</code> (will be converted to LEA <code>.geno</code>) or an existing <code>.geno</code>.</li>",
                  "<li>Choose a K range and repetitions.</li>",
                  "<li>In cross-entropy modes, the app summarizes cross-entropy per K and can auto-pick the best K/run.</li>",
                  "</ul>"
                )
              ),
              style = "overflow-y: auto; height: 500px"
            ),
            shiny::tabPanel(
              "Cross-Entropy",
              shiny::plotOutput(ns("snmf_ce_plot"), height = "420px"),
              DT::DTOutput(ns("snmf_ce_table")),
              style = "overflow-y: auto; height: 500px"
            ),
            shiny::tabPanel(
              "Ancestry Plot",
              shiny::plotOutput(ns("snmf_q_plot"), height = "450px"),
              style = "overflow-y: auto; height: 500px"
            ),
            shiny::tabPanel(
              "Q Matrix",
              DT::DTOutput(ns("snmf_q_table")),
              style = "overflow-y: auto; height: 500px"
            ),
            shiny::tabPanel(
              "Logs",
              shiny::verbatimTextOutput(ns("snmf_status")),
              style = "overflow-y: auto; height: 500px"
            )
          )
        )
      ),
      shiny::column(
        width = 3,
        bs4Dash::valueBoxOutput(ns("snmf_best_k_box"), width = NULL),
        bs4Dash::valueBoxOutput(ns("snmf_best_ce_box"), width = NULL),
        bs4Dash::box(
          title = "Status",
          width = 12,
          collapsible = TRUE,
          status = "info",
          shinyWidgets::progressBar(
            id = ns("pb_snmf"),
            value = 0,
            status = "info",
            display_pct = TRUE,
            striped = TRUE,
            title = " "
          )
        )
      )
    )
  )
}

#' SNMF Server Functions
#'
#' @noRd
mod_SNMF_server <- function(input, output, session, parent_session) {
  ns <- session$ns

  set_status <- function(...) {
    msg <- paste0(...)
    output$snmf_status <- shiny::renderText(msg)
  }

  show_error <- function(title, message) {
    shiny::showModal(shiny::modalDialog(
      title = title,
      easyClose = TRUE,
      footer = shiny::modalButton("Close"),
      message
    ))
  }

  call_with_allowed_named_args <- function(fun, args) {
    allowed <- names(formals(fun))
    if (is.null(allowed)) {
      return(do.call(fun, args))
    }
    keep <- names(args) == "" | names(args) %in% allowed
    do.call(fun, args[keep])
  }

  decompress_gz <- function(gz_path, out_path) {
    in_con <- gzfile(gz_path, open = "rb")
    on.exit(close(in_con), add = TRUE)
    out_con <- file(out_path, open = "wb")
    on.exit(close(out_con), add = TRUE)

    repeat {
      buf <- readBin(in_con, what = "raw", n = 1024 * 1024)
      if (length(buf) == 0) break
      writeBin(buf, out_con)
    }
    out_path
  }

  state <- shiny::reactiveValues(
    run_dir = NULL,
    project = NULL,
    geno_path = NULL,
    vcf_path = NULL,
    k_values = NULL,
    repetitions = NULL,
    entropy_enabled = FALSE,
    ce_df = NULL,
    ce_summary = NULL,
    best_k = NULL,
    best_run_by_k = NULL
  )

  output$snmf_best_k_box <- bs4Dash::renderValueBox({
    bs4Dash::valueBox(
      value = if (!is.null(state$best_k)) state$best_k else "—",
      subtitle = "Best K",
      icon = shiny::icon("layer-group"),
      color = "info"
    )
  })

  output$snmf_best_ce_box <- bs4Dash::renderValueBox({
    bs4Dash::valueBox(
      value = if (isTRUE(state$entropy_enabled) && !is.null(state$ce_summary)) {
        best_k <- state$best_k
        best_row <- state$ce_summary[state$ce_summary$K == best_k, , drop = FALSE]
        if (nrow(best_row) == 1) round(best_row$min_cross_entropy, 6) else "—"
      } else {
        "—"
      },
      subtitle = "Min cross-entropy",
      icon = shiny::icon("chart-line"),
      color = "warning"
    )
  })

  output$snmf_selectors_ui <- shiny::renderUI({
    if (is.null(state$project) || is.null(state$k_values) || is.null(state$repetitions)) {
      return(shiny::HTML("<em>Run SNMF to enable K/run selectors and downloads.</em>"))
    }
    shiny::tagList(
      shiny::selectInput(
        ns("snmf_selected_k"),
        "Selected K",
        choices = as.character(state$k_values),
        selected = as.character(state$best_k %||% state$k_values[[1]])
      ),
      shiny::selectInput(
        ns("snmf_selected_run"),
        "Selected run",
        choices = as.character(seq_len(state$repetitions)),
        selected = "1"
      )
    )
  })

  observeEvent(input$snmf_selected_k, {
    req(state$project, state$k_values, state$repetitions)
    k <- as.integer(input$snmf_selected_k)

    selected_run <- 1L
    if (!is.null(state$best_run_by_k) && !is.na(state$best_run_by_k[as.character(k)])) {
      selected_run <- as.integer(state$best_run_by_k[as.character(k)])
    }

    shiny::updateSelectInput(
      session,
      "snmf_selected_run",
      choices = as.character(seq_len(state$repetitions)),
      selected = as.character(selected_run)
    )
  }, ignoreInit = TRUE)

  selected_k <- shiny::reactive({
    req(state$project, state$k_values)
    k <- input$snmf_selected_k
    if (is.null(k) || !nzchar(k)) {
      return(as.integer(state$best_k %||% state$k_values[[1]]))
    }
    as.integer(k)
  })

  selected_run <- shiny::reactive({
    req(state$project, state$repetitions)
    r <- input$snmf_selected_run
    if (is.null(r) || !nzchar(r)) return(1L)
    as.integer(r)
  })

  q_matrix <- shiny::reactive({
    req(state$project)
    k <- selected_k()
    r <- selected_run()

    q <- call_with_allowed_named_args(
      LEA::Q,
      list(state$project, K = k, run = r)
    )

    q <- as.matrix(q)
    if (is.null(rownames(q))) {
      rownames(q) <- paste0("ind", seq_len(nrow(q)))
    }
    colnames(q) <- paste0("Cluster", seq_len(ncol(q)))
    q
  })

  output$snmf_q_table <- DT::renderDT({
    q <- q_matrix()
    df <- data.frame(ID = rownames(q), q, check.names = FALSE)
    DT::datatable(df, options = list(scrollX = TRUE, pageLength = 10))
  })

  output$snmf_q_plot <- shiny::renderPlot({
    q <- q_matrix()
    df <- data.frame(ID = rownames(q), q, check.names = FALSE)

    q_cols <- colnames(q)
    long <- stats::reshape(
      df,
      varying = q_cols,
      v.names = "Q",
      timevar = "Cluster",
      times = q_cols,
      direction = "long"
    )
    long$ID <- factor(long$ID, levels = unique(df$ID))
    long$Cluster <- factor(long$Cluster, levels = q_cols)

    ggplot2::ggplot(long, ggplot2::aes(x = ID, y = Q, fill = Cluster)) +
      ggplot2::geom_col(width = 0.9) +
      ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      ggplot2::labs(x = "Individual", y = "Ancestry proportion", fill = "Cluster") +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1, size = 8),
        panel.grid.major.x = ggplot2::element_blank()
      )
  })

  output$snmf_ce_plot <- shiny::renderPlot({
    validate(shiny::need(isTRUE(state$entropy_enabled), "Cross-entropy disabled (see Selection mode)."))
    validate(shiny::need(!is.null(state$ce_summary), "Run SNMF to compute cross-entropy."))

    ggplot2::ggplot(state$ce_summary, ggplot2::aes(x = K, y = min_cross_entropy)) +
      ggplot2::geom_line() +
      ggplot2::geom_point() +
      ggplot2::labs(x = "K", y = "Minimum cross-entropy", title = "SNMF cross-entropy by K") +
      ggplot2::theme_minimal()
  })

  output$snmf_ce_table <- DT::renderDT({
    validate(shiny::need(isTRUE(state$entropy_enabled), "Cross-entropy disabled (see Selection mode)."))
    validate(shiny::need(!is.null(state$ce_summary), "Run SNMF to compute cross-entropy."))

    DT::datatable(
      state$ce_summary,
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })

  observeEvent(input$snmf_run, {
    if (!requireNamespace("LEA", quietly = TRUE)) {
      show_error("Missing dependency", "Install the LEA package to use SNMF.")
      return()
    }

    if (is.null(input$snmf_file$datapath)) {
      show_error("Missing input", "Upload a .vcf/.vcf.gz or .geno file.")
      return()
    }

    k_min <- as.integer(input$snmf_k_min)
    k_max <- as.integer(input$snmf_k_max)
    if (is.na(k_min) || is.na(k_max) || k_min < 1 || k_max < 1 || k_min > k_max) {
      show_error("Invalid K range", "K min and K max must be integers with K min ≤ K max and both ≥ 1.")
      return()
    }

    reps <- as.integer(input$snmf_repetitions)
    if (is.na(reps) || reps < 1) {
      show_error("Invalid repetitions", "Repetitions must be an integer ≥ 1.")
      return()
    }

    ploidy <- as.integer(input$snmf_ploidy)
    if (is.na(ploidy) || ploidy < 1) {
      show_error("Invalid ploidy", "Ploidy must be an integer ≥ 1.")
      return()
    }

    entropy_enabled <- input$snmf_select_mode %in% c("auto_entropy", "manual_entropy")

    if (!is.null(state$run_dir) && dir.exists(state$run_dir)) {
      unlink(state$run_dir, recursive = TRUE, force = TRUE)
    }

    state$run_dir <- tempfile("snmf_", tmpdir = tempdir())
    dir.create(state$run_dir, recursive = TRUE, showWarnings = FALSE)
    state$project <- NULL
    state$geno_path <- NULL
    state$vcf_path <- NULL
    state$k_values <- seq(k_min, k_max)
    state$repetitions <- reps
    state$entropy_enabled <- entropy_enabled
    state$ce_df <- NULL
    state$ce_summary <- NULL
    state$best_k <- NULL
    state$best_run_by_k <- NULL

    shinyWidgets::updateProgressBar(session = session, id = "pb_snmf", value = 5, title = "Preparing input")
    set_status("Preparing input...\n")

    uploaded_name <- input$snmf_file$name %||% "genotypes"
    ext_lower <- tolower(uploaded_name)

    file_base <- sub("\\.(vcf\\.gz|vcf|geno|gz)$", "", basename(uploaded_name), ignore.case = TRUE)

    geno_path <- file.path(state$run_dir, paste0(file_base, ".geno"))
    vcf_path <- file.path(state$run_dir, paste0(file_base, ".vcf"))

    # Copy uploaded file into run_dir for consistent outputs.
    uploaded_copy <- file.path(state$run_dir, basename(uploaded_name))
    file.copy(input$snmf_file$datapath, uploaded_copy, overwrite = TRUE)

    # Convert input to .geno if needed
    if (grepl("\\.geno$", ext_lower)) {
      file.copy(uploaded_copy, geno_path, overwrite = TRUE)
    } else if (grepl("\\.vcf$", ext_lower)) {
      file.copy(uploaded_copy, vcf_path, overwrite = TRUE)
      shinyWidgets::updateProgressBar(session = session, id = "pb_snmf", value = 15, title = "Converting VCF → GENO")
      set_status("Converting VCF to GENO...\n")

      vcf2geno_res <- tryCatch(
        {
          call_with_allowed_named_args(LEA::vcf2geno, list(vcf_path, output.file = geno_path))
        },
        error = function(e) e
      )

      if (!file.exists(geno_path)) {
        # Fallback: vcf2geno might choose its own output filename
        geno_candidates <- list.files(state$run_dir, pattern = "\\.geno$", full.names = TRUE)
        if (length(geno_candidates) >= 1) {
          newest <- geno_candidates[which.max(file.info(geno_candidates)$mtime)]
          file.copy(newest, geno_path, overwrite = TRUE)
        }
      }

      if (!file.exists(geno_path)) {
        msg <- if (inherits(vcf2geno_res, "error")) vcf2geno_res$message else "vcf2geno() did not produce a .geno file."
        show_error("VCF conversion failed", msg)
        shinyWidgets::updateProgressBar(session = session, id = "pb_snmf", value = 0, title = " ")
        set_status(paste0("ERROR: ", msg, "\n"))
        return()
      }
    } else if (grepl("\\.vcf\\.gz$|\\.gz$", ext_lower)) {
      shinyWidgets::updateProgressBar(session = session, id = "pb_snmf", value = 10, title = "Decompressing VCF.GZ")
      set_status("Decompressing VCF.GZ...\n")

      decompress_gz(uploaded_copy, vcf_path)

      shinyWidgets::updateProgressBar(session = session, id = "pb_snmf", value = 15, title = "Converting VCF → GENO")
      set_status("Converting VCF to GENO...\n")

      vcf2geno_res <- tryCatch(
        {
          call_with_allowed_named_args(LEA::vcf2geno, list(vcf_path, output.file = geno_path))
        },
        error = function(e) e
      )

      if (!file.exists(geno_path)) {
        geno_candidates <- list.files(state$run_dir, pattern = "\\.geno$", full.names = TRUE)
        if (length(geno_candidates) >= 1) {
          newest <- geno_candidates[which.max(file.info(geno_candidates)$mtime)]
          file.copy(newest, geno_path, overwrite = TRUE)
        }
      }

      if (!file.exists(geno_path)) {
        msg <- if (inherits(vcf2geno_res, "error")) vcf2geno_res$message else "vcf2geno() did not produce a .geno file."
        show_error("VCF.GZ conversion failed", msg)
        shinyWidgets::updateProgressBar(session = session, id = "pb_snmf", value = 0, title = " ")
        set_status(paste0("ERROR: ", msg, "\n"))
        return()
      }
    } else {
      show_error("Unsupported file type", "Upload a .vcf, .vcf.gz, or .geno file.")
      shinyWidgets::updateProgressBar(session = session, id = "pb_snmf", value = 0, title = " ")
      set_status("ERROR: Unsupported file type.\n")
      return()
    }

    state$geno_path <- geno_path
    state$vcf_path <- if (file.exists(vcf_path)) vcf_path else NULL

    shinyWidgets::updateProgressBar(session = session, id = "pb_snmf", value = 35, title = "Running SNMF")
    set_status(
      "Running SNMF...\n",
      "Input GENO: ", basename(state$geno_path), "\n",
      "K: ", k_min, "–", k_max, "\n",
      "Repetitions: ", reps, "\n",
      "Entropy: ", if (entropy_enabled) "enabled" else "disabled", "\n"
    )

    old_wd <- getwd()
    on.exit(setwd(old_wd), add = TRUE)
    setwd(state$run_dir)

    snmf_args <- list(
      state$geno_path,
      K = state$k_values,
      repetitions = reps,
      ploidy = ploidy,
      entropy = entropy_enabled,
      alpha = input$snmf_alpha,
      iterations = as.integer(input$snmf_iterations),
      tolerance = as.numeric(input$snmf_tolerance),
      percentage = as.numeric(input$snmf_percentage),
      CPU = as.integer(input$snmf_cpu),
      seed = as.integer(input$snmf_seed)
    )

    project <- tryCatch(
      call_with_allowed_named_args(LEA::snmf, snmf_args),
      error = function(e) e
    )

    if (inherits(project, "error")) {
      show_error("SNMF failed", project$message)
      shinyWidgets::updateProgressBar(session = session, id = "pb_snmf", value = 0, title = " ")
      set_status(paste0("ERROR: ", project$message, "\n"))
      return()
    }

    state$project <- project

    shinyWidgets::updateProgressBar(session = session, id = "pb_snmf", value = 75, title = "Summarizing results")
    set_status(paste0(capture.output(str(project, max.level = 1)), collapse = "\n"), "\n")

    if (entropy_enabled) {
      ce_records <- list()
      for (k in state$k_values) {
        for (r in seq_len(reps)) {
          ce_val <- tryCatch(
            call_with_allowed_named_args(LEA::cross.entropy, list(state$project, K = k, run = r)),
            error = function(e) NA_real_
          )
          ce_records[[length(ce_records) + 1]] <- data.frame(
            K = as.integer(k),
            run = as.integer(r),
            cross_entropy = as.numeric(ce_val),
            stringsAsFactors = FALSE
          )
        }
      }
      state$ce_df <- do.call(rbind, ce_records)

      min_ce_by_k <- tapply(state$ce_df$cross_entropy, state$ce_df$K, min, na.rm = TRUE)
      best_run_by_k <- sapply(names(min_ce_by_k), function(k_chr) {
        k_int <- as.integer(k_chr)
        sub <- state$ce_df[state$ce_df$K == k_int, , drop = FALSE]
        if (nrow(sub) == 0) return(NA_integer_)
        sub$run[which.min(sub$cross_entropy)]
      })

      state$best_run_by_k <- best_run_by_k
      ce_summary <- data.frame(
        K = as.integer(names(min_ce_by_k)),
        best_run = as.integer(best_run_by_k[names(min_ce_by_k)]),
        min_cross_entropy = as.numeric(min_ce_by_k),
        stringsAsFactors = FALSE
      )
      ce_summary <- ce_summary[order(ce_summary$K), , drop = FALSE]
      state$ce_summary <- ce_summary
      state$best_k <- ce_summary$K[which.min(ce_summary$min_cross_entropy)]
    } else {
      state$best_k <- state$k_values[[1]]
    }

    # Initialize selectors (K + run)
    shiny::updateSelectInput(
      session,
      "snmf_selected_k",
      choices = as.character(state$k_values),
      selected = as.character(state$best_k %||% state$k_values[[1]])
    )

    initial_run <- 1L
    if (!is.null(state$best_run_by_k)) {
      br <- state$best_run_by_k[as.character(state$best_k)]
      if (!is.na(br)) initial_run <- as.integer(br)
    }
    shiny::updateSelectInput(
      session,
      "snmf_selected_run",
      choices = as.character(seq_len(reps)),
      selected = as.character(initial_run)
    )

    shinyWidgets::updateProgressBar(session = session, id = "pb_snmf", value = 100, title = "Complete!")
    set_status("SNMF complete.\n")
  })

  output$download_q_csv <- shiny::downloadHandler(
    filename = function() {
      paste0("snmf_Q_K", selected_k(), "_run", selected_run(), "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(state$project)
      q <- q_matrix()
      df <- data.frame(ID = rownames(q), q, check.names = FALSE)
      utils::write.csv(df, file, row.names = FALSE)
    }
  )

  output$download_ce_csv <- shiny::downloadHandler(
    filename = function() {
      paste0("snmf_cross_entropy_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(state$project)
      validate(shiny::need(isTRUE(state$entropy_enabled), "Cross-entropy disabled (see Selection mode)."))
      utils::write.csv(state$ce_df %||% data.frame(), file, row.names = FALSE)
    }
  )

  output$download_project_zip <- shiny::downloadHandler(
    filename = function() {
      paste0("snmf_project_", Sys.Date(), ".zip")
    },
    content = function(file) {
      req(state$project)
      export_try <- function(args) {
        tryCatch(call_with_allowed_named_args(LEA::export.snmfProject, args), error = function(e) e)
      }

      res <- export_try(list(state$project, file = file))
      if (file.exists(file)) return()

      res2 <- export_try(list(state$project))
      if (is.character(res2) && length(res2) >= 1) {
        if (file.exists(res2[[1]])) {
          file.copy(res2[[1]], file, overwrite = TRUE)
          return()
        }
        if (dir.exists(res2[[1]])) {
          all_paths <- list.files(res2[[1]], full.names = TRUE, recursive = TRUE)
          all_paths <- all_paths[file.info(all_paths)$isdir %in% FALSE]
          utils::zip(zipfile = file, files = all_paths)
          return()
        }
      }

      zips <- list.files(state$run_dir, pattern = "\\.zip$", full.names = TRUE)
      if (length(zips) >= 1) {
        newest <- zips[which.max(file.info(zips)$mtime)]
        file.copy(newest, file, overwrite = TRUE)
        return()
      }

      if (inherits(res, "error")) stop(res$message)
      if (inherits(res2, "error")) stop(res2$message)
      stop("export.snmfProject() did not produce a zip file.")
    }
  )

  session$onSessionEnded(function() {
    if (!is.null(state$run_dir) && dir.exists(state$run_dir)) {
      unlink(state$run_dir, recursive = TRUE, force = TRUE)
    }
  })
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
