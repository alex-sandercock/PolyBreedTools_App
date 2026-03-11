#' PolyBreedTools UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import shinydisconnect
#' @importFrom bs4Dash valueBoxOutput
mod_polybreedtools_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 3,
        bs4Dash::box(
          title = "Inputs",
          width = 12,
          collapsible = TRUE,
          collapsed = FALSE,
          status = "info",
          solidHeader = TRUE,
          fileInput(ns("reference_file"), "Reference Genotypes (.txt)", accept = ".txt"),
          fileInput(ns("ref_ids_file"), "Reference IDs (.txt)", accept = ".txt"),
          fileInput(ns("validation_file"), "Validation Genotypes (.txt)", accept = ".txt"),
          numericInput(ns("ploidy"), "Ploidy", value = 2, min = 1, max = 20, step = 1),
          actionButton(ns("run"), "Run Estimation"),
          br(),
          br()
        )
      ),
      column(
        width = 6,
        bs4Dash::box(
          title = "Line/breed content estimation",
          status = "info",
          solidHeader = FALSE,
          width = 12,
          height = 600,
          maximizable = TRUE,
          bs4Dash::tabsetPanel(
            id = ns("polybreedtools_results_tabs"),
            type = "tabs",
            tabPanel(
              "Instructions",
              fluidRow(
                column(12, shiny::wellPanel(shiny::HTML('
              <ul>
                <li>This tool was developed by Breeding Insight.</li>
                <li>It estimates the proportion of each of the lines/groups included in the <strong>reference population</strong> from genotype samples using methods from 
                  <a href="https://www.animalsciencepublications.org/publications/tas/articles/1/1/36" target="_blank">Funkhouser et al. (2017)</a>.</li>
                <li><strong>Input format:</strong></li>
                <ul>
                  <li><strong>Reference Genotypes:</strong> A genotype matrix (.txt) with SNPs in rows and samples in columns. The first column must be <code>ID</code>. Missing should be coded as <code>NA</code>.</li>
                  <li><strong>Reference IDs:</strong> A two-column .txt file with population labels. Header example: <code>Group1</code>, <code>Group2</code>.</li>
                  <li><strong>Validation Genotypes:</strong> Same format as the reference genotype file.</li>
                </ul>
              </ul>
            ')))
              ),
              style = "overflow-y: auto; height: 500px"
            ),
            shiny::tabPanel("Results Table", DT::DTOutput(ns("preview")), style = "overflow-y: auto; height: 500px"),
            shiny::tabPanel("Ancestry Plot", shiny::plotOutput(ns("bar_plot"), height = "450px"), style = "overflow-y: auto; height: 500px")
          )
        ),
        box(
          title = "Example Inputs", status = "info", solidHeader = FALSE, width = 12, height = 400, maximizable = T,
          bs4Dash::tabsetPanel(
            id = ns('example_tabs'),
            type = "tabs",
            tabPanel(
              "Reference IDs",
              tableOutput(ns("example_ids")),
              br(),
              downloadButton(ns("download_ids"), "Download Sample Reference IDs"),
              style = "overflow-y: auto; height: 350px"
            ),
            tabPanel(
              "Genotypes",
              tableOutput(ns("example_genos")),
              br(),
              downloadButton(ns("download_genos"), "Download Sample Genotypes"),
              style = "overflow-y: auto; height: 350px"
            )
          )
        )
      ),
      shiny::column(
        width = 3,
        bs4Dash::box(
          title = "Status",
          width = 12,
          collapsible = TRUE,
          status = "info",
          solidHeader = TRUE,
          shiny::verbatimTextOutput(ns("status"))
        ),
        box(title = "Plot Controls", width=12, status = "warning", solidHeader = TRUE, collapsible = TRUE,
            selectInput(ns("color_choice"), "Color Palette", choices = list("Standard Palettes" = c("Set1","Set3","Pastel2",
                                                                                                    "Pastel1","Accent","Spectral",
                                                                                                    "RdYlGn","RdGy"),
                                                                            "Colorblind Friendly" = c("Set2","Paired","Dark2","YlOrRd","YlOrBr","YlGnBu","YlGn",
                                                                                                      "Reds","RdPu","Purples","PuRd","PuBuGn","PuBu",
                                                                                                      "OrRd","Oranges","Greys","Greens","GnBu","BuPu",
                                                                                                      "BuGn","Blues","RdYlBu",
                                                                                                      "RdBu", "PuOr","PRGn","PiYG","BrBG"
                                                                            )),
                        selected = "Set1"),
            checkboxInput(ns("poly_show_sample_labels"), "Show sample labels", value = FALSE),
            checkboxInput(ns("poly_sort_by_predicted"), "Sort by predicted line", value = TRUE),
            sliderInput(ns("poly_label_size"), "Label size", min = 6, max = 14, value = 8, step = 1),
            div(style="display:inline-block; float:left", dropdownButton(
              tags$h3("Save"),
              selectInput(
                inputId = ns("poly_image_type"),
                label = "File Type",
                choices = c("png", "jpeg", "svg", "pdf"),
                selected = "png"
              ),
              sliderInput(inputId = ns("poly_image_res"), label = "Resolution (DPI)", value = 300, min = 50, max = 1000, step = 50),
              sliderInput(inputId = ns("poly_image_width"), label = "Width (in)", value = 10, min = 3, max = 30, step = 0.5),
              sliderInput(inputId = ns("poly_image_height"), label = "Height (in)", value = 5, min = 3, max = 20, step = 0.5),
              fluidRow(
                downloadButton(ns("download_poly_figure"), "Save Image"),
                downloadButton(ns("download_poly_file"), "Save Files")
              ),
              circle = FALSE,
              status = "danger",
              icon = icon("floppy-disk"),
              width = "300px",
              label = "Save",
              tooltip = tooltipOptions(title = "Click to see options!")
            ))
        )
      )
    )
  )
}

#' PolyBreedTools Server Functions
#'
#' @importFrom graphics axis hist points
#' @import ggplot2
#' @import RColorBrewer
#' @importFrom scales comma_format
#' @import openxlsx
#' @import BIGr
#'
#' @noRd
mod_polybreedtools_server <- function(input, output, session, parent_session){
  
  ns <- session$ns
  
  #Helper function
  format_percent <- function(x) {
    scales::percent_format(accuracy = 0.1)(x)
  }
  
  
  result_data <- reactiveVal(NULL)
  poly_items <- reactiveValues(
    pred_results = NULL,
    pred_results_long = NULL,
    id_order = NULL
  )
  
  observeEvent(input$run, {
    req(input$reference_file, input$ref_ids_file, input$validation_file)
    output$status <- renderText("Running estimation...")
    
    tryCatch({
      reference <- utils::read.table(input$reference_file$datapath, header = TRUE, sep = "\t")
      reference <- dplyr::distinct(reference, ID, .keep_all = TRUE)
      reference <- tibble::column_to_rownames(reference, "ID")
      
      reference_ids <- utils::read.table(input$ref_ids_file$datapath, header = TRUE, sep = "\t")
      ref_ids <- lapply(as.list(reference_ids), as.character)
      
validation_raw <- utils::read.table(input$validation_file$datapath, header = TRUE, sep = "\t")     

# NA filtering: validation samples (rows) with < 50% call rate
sample_call_rate <- rowSums(!is.na(validation_raw)) / ncol(validation_raw)
removed_samples <- validation_raw$ID[sample_call_rate < 0.5]
validation_filtered <- validation_raw[sample_call_rate >= 0.5, , drop = FALSE]

# NA filtering: validation markers (columns) with all NA
col_call_counts <- colSums(!is.na(validation_filtered))
removed_markers <- colnames(validation_filtered)[col_call_counts == 0]
validation <- validation_filtered[, col_call_counts > 0, drop = FALSE]


# Build warning messages
warning_messages <- c()
if (length(removed_samples) > 0) {
  warning_messages <- c(warning_messages, paste(
    "WARNING: The following validation samples were removed due to genotyping rate < 50%:\n",
    paste0("  • ", removed_samples, collapse = "\n")
  ))
}
if (length(removed_markers) > 0) {
  warning_messages <- c(warning_messages, paste(
    "WARNING: The following markers were removed from validation because they had no successful genotype calls:\n",
    paste0("  • ", removed_markers, collapse = "\n")
  ))
}
      
      
      
      #duplicated ids in validation file
       val_ids <- validation[, 1]
      dup_val <- val_ids[duplicated(val_ids)]
      if (length(dup_val) > 0) {
        
        # Build message
        dup_val_msg <- paste(
          "Error: The following sample IDs have duplicates in your validation file.",
          "Please check your input file and remove or rename the following IDs:\n",
          paste0("  • ", dup_val, collapse = "\n")
        )
        
        # Show it in the ‘Status’ box
        output$status <- renderText(dup_val_msg)
        return()
      }
      
      
      validation <- dplyr::distinct(validation, ID, .keep_all = TRUE)
      validation <- tibble::column_to_rownames(validation, "ID")
      
      freq <- BIGr:::allele_freq_poly(reference, ref_ids, ploidy = input$ploidy)
      
      # Error on NaN in freq
      
      na_pos <- which(is.na(freq), arr.ind = TRUE)   # rows = row #, cols = col #
      
      if (nrow(na_pos) > 0) {
        
        # For each marker (column) collect the rows that contain NaN
        na_report <- lapply(unique(na_pos[, 2]), function(col_idx) {
          rows_with_na <- na_pos[na_pos[, 2] == col_idx, 1]   # row numbers
          
          paste0(
            "  • ", colnames(freq)[col_idx], ": ",          # marker name
            paste(rownames(freq)[rows_with_na], collapse = ", ")
          )
        })
        
        # Build message
        NaN_freq_msg <- paste(
          "Error: The following markers where not succesfully genotyped for at least one reference population, please remove or correct them:",
          paste(na_report, collapse = "\n"),
          "\nPlease remove or correct these markers", sep = "\n"
        )
        
        # Show it in the ‘Status’ box
        output$status <- renderText(NaN_freq_msg)
        return()
      }
        
      
      prediction <- BIGr:::solve_composition_poly(validation, freq, ploidy = input$ploidy)
      
      prediction <- as.data.frame(prediction, check.names = FALSE)
      prediction <- prediction[, !colnames(prediction) %in% c("R2"), drop = FALSE]
      prediction[] <- lapply(prediction, as.numeric)
      
      columns_to_select <- colnames(prediction)
      
      predicted_line <- columns_to_select[max.col(prediction[, columns_to_select, drop = FALSE], ties.method = "first")]
      
      pred_results <- tibble::rownames_to_column(prediction, var = "ID")
      pred_results <- dplyr::mutate(
        pred_results,
        `Predicted line` = predicted_line
      )
      pred_results <- dplyr::mutate(
        pred_results,
        dplyr::across(dplyr::all_of(columns_to_select), ~format_percent(.x))
      )
      
      result_data(pred_results)
      
      id_order <- data.frame(
        ID = rownames(prediction),
        predicted_line = predicted_line,
        predicted_value = apply(prediction[, columns_to_select, drop = FALSE], 1, max, na.rm = TRUE),
        stringsAsFactors = FALSE
      )
      
      output$preview <- DT::renderDT({
        DT::datatable(pred_results, options = list(pageLength = 10, scrollX = TRUE))
      })
      
      pred_results_long <- tibble::rownames_to_column(prediction, var = "ID")
      pred_results_long <- tidyr::pivot_longer(
        pred_results_long,
        cols = dplyr::all_of(columns_to_select),
        names_to = "category",
        values_to = "percent"
      )
      
      pred_results_long$predicted_line <- id_order$predicted_line[match(pred_results_long$ID, id_order$ID)]
      
      poly_items$pred_results <- pred_results
      poly_items$pred_results_long <- pred_results_long
      poly_items$id_order <- id_order
      
      final_status <- "Estimation complete. File ready for download."
      
      if (length(warning_messages) > 0) {
        final_status <- paste(
          final_status,
          "\n\n",
          paste(warning_messages, collapse = "\n\n")
        )
      }
      
      output$status <- renderText(final_status)      
    }, error = function(e) {
      output$status <- renderText(paste("Error during estimation:", e$message))
    })
  })
  
  ancestry_plot <- reactive({
    req(poly_items$pred_results_long, poly_items$id_order)
    
    dat <- poly_items$pred_results_long
    
    if (isTRUE(input$poly_sort_by_predicted)) {
      ord <- poly_items$id_order[order(poly_items$id_order$predicted_line, -poly_items$id_order$predicted_value), , drop = FALSE]
      dat$ID <- factor(dat$ID, levels = ord$ID)
    } else {
      dat$ID <- factor(dat$ID, levels = unique(dat$ID))
    }
    
    p <- ggplot(dat, aes(x = ID, y = percent, fill = category)) +
      geom_bar(stat = "identity") +
      scale_fill_brewer(palette = input$color_choice) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      labs(x = "Individual ID", y = "Ancestry Proportion", fill = "Line") +
      theme_minimal()
    
    if (isTRUE(input$poly_show_sample_labels)) {
      p <- p + theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = as.numeric(input$poly_label_size %||% 8))
      )
    } else {
      p <- p + theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      )
    }
    
    p
  })
  
  output$bar_plot <- renderPlot({
    req(poly_items$pred_results_long)
    ancestry_plot()
  })
  
  output$download_poly_file <- downloadHandler(
    filename = function() {
      paste0("lineage_estimation_", format(Sys.Date(), "%Y-%m-%d"), ".xlsx")
    },
    content = function(file) {
      req(poly_items$pred_results)
      openxlsx::write.xlsx(poly_items$pred_results, file = file, rowNames = FALSE)
    }
  )
  
  output$download_poly_figure <- downloadHandler(
    filename = function() {
      ext <- input$poly_image_type %||% "png"
      paste0("polybreedtools_ancestry_plot_", format(Sys.Date(), "%Y-%m-%d"), ".", ext)
    },
    content = function(file) {
      req(poly_items$pred_results_long)
      p <- ancestry_plot()
      
      ext <- input$poly_image_type %||% "png"
      width <- as.numeric(input$poly_image_width %||% 10)
      height <- as.numeric(input$poly_image_height %||% 5)
      dpi <- as.numeric(input$poly_image_res %||% 300)
      
      if (ext %in% c("png", "jpeg")) {
        ggplot2::ggsave(filename = file, plot = p, width = width, height = height, units = "in", dpi = dpi)
      } else {
        ggplot2::ggsave(filename = file, plot = p, width = width, height = height, units = "in")
      }
    }
  )
  
  # Sample reference IDs with >2 rows and variable length names
  example_ids_df <- data.frame(
    Group1 = c("SampleAlpha", "S3", "ExampleFour", "",""),
    Group2 = c("SampleOne", "SampleTwo", "SampleThree", "SampleFour", "SampleFive"),
    Group3 = c("SampleX", "SampleYy", "SampleZzzz", "ExampleEight", "")
  )
  
  output$example_ids <- renderTable({
    example_ids_df
  }, bordered = TRUE)
  
  # Sample genotype matrix matching your example
  example_genos_df <- data.frame(
    ID = paste0("Sample", c("1", "2", "3", "4", "5")),
    Marker1 = as.integer(c(0, 0, 1, 2, 1)),
    Marker2 = as.integer(c(NA, 1, 0, 1, 2)),
    Marker3 = as.integer(c(0, 0, NA, 1, 1)),
    Marker4 = as.integer(c(0, 0, 0, 0, 0))
  )
  
  output$example_genos <- renderTable({
    example_genos_df
  }, bordered = TRUE)
  
  # Download handlers for sample files
  output$download_ids <- downloadHandler(
    filename = function() "sample_reference_ids.txt",
    content = function(file) {
      write.table(example_ids_df, file, sep = "\t", row.names = FALSE, quote = FALSE)
    }
  )
  
  output$download_genos <- downloadHandler(
    filename = function() "sample_genotypes.txt",
    content = function(file) {
      write.table(example_genos_df, file, sep = "\t", row.names = FALSE, quote = FALSE)
    }
  )
}

## To be copied in the UI
# mod_diversity_ui("SNMF_1")

## To be copied in the server
# mod_diversity_server("SNMF_1")

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
