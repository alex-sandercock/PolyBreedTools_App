#' help UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList includeMarkdown
mod_help_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      column(width=12),
      column(width=12,
             box(title="Predictive Ability", id = "Predictive_Ability_box",width = 12, collapsible = TRUE, collapsed = TRUE, status = "info", solidHeader = TRUE,
                 "This tab provides the predictive ability of a GBLUP model for each trait across all samples within a genomic dataset",
                 br(), br(),
                 bs4Dash::tabsetPanel(id = "Predictive_Ability_tabset",
                                      tabPanel("Parameters description", value = "Predictive_Ability_par", br(),
                                               includeMarkdown(system.file("help_files/Predictive_Ability_par.Rmd", package = "BIGapp"))
                                      ),
                                      tabPanel("Results description", value = "Predictive_Ability_results", br(),
                                               includeMarkdown(system.file("help_files/Predictive_Ability_res.Rmd", package = "BIGapp"))
                                      ),
                                      tabPanel("How to cite", value = "Predictive_Ability_cite", br(),
                                               includeMarkdown(system.file("help_files/Predictive_Ability_cite.Rmd", package = "BIGapp"))
                                      ))
             ),
             box(title="Genomic Prediction", id = "Genomic_Prediction_box",width = 12, collapsible = TRUE, collapsed = TRUE, status = "info", solidHeader = TRUE,
                 "his tab estimates the trait and estimated-breeding-values (EBVs) for either all individuals in a genomic dataset, or by training the model with one genomic dataset to predict the values in another.",
                 br(), br(),
                 bs4Dash::tabsetPanel(id = "Genomic_Prediction_tabset",
                                      tabPanel("Parameters description", value = "Genomic_Prediction_par", br(),
                                               includeMarkdown(system.file("help_files/Genomic_Prediction_par.Rmd", package = "BIGapp"))
                                      ),
                                      tabPanel("Results description", value = "Genomic_Prediction_results", br(),
                                               includeMarkdown(system.file("help_files/Genomic_Prediction_res.Rmd", package = "BIGapp"))
                                      ),
                                      tabPanel("How to cite", value = "Genomic_Prediction_cite", br(),
                                               includeMarkdown(system.file("help_files/Genomic_Prediction_cite.Rmd", package = "BIGapp"))
                                      ))
             ),
      ),
      column(width=2)
      # Add Help content here
    )
  )
}

#' help Server Functions
#'
#' @noRd
mod_help_server <- function(input, output, session, parent_session){

  ns <- session$ns

}

## To be copied in the UI
# mod_help_ui("help_1")

## To be copied in the server
# mod_help_server("help_1")
