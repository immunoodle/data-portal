# Migration Module Integration
# Add this to your main app.R or as a separate migration app

# Load required libraries
library(shiny)
library(shinydashboard)
library(DBI)
library(RPostgreSQL)
library(DT)
library(shinyWidgets)

# Source the migration modules
source("data_migration_ui.R")
source("data_migration_server.R")

# Simple migration app (can be integrated into existing app)
migration_app <- function() {
  
  ui <- dashboardPage(
    dashboardHeader(title = "MADI Data Migration"),
    
    dashboardSidebar(
      sidebarMenu(
        menuItem("Migration Tool", tabName = "migration", icon = icon("exchange-alt")),
        menuItem("Documentation", tabName = "docs", icon = icon("file-text"))
      )
    ),
    
    dashboardBody(
      tabItems(
        tabItem(tabName = "migration",
          migration_ui("migration_module")
        ),
        
        tabItem(tabName = "docs",
          fluidPage(
            h2("Migration Documentation"),
            
            wellPanel(
              h4("Overview"),
              p("This tool automates the data migration process between MADI databases. 
                It replaces the manual Excel export/import workflow with direct database connections."),
              
              h4("Migration Process"),
              tags$ol(
                tags$li("Configure source database connection (mlr-c3d7-db.c.dartmouth.edu)"),
                tags$li("Configure target database connection"),
                tags$li("Set migration parameters (study accession, experiment accession, etc.)"),
                tags$li("Execute migration procedures"),
                tags$li("Monitor progress and review results")
              ),
              
              h4("Required Parameters"),
              tags$ul(
                tags$li(strong("Study Accession:"), " Unique identifier for the study (e.g., SDY123)"),
                tags$li(strong("Experiment Accession:"), " Unique identifier for the experiment (e.g., EXP123)"),
                tags$li(strong("Workspace ID:"), " Target workspace identifier"),
                tags$li(strong("Concentration Unit:"), " Unit for concentration values (e.g., pg/mL)"),
                tags$li(strong("Source Type:"), " Type of biological source (e.g., SERUM, PLASMA)"),
                tags$li(strong("Time Collection Unit:"), " Unit for time measurements (e.g., Days)"),
                tags$li(strong("T0 Event:"), " Reference event for time measurements (e.g., vaccination)")
              )
            ),
            
            wellPanel(
              h4("Database Procedures"),
              p("The migration executes 4 main procedures in sequence:"),
              
              tags$div(
                style = "margin-left: 20px;",
                h5("1. database_control_samples"),
                p("Creates control samples table by combining data from xmap_buffer, xmap_control, and xmap_standard tables."),
                
                h5("2. database_std_curves"),
                p("Creates standard curves table from xmap_standard_fits data."),
                
                h5("3. db_control_results"),
                p("Creates control results table combining control data from multiple sources."),
                
                h5("4. mbaa_for_database"),
                p("Main MBAA (Multiplex Bead Array Assay) data migration - creates the primary mbaa_samples table.")
              )
            ),
            
            wellPanel(
              h4("Troubleshooting"),
              tags$ul(
                tags$li("Ensure both database connections are successful before starting migration"),
                tags$li("Verify that the madi_results schema exists in the source database"),
                tags$li("Check that all required parameters are provided"),
                tags$li("Monitor the migration log for detailed error messages"),
                tags$li("If migration fails, check database permissions and network connectivity")
              )
            )
          )
        )
      )
    )
  )
  
  server <- function(input, output, session) {
    migration_server("migration_module")
  }
  
  shinyApp(ui, server)
}

# Run the migration app
if (interactive()) {
  migration_app()
}
