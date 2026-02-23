# Shiny MySQL/MariaDB table editor with row editing & deletion
# Uses RMariaDB + pool + DT with safe JSON responses.

# Main Shiny Application
library(shiny)
library(bslib)

# Source utility files
source("R/config.R")
source("R/db_utils.R")
source("R/table_editor_module.R")

# Create database pool
pool <- create_db_pool(DB_CONFIG)

# Ensure pool is closed on app stop
onStop(function() {
  try(poolClose(pool))
})

# UI
ui <- page_fluid(
  title = "MariaDB Table Editor",
  h1("MariaDB Table Editor"),
  table_editor_ui("table_editor")
)

# Server
server <- function(input, output, session) {
  table_editor_server("table_editor", pool, DB_CONFIG)
}

# Run the application
shinyApp(ui = ui, server = server)
