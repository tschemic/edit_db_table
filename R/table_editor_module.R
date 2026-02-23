# Table Editor Module
library(shiny)
library(DT)
library(bslib)

# Module UI
table_editor_ui <- function(id) {
  ns <- NS(id)
  
  # Create JavaScript with the correct namespaced input ID
  delete_button_js <- sprintf("
$(document).on('click', '.delete_btn', function(e) {
  e.preventDefault();
  e.stopPropagation();
  e.stopImmediatePropagation();
  
  var id = $(this).data('id');
  console.log('Delete button clicked, id:', id);
  console.log('Sending input to: %s');
  
  Shiny.setInputValue('%s',
      { PrimaryKey: id, ts: new Date().toISOString() },
      { priority: 'event' }
  );
  return false;
});
", ns("shiny_delete_row"), ns("shiny_delete_row"))
  
  tagList(
    tags$head(tags$script(HTML(delete_button_js))),
    card(
      card_header("Actions"),
      actionButton(
        ns("insert_btn"), 
        "Insert to einkauf_neu", 
        class = "btn-primary",
        icon = icon("plus")
      )
    ),
    card(
      card_header("Table Data"),
      height = "800px",  # Increased card height
      DTOutput(ns("db_table"))
    )
  )
}

# Module Server
table_editor_server <- function(id, pool, config) {
  moduleServer(id, function(input, output, session) {
    
    # Table reactive container - store original data without delete column
    data_reactive <- reactiveVal(load_table(pool, config$table))
    
    # Reactive expression for display data with delete buttons
    display_data <- reactive({
      df <- data_reactive()
      if (nrow(df) == 0) return(df)
      
      # Add delete button column
      df$delete <- sprintf(
        '<button type="button" class="delete_btn btn btn-danger btn-sm" data-id="%s">Delete</button>',
        as.character(df[[config$primary_key]])
      )
      df
    })
    
    # Render DT table with server-side processing
    output$db_table <- renderDT({
      datatable(
        display_data(),
        escape = FALSE,
        editable = list(
          target = "cell",
          disable = list(columns = ncol(display_data()) - 1)  # Disable last column (delete), 0-indexed
        ),
        selection = "none",
        rownames = FALSE,
        options = list(
          pageLength = 100,
          dom = 'Bfrtip',
          scrollY = "700px",  # Added vertical scrolling
          scrollCollapse = TRUE
        )
      )
    }, server = TRUE)  # Changed to server = TRUE
    
    # Handle insert button click
    observeEvent(input$insert_btn, {
      # cat("Insert button clicked\n")
      
      success <- insert_to_einkauf_neu(pool, 
                                       table_source = config$table,
                                       table_target = config$table_final)
      
      if (success) {
        dbExecute(pool, sprintf("DELETE FROM %s", config$table))
        showNotification(
          "Data successfully inserted into einkauf_neu table",
          type = "default",
          duration = 3
        )
      } else {
        showNotification(
          "Failed to insert data into einkauf_neu table",
          type = "error",
          duration = 5
        )
      }
      # tabelle neu einlesen
      updated <- load_table(pool, config$table)
      data_reactive(updated)
    })
    
    # Handle cell edits
    observeEvent(input$db_table_cell_edit, {
      info <- input$db_table_cell_edit
      original_df <- data_reactive()  # Use original dataframe without delete column
      
      row <- info$row
      col <- info$col + 1  # Convert from 0-based to 1-based indexing
      new_val <- info$value
      
      # Get column name from original dataframe (without delete column)
      if (col > ncol(original_df)) {
        # This should be the delete column, skip it
        return()
      }
      
      col_name <- colnames(original_df)[col]
      
      # Skip if somehow we're trying to edit the delete column
      if (col_name == "delete") return()
      
      pk_value <- original_df[[config$primary_key]][row]
      
      # Debug: print the values being used
      # cat("Editing row:", row, "col (0-indexed):", info$col, "col (1-indexed):", col,
      # "col_name:", col_name, "new_val:", new_val, "pk_value:", pk_value, "\n")
      
      # Update database
      success <- update_cell(pool, config$table, config$primary_key, 
                             pk_value, col_name, new_val)
      
      if (success) {
        # Reload data - the reactive expression will handle adding delete buttons
        updated <- load_table(pool, config$table)
        data_reactive(updated)
      }
    })
    
    # Handle row deletions
    observeEvent(input$shiny_delete_row, {
      payload <- input$shiny_delete_row
      
      # Debug: print what we received
      # cat("Delete event received. Payload:", paste(capture.output(str(payload)), collapse = "\n"), "\n")
      
      pk_value <- payload$PrimaryKey
      if (is.null(pk_value) || is.na(pk_value)) {
        # cat("No valid primary key value found, aborting delete\n")
        return()
      }
      
      # Debug: print the values being used
      # cat("Deleting row with pk_value:", pk_value, "\n")
      
      # Delete from database
      success <- delete_row(pool, config$table, config$primary_key, pk_value)
      
      if (success) {
        # Reload data - the reactive expression will handle adding delete buttons
        updated <- load_table(pool, config$table)
        data_reactive(updated)
      }
    })
    
    # WebSocket connection check
    # observe({
    #   cat("Websocket connected:", session$token, "\n")
    # })
    
  })
}