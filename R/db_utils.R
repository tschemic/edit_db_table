# Database utility functions
library(DBI)
library(RMariaDB)
library(pool)

# Create database connection pool
create_db_pool <- function(config) {
  dbPool(
    drv = MariaDB(),
    host = config$host,
    user = config$user,
    password = config$password,
    dbname = config$dbname,
    port = config$port
  )
}

# Load table from database safely
load_table <- function(pool, table_name) {
  df <- tryCatch({
    dbGetQuery(pool, paste0("SELECT * FROM ", table_name))
  }, error = function(e) {
    message("Load error: ", conditionMessage(e))
    data.frame()
  })
  
  as.data.frame(df, stringsAsFactors = FALSE)
}

# Execute UPDATE query
update_cell <- function(pool, table_name, pk_col, pk_value, col_name, new_value) {
  print(paste("New value:", new_value))
  sql <- sprintf(
    "UPDATE %s SET %s = %s WHERE %s = %s",
    table_name,
    dbQuoteIdentifier(pool, col_name),
    if (is.na(new_value) | new_value == "") {
      "NULL"} 
    else {
      ifelse(col_name %in% c("Produktnummer",
                              "Preis",
                              "Menge",
                              "Preis_pro_Menge"),
               new_value,
               dbQuoteString(pool, new_value))
        },
        dbQuoteIdentifier(pool, pk_col),  # Quote the column name, not the value
        # dbQuoteString(pool, pk_value)  # Quote the value as string
        pk_value
  )
  
  # Debug: print the SQL query
  # cat("UPDATE SQL:", sql, "\n")
  
  tryCatch({
    result <- dbExecute(pool, sql)
    # cat("UPDATE successful, rows affected:", result, "\n")
    TRUE
  }, error = function(e) {
    message("UPDATE error: ", conditionMessage(e))
    FALSE
  })
}

# Execute DELETE query
delete_row <- function(pool, table_name, pk_col, pk_value) {
  sql <- sprintf("DELETE FROM %s WHERE %s = %s", 
                 table_name, 
                 dbQuoteIdentifier(pool, pk_col),  # Quote the column name
                 # dbQuoteString(pool, pk_value) # Quote the value as string
                 pk_value
                 )
  
  # Debug: print the SQL query
  # cat("DELETE SQL:", sql, "\n")
  
  tryCatch({
    result <- dbExecute(pool, sql)
    # cat("DELETE successful, rows affected:", result, "\n")
    TRUE
  }, error = function(e) {
    message("DELETE error: ", conditionMessage(e))
    FALSE
  })
}

# Execute INSERT INTO einkauf_neu query
insert_to_einkauf_neu <- function(pool, table_source, table_target) {
  sql <- sprintf("INSERT INTO %s 
  (Datum, Supermarkt, Produktnummer, Produktname, Preis, Steuerklasse, Menge, 
  Preis_pro_Menge, Einheit, Rabatt, GS1_Code) 
  SELECT Datum, Supermarkt, Produktnummer, Produktname, Preis, Steuerklasse, 
          Menge, Preis_pro_Menge, Einheit, Rabatt, '' AS GS1_Code FROM %s", 
                 table_target, 
                 table_source)
  
  # Debug: print the SQL query
  # cat("INSERT SQL:", sql, "\n")
  
  tryCatch({
    result <- dbExecute(pool, sql)
    # cat("INSERT successful, rows affected:", result, "\n")
    TRUE
  }, error = function(e) {
    message("INSERT error: ", conditionMessage(e))
    FALSE
  })
}