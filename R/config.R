# Database configuration
DB_CONFIG <- list(
  host = "192.168.178.88",
  user = "tschemic",
  password = Sys.getenv("MARIADB_PW"),
  dbname = "ausgaben",
  port = 3306,
  table = "einkauf_stage",
  table_final = "einkauf_neu",
  primary_key = "PrimaryKey"
)
