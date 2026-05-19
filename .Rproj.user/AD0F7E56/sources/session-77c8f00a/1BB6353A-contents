library(DBI)
library(RPostgres)

con <- dbConnect(
  Postgres(),
  host     = Sys.getenv("SUPABASE_HOST"),
  port     = as.integer(Sys.getenv("SUPABASE_PORT")),
  dbname   = Sys.getenv("SUPABASE_DB"),
  user     = Sys.getenv("SUPABASE_USER"),
  password = Sys.getenv("SUPABASE_PASSWORD"),
  sslmode  = "require"
)

# Si funciona, verás tus tablas:
dbListTables(con)


# Leer tabla completa
df <- dbReadTable(con, "lista")
head(df)

# O con una query SQL específica
df <- dbGetQuery(con, "SELECT * FROM lista LIMIT 10")
head(df)
