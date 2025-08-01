library(shiny)
library(readr)
library(RPostgres)
library(DBI)

readRenviron(".Renviron")

###################################################
# There we create connection with database in SQL #
###################################################
con <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = Sys.getenv("DB_NAME"),       # np. "my_database"
  host = Sys.getenv("DB_HOST"),         # np. "localhost"
  port = Sys.getenv("DB_PORT"),         # np. "5432"
  user = Sys.getenv("DB_USER"),         # np. "postgres"
  password = Sys.getenv("DB_PASSWORD")  # np. "tajne_haslo"
)

# We load our data into database

df_clubs <- read_delim("clubs.csv", delim=";")
df_players <- read_delim("players.csv", delim=";")
df_stats <- read_delim("all_stats.csv", delim=";")

df_players
dbExecute(con, "DELETE FROM stats")
dbExecute(con, "DELETE FROM player")
dbExecute(con, "DELETE FROM clubs")


dbWriteTable(con, 'clubs', df_clubs, append=TRUE)
dbWriteTable(con, 'player', df_players, append=TRUE)
dbWriteTable(con, 'stats', df_stats, append=TRUE)

