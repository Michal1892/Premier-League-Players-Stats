# Premier-League-Players-Stats
R Shiny app for analyzing Premier League player statistics with PostgreSQL database connection.

# About the app

This Shiny application allows you to explore player statistics available in Fantasy Premier League for the 25/26 season, based on data from the 24/25 season. The app provides various statistics with two main tabs:

Player Stats — detailed statistics for a selected player.

Match Stats — aggregated statistics from all matches.


# Shiny Project – Setup Instructions

## Requirements

- R 
- R packages: shiny, shinywidgets, DT, readr, DBI, RPostgres  
- Installed PostgreSQL with **SQL Shell (psql)**  
- Access to a PostgreSQL database  

---

## Step 1: Open SQL Shell (psql)

1. Open the **SQL Shell (psql)** program (you can find it in the Start menu or in the PostgreSQL folder).

2. When starting, the program will ask for connection details:

   - Server [localhost]: press Enter or type `localhost`  
   - Database [postgres]: type the database name, e.g. `my_database` (if you don’t have it yet, enter `postgres`)  
   - Port [5432]: press Enter  
   - Username [postgres]: type your username, e.g. `postgres`  
   - Password: type your password (input will be hidden)  

---

## Step 2: Create the database (if it doesn’t exist yet)

In SQL Shell, enter the command:

```sql
CREATE DATABASE my_database;
```

Then connect to the database with this command (exit and reopen SQL Shell or change connection):

```sql
\c my_database
```

---

## Step 3: Load structure and data from the SQL file

In SQL Shell, execute the following command (replace the path with your actual file path):

```sql
\i C:/full/path/to/project/sql/sql_commands.txt
```

---

## Step 4: Configure the `.Renviron` file

1. Copy the `.Renviron_example` file to `.Renviron`:

```bash
cp .Renviron_example .Renviron
```

2. Open `.Renviron` and fill in your environment variables:

```
DB_HOST=localhost
DB_USER=postgres
DB_PASSWORD=your_password
DB_NAME=my_database
DB_PORT=5432
```

---

## Step 5: Install required R packages

In R or RStudio, run:

```r
install.packages(c("shiny", "shinywidgets", "DT", "readr", "DBI", "RPostgres"))
```

---

## Step 6: Run the Shiny application

In the R or RStudio console, type:

```r
shiny::runApp()
```
