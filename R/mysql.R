#' Connect to the AWS MySQL database
#'
#' Connect to the AWS MySQL database using variables saved in the .Renviron
#'
#' @return returns an S4 object that inherits from \code{\link[DBI]{DBIConnection-class}}.
#' This object is used to communicate with the database engine.
#' @export
db_connect <- function() {
  if (Sys.getenv("CURBCUT_DB_USER") == "")
    stop(paste0("You do not have a Curbcut database user access."))

  DBI::dbConnect(
    drv = RMySQL::MySQL(),
    username = Sys.getenv("CURBCUT_DB_USER"),
    password = Sys.getenv("CURBCUT_DB_PASSWORD"),
    host = "ccdb-instance-1.cplnwzthenux.us-east-1.rds.amazonaws.com",
    port = 3306,
    dbname = "ccdb")
}

#' Execute, write or get queries from the MySQL database
#'
#' Execute, write or get queries with connection and disconnection included, to
#' ensure that connections are as short-lived as possible, and that no
#' connection remains open indefinitely.
#'
#' @param type <`character`> One of `execute`, `write` or `get`. They correspond
#' to `DBI::dbExecute`, `DBI::dbWriteTable` and `DBI::dbGetQuery`.
#' @param statement <`character` or `data.frame`> A character string containing SQL
#' for type `execute` and `get`, but a data.frame for type `write`.
#' @param name <`character`> The table name, mandatory for type `write`. In other
#' cases, won't be used.
#'
#' @return An error or nothing if ran successfully.
#' @export
db_query <- function(type, statement, name = NULL) {

  # Connect to database
  conn <- db_connect()

  # Englobe the call into a tryCatch to disconnect if it fails
  tryCatch({
    call_fun <- (\(x) if (x == "execute") return("DBI::dbExecute") else
      if (x == "write") return("DBI::dbWriteTable") else
        if (x == "get") return("DBI::dbGetQuery") else
          return(stop("Argument `type` unrecognized.")))(type)

    if (type == "write" && is.null(name))
      stop("A name must be given to the data.frame")

    call <- list(conn = conn, statement = statement)

    if (type == "write") {
      call <- c(call, list(name = name, overwrite = TRUE))
      names(call)[names(call) == "statement"] <- "value"
    }

    do.call(eval(parse(text = call_fun)), call)
  }, error = function(e) {
    db_disconnect(conn)
    stop(e)
  })

  # Disconnect
  db_disconnect(conn)

  # Return
  return(invisible(NULL))
}

#' Disconnect from the AWS MySQL database
#'
#' @param conn A \code{\link[DBI]{DBIConnection-class}} object, as returned by
#' \code{\link[cc.data]{db_connect}}.
#' @return A message out of \code{\link[DBI]{dbDisconnect}}
#' @export
db_disconnect <- function(conn) {
  DBI::dbDisconnect(conn)
}

#' Write the list of processed census data to the MySQL database
#'
#' @param processed_census_data <`named list`> The finalized process data
#' normally coming out of the final processing function:
#' \code{\link[cc.data]{census_reduce_years}}
#'
#' @return Returns an error or nothing if ran successfully. All tables in the
#' list fed to `processed_census_data` are written to the MySQL database with
#' the spatial features dropped.
#' @export
db_write_processed_data <- function(processed_census_data) {

  out <-
    sapply(names(processed_census_data), \(scale) {
      tb_name <- paste("processed", scale, sep = "_")

      tb <- processed_census_data[[scale]]
      if ("sf" %in% class(tb)) tb <- sf::st_drop_geometry(tb)

      # Write the table
      db_query(type = "write", statement = tb, name = tb_name)

      # Create an index on ID for faster retrieval
      # Modify the column first so that it's character type
      max_cell_size <- max(nchar(tb$ID))
      db_query(type = "execute",
               statement = paste("ALTER TABLE", tb_name, "MODIFY COLUMN",
                                 "ID", "VARCHAR(", max_cell_size, ")"))
      # Index the ID column
      index_name <- paste0("ID_index_", tb_name)
      db_query(type = "execute",
               statement = paste("CREATE INDEX", index_name,
                                 "ON", tb_name, "(ID)"))
    })

  return(invisible(NULL))
}

#' Write the list of scales and years of raw census data to the MySQL database
#'
#' @param DA_data_raw <`list of sf data.frame`> The DA output of
#' \code{\link[cc.data]{census_data_raw}}. The DA output of the raw data retrieval.
#'
#' @return Returns an error or nothing if ran successfully. All DA tables in the list fed
#' to `DA_data_raw` are written to the MySQL database with the spatial
#' features transformed to binary then hexadecimal encoded character. If the
#' character is too large for the MySQL cell limit, the geometry is split in
#' multiple columns, e.g. geometry_1, geometry_2, etc.
#' @export
db_write_raw_data <- function(DA_data_raw) {

  # The innodb_strict_mode setting affects the handling of syntax errors
  # for CREATE TABLE, ALTER TABLE and CREATE INDEX statements.
  # innodb_strict_mode also enables a record size check, so that an
  # INSERT or UPDATE never fails due to the record being too large for
  # the selected page size.
  #
  # By default, the option is ON:
  # `DBI::dbGetQuery(conn, "show variables like '%strict%'")`
  # Set it to OFF:
  # `DBI::dbGetQuery(conn, "set innodb_strict_mode = 0;")`
  innodb_strict_mode <-
    db_query(type = "get", "show variables like '%strict%'")$Value
  if (innodb_strict_mode == "ON")
    db_query(type = "execute", "set innodb_strict_mode = 0;")

  # Progress bar
  pb <- progressr::progressor(steps = length(DA_data_raw))

  # Territories sf to filter out from the db
  terr <- data.table::rbindlist(lapply(c(60, 61, 62), \(x) {
    cancensus::get_census(
      dataset = cc.data::census_years[length(cc.data::census_years)],
      regions = list(PR = x),
      geo_format = "sf",
      quiet = TRUE)
  })) |> sf::st_as_sf()
  terr <- sf::st_union(terr)
  terr <- sf::st_transform(terr, 3347)

  # Write to db
  mapply(\(df, year) {
    tb_name <- paste("raw", "DA", year, sep = "_")

    # Cut the Canada's three territories. They are huge DAs, leading to hundreds
    # of geometry columns in the database. e.g. DA 62040033 in 2006. By itself:
    # 106mb and leading to 402 geometry columns to respect the MySQL cell limit.
    df_centroids <- suppressWarnings(sf::st_point_on_surface(df))
    df <- df[!as.vector(sf::st_intersects(df_centroids, terr, sparse = FALSE)), ]

    # Get a df without geometry column
    df_no_geo <- sf::st_drop_geometry(df)

    # Get the geometry has hex for better storage. The maximum size of a cell
    # is around 65k characters, so split for as many columns as needed.
    hexes <-
      lapply(seq_along(df$geometry), \(x) {
        hex <- sf::st_as_binary(df$geometry[x], hex = TRUE)
        if (nchar(hex) > 60000) {
          split_every <- 60000
          start <- 0:(ceiling(nchar(hex)/split_every) - 1) * split_every + 1
          end <- 1:(ceiling(nchar(hex)/split_every)) * split_every
          hex <- mapply(\(st, en) substr(x = hex, start = st, stop = en),
                        start, end)
          hex <- Reduce(c, hex)
        }

        nb <- seq_len(length(hex))
        names(hex) <- paste0("geometry_", nb)
        hex
      })

    # How many geometry columns are needed
    possible_geometries <-
      paste0("geometry_", seq_len(max(sapply(hexes, length))))

    # Get each column values in order. If empty, empty string.
    hexes_split <-
      sapply(possible_geometries, \(geom) {
        sapply(hexes, \(hex) {
          if (!geom %in% names(hex)) return("")
          return(hex[[geom]])
        })
      }, simplify = FALSE, USE.NAMES = TRUE)

    # Assign each row/column values to the df with no geometry
    for (i in possible_geometries) {
      df_no_geo[[i]] <- hexes_split[[i]]
    }

    row.names(df_no_geo) <- NULL

    # Advance the progress bar
    pb()

    # # Write to the MySQL database. If file size is to big, split in two.
    # # log_file_size is a System variable that is simply impossible to change on
    # # AWS RDS Aurora Serverless.
    # if (object.size(df_no_geo) > 500000000) {
    #   df_split <- split(df_no_geo, 1:2)
    #   sapply(names(df_split), \(x) {
    #     tb <- df_split[[x]]
    #     new_tb_name <- paste(tb_name, x, sep = "_")
    #     DBI::dbWriteTable(conn, new_tb_name, tb, overwrite = TRUE)
    #   })
    # } else {
    db_query(type = "write", name = tb_name, statement = df_no_geo)
    # }

    # Create an index on ID for faster retrieval
    # Modify the column first so that it's character type
    max_cell_size <- max(nchar(df_no_geo$ID))
    db_query(type = "execute", paste("ALTER TABLE", tb_name, "MODIFY COLUMN",
                                     "ID", "VARCHAR(", max_cell_size, ")"))
    # Index the ID column
    index_name <- paste0("ID_index_", tb_name)
    db_query(type = "execute", paste("CREATE INDEX", index_name,
                                     "ON", tb_name, "(ID)"))

  }, DA_data_raw, names(DA_data_raw))

  return(invisible(NULL))
}

#' Read a table in the MySQL database
#'
#' Read a table in the database by filtering the ID column.
#'
#' @param table <`character`> The table name in the MySQL database. To list all
#' tables, use \code{\link[DBI]{dbListTables}}.
#' @param columns <`character vector`> A character vector of all columns to
#' retrieve. By default, all columns (`*`).
#' @param IDs <`character vector`> A character vector of all IDs to retrieve.
#'
#' @return A tibble or an sf tibble depending if geometry is available.
#' @export
db_read_data <- function(table, columns = "*", IDs) {
  IDs_condition <- paste0("ID = ", IDs)

  # If not retrieve all columns, make sure to retrieve IDs
  if (columns != "*") columns <- c("ID", columns, "geometry_*")

  query <- paste("SELECT", paste0(columns, collapse = ", "), "FROM",
                 table, "WHERE",
                 paste0(IDs_condition, collapse = " OR "))

  df <- db_query(type = "execute", statement = query)
  df <- df[, names(df) != "row_names"]
  df <- tibble::as_tibble(df)

  # If it is geometry, convert it to sf
  if (sum(grepl("geometry", names(df))) > 0) {
    geo_columns <- names(df)[grepl("geometry_", names(df))]

    df$geometry <-
      lapply(seq_len(nrow(df)), \(x) {
        paste0(df[x, geo_columns], collapse = "")
      })

    df <- df[, !grepl("geometry_", names(df))]

    df$geometry <-
      sapply(seq_along(df$geometry), \(x) {
        wkb::hex2raw(df$geometry[x])[[1]] |>
          sf::st_as_sfc(crs = 3347)
      })

    df <- sf::st_as_sf(df, crs = 3347)
  }

  return(df)
}

#' Create a read only user to the AWS MySQL database
#'
#' @param user <`character`> User identifier for the new user
#' @param password <`character`> Password identifier for the new user
#' @param admin <`logical`> If the user created should be granted all powers
#' on the MySQL db. If FALSE, read only permissions are attributed.
#'
#' @return Returns an error or nothing if ran successfully. User is then created and
#' has SELECT and SHOW VIEW privileges only if `admin` is set to FALSE (default).
#' @export
db_create_user <- function(user, password, admin = FALSE) {
  create <-
    db_query(type = "execute", sprintf("CREATE USER %s IDENTIFIED BY '%s';",
                                       user, password))

  rights <- if (admin) "ALL PRIVILEGES" else "SELECT, SHOW VIEW"

  grant <-
    db_query(type = "execute", sprintf("GRANT %s ON ccdb.* TO %s@'%%';",
                                       rights, user))
  flush <-
    db_query(type = "execute", "FLUSH PRIVILEGES;")

  return(invisible(NULL))
}

#' Delete a user that has access to the AWS MySQL database
#'
#' @param user <`character`> User identifier for the user to remove
#'
#' @return Returns an error or nothing if ran successfully. User's access are
#' then removed and the user is deleted.
#' @export
db_delete_user <- function(user) {
  drop <- db_query(type = "execute", sprintf("DROP USER %s@'%%';", user))

  return(invisible(NULL))
}


