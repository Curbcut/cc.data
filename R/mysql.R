#' Connect to the AWS MySQL database
#'
#' Connect to the AWS MySQL database using variables saved in the .Renviron
#'
#' @return returns an S4 object that inherits from \code{\link[DBI]{DBIConnection-class}}.
#' This object is used to communicate with the database engine.
#' @export
db_connect <- function() {
  if (Sys.getenv("CURBCUT_DB_USER") == "") {
    stop(paste0("You do not have a Curbcut database user access."))
  }

  DBI::dbConnect(
    drv = RMySQL::MySQL(),
    username = Sys.getenv("CURBCUT_DB_USER"),
    password = Sys.getenv("CURBCUT_DB_PASSWORD"),
    host = "ccdb-instance-1.cplnwzthenux.us-east-1.rds.amazonaws.com",
    port = 3306,
    dbname = "ccdb"
  )
}

#' Execute, write or get queries from the MySQL database
#'
#' Execute, write or get queries with connection and disconnection included, to
#' ensure that connections are as short-lived as possible, and that no
#' connection remains open indefinitely.
#'
#' @param type <`character`> One of `execute`, `write`, `get`, `remove` or `append`.
#' They correspond to `DBI::dbExecute`, `DBI::dbWriteTable`, `DBI::dbGetQuery`,
#' `DBI::dbRemoveTable` and `DBI::sqlAppendTable`.
#' @param statement <`character` or `data.frame`> A character string containing SQL
#' for type `execute` and `get`, but a data.frame for type `write`.
#' @param name <`character`> The table name, mandatory for type `write`. In other
#' cases, won't be used.
#'
#' @return An error, a data.frame if `type` is get, or nothing if other types ran
#' successfully.
#' @export
db_query <- function(type, statement, name = NULL) {

  # Connect to database
  conn <- db_connect()

  if (type %in% c("write", "execute")) {
    # The following is a SESSION variable, and so it must be enable/disable
    # for every connection.
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
      DBI::dbGetQuery(conn, "show variables like '%strict%'")$Value
    if (innodb_strict_mode == "ON") {
      DBI::dbExecute(conn, "set innodb_strict_mode = 0;")
    }
  }

  # Include the call into a tryCatch to disconnect if it fails
  tryCatch(
    {
      call_fun <- (\(x) if (x == "execute") {
        return("DBI::dbExecute")
      } else
        if (x == "write") {
          return("DBI::dbWriteTable")
        } else
          if (x == "get") {
            return("DBI::dbGetQuery")
          } else
            if (x == "remove") {
              return("DBI::dbRemoveTable")
            } else
              if (x == "append") {
                return("DBI::sqlAppendTable")
              } else {
              return(stop("Argument `type` unrecognized."))
            })(type)

      if (type == "write" && is.null(name)) {
        stop("A name must be given to the data.frame")
      }

      call <- list(conn = conn, statement = statement)

      if (type %in% "write") {
        call <- c(call, list(name = name))
        names(call)[names(call) == "statement"] <- "value"
      }

      if (type == "append") {
        call <- c(call, list(table = name))
        names(call)[names(call) == "conn"] <- "con"
        names(call)[names(call) == "statement"] <- "values"
      }

      if (type == "remove") {
        names(call)[names(call) == "statement"] <- "name"
      }

      out <- suppressWarnings(do.call(eval(parse(text = call_fun)), call))
    },
    error = function(e) {
      db_disconnect(conn)
      stop(e)
    }
  )

  # Disconnect
  db_disconnect(conn)

  # Return
  return(if (type %in% c("append", "get")) out else invisible(NULL))
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

#' Write the an sf or non-sf table in the MySQL database
#'
#' @param df <`data.frame`> An sf or non-sf data.frame to be written to
#' the MySQL
#' @param tb_name <`character`> The name of the table in the MySQL database
#' @param primary_key <`character`> A column of the table to be written that should
#' be the pimary key for faster retrieval. The ID in that column needs to be
#' unique.
#' @param index <`character`> A column of the table to be written that should
#' be indexed for faster retrieval. No need for the values of the column to be
#' unique.
#' @param rows_per_append_wave <`numeric`> Number of rows to each for each waves
#' of append. The thiner the dataframe is, the more many rows can be appended
#' at a time. For census data with lots of columns and geometry columns, as
#' each row is heavier, the number of rows to append at once needs to be smaller
#' (around 1000). For buildings, every row is smaller and so around 50k can
#' be appended at the same time.
#'
#' @return Returns an error or nothing if ran successfully.
#' @export
db_write_table <- function(df, tb_name, primary_key = NULL, index = NULL,
                           rows_per_append_wave = 1000) {

  if ("sf" %in% class(df)) {
    # Get a df without geometry column
    df_no_geo <- sf::st_drop_geometry(df)

    # Get the geometry as hex for better storage. The maximum size of a cell
    # is around 65k characters, so split for as many columns as needed.

    hexes <-
      lapply(seq_len(nrow(df_no_geo)), \(x) {
        hex <- sf::st_as_binary(df$geometry[x], hex = TRUE)
        if (nchar(hex) > 60000) {
          split_every <- 60000
          start <- 0:(ceiling(nchar(hex) / split_every) - 1) * split_every + 1
          end <- 1:(ceiling(nchar(hex) / split_every)) * split_every
          hex <- mapply(
            \(st, en) substr(x = hex, start = st, stop = en),
            start, end
          )
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
          if (!geom %in% names(hex)) {
            return("")
          }
          return(hex[[geom]])
        })
      }, simplify = FALSE, USE.NAMES = TRUE)

    # Assign each row/column values to the df with no geometry
    for (i in possible_geometries) {
      df_no_geo[[i]] <- hexes_split[[i]]
    }

    row.names(df_no_geo) <- NULL
  } else df_no_geo <- df


  # Remove if existing table
  db_query("remove", tb_name)

  if (!is.null(primary_key)) {
    # Create the table with the primary key for faster retrieval
    cols <-
      paste0(paste(
        names(df_no_geo), "VARCHAR(",
        sapply(seq_len(ncol(df_no_geo)),
               \(x) {
                 n <- suppressWarnings(max(nchar(df_no_geo[[x]]), na.rm = TRUE))
                 if (is.infinite(n)) n <- 5
                 n
               })
        ,"),"), collapse = " ")
    db_query(type = "execute",
             statement = paste("CREATE TABLE", tb_name, "(",
                               cols,
                               "CONSTRAINT", paste0(tb_name, "_pk"),
                               "PRIMARY KEY",
                               paste0("(", primary_key , "))")))

    # Append the table's data by waves of 25k rows
    waves <- split(df_no_geo, 1:ceiling(nrow(df_no_geo) / rows_per_append_wave)) |>
      suppressWarnings()

    pb <- progressr::progressor(steps = length(waves))
    lapply(waves, \(x) {
      code <- db_query(type = "append", statement = x, name = tb_name)
      db_query(type = "execute", code)
      pb()
    })
  } else {
    db_query(type = "write", statement = df_no_geo, name = tb_name)
  }

  if (!is.null(index)) {
  # Create an index on ID for faster retrieval
  # Modify the column first so that it's character type
  max_cell_size <- max(nchar(df_no_geo[[index]]), na.rm = TRUE)
  db_query(type = "execute", paste(
    "ALTER TABLE", tb_name, "MODIFY COLUMN",
    index, "VARCHAR(", max_cell_size, ")"
  ))
  # Index the ID column
  index_name <- paste0("ID_index_", tb_name)
  db_query(type = "execute", paste(
    "CREATE INDEX", index_name,
    "ON", tb_name, "(", index, ")"
  ))
  }

  return(invisible(NULL))
}

#' Write the list of processed census data to the MySQL database
#'
#' @param processed_census <`named list`> The finalized process data
#' normally coming out of the final processing function:
#' \code{\link[cc.data]{census_reduce_years}}
#'
#' @return Returns an error or nothing if ran successfully. All tables in the
#' list fed to `processed_census` are written to the MySQL database with
#' the spatial features dropped.
#' @export
db_write_processed_data <- function(processed_census) {

  # Territories sf to filter out from the db
  terr <- data.table::rbindlist(lapply(c(60, 61, 62), \(x) {
    cancensus::get_census(
      dataset = cc.data::census_years[length(cc.data::census_years)],
      regions = list(PR = x),
      geo_format = "sf",
      quiet = TRUE
    )
  })) |> sf::st_as_sf()
  terr <- sf::st_union(terr)
  terr <- sf::st_transform(terr, 3347)

  out <-
    sapply(names(processed_census), \(scale) {
      tb_name <- paste("processed", scale, sep = "_")

      tb <- processed_census[[scale]]

      # Cut the Canada's three territories. They are huge DAs, leading to hundreds
      # of geometry columns in the database. Filter out to respect the MySQL cell
      # limit.
      tb_centroids <- suppressWarnings(sf::st_point_on_surface(tb))
      tb <- tb[!as.vector(sf::st_intersects(tb_centroids, terr, sparse = FALSE)), ]

      db_write_table(df = tb, tb_name = tb_name, primary_key = "ID")
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

  # Progress bar
  pb <- progressr::progressor(steps = length(DA_data_raw))

  # Territories sf to filter out from the db
  terr <- data.table::rbindlist(lapply(c(60, 61, 62), \(x) {
    cancensus::get_census(
      dataset = cc.data::census_years[length(cc.data::census_years)],
      regions = list(PR = x),
      geo_format = "sf",
      quiet = TRUE
    )
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

    # Write
    db_write_table(df = df, tb_name = tb_name, primary_key = "ID")

    # Advance the progress bar
    pb()

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
#' @param column_to_select <`character vector`> To column from which the `IDs`
#' should be selected. Defaults to `ID`.
#' @param IDs <`character vector`> A character vector of all IDs to retrieve.
#' @param crs <`numeric`> EPSG coordinate reference system for which the data
#' should be transformed.
#'
#' @return A tibble or an sf tibble depending if geometry is available.
#' @export
db_read_data <- function(table, columns = "*", column_to_select = "ID", IDs,
                         crs = 3347) {

  # If not retrieve all columns, make sure to retrieve IDs
  if (length(columns) != 1 && all(columns != "*")) {
    all_cols <- db_query(type = "get", statement = sprintf("SELECT COLUMN_NAME
          FROM INFORMATION_SCHEMA.COLUMNS
          WHERE table_name = '%s'", table))$COLUMN_NAME

    geo_cols <- all_cols[grepl("geometry_", all_cols)]
    columns <- columns[columns %in% all_cols]

    columns <- c("ID", columns, geo_cols)
    columns <- paste0(columns, collapse = ", ")
  }

  # Split in multiple smaller calls
  rows_calls <- 100
  ids <- suppressWarnings(split(IDs, 1:ceiling(length(IDs)/rows_calls)))
  while (length(ids) > 2500) {
    rows_calls <- rows_calls * 1.2
    ids <- suppressWarnings(split(IDs, 1:ceiling(length(IDs)/rows_calls)))
  }


  # Construct the calls
  ids <- lapply(ids, \(x) paste0(column_to_select, " = '", x, "'"))
  query <- lapply(ids, \(x) paste(
    "(SELECT ", columns, " FROM ", table, " WHERE", paste0(x, collapse = " OR "), ")"
  ))
  query <- paste0(query, collapse = " UNION ALL ")

  # Call and consolidate
  df <- db_query(type = "get", statement = query)
  df <- df[, names(df) != "row_names"]
  df <- tibble::as_tibble(df)

  # Convert each column to their right class
  df <- lapply(names(df), \(col) {
    out <- tibble::tibble(.rows = nrow(df))
    out[[col]] <- if (grepl("ID$", col)) as.character(df[[col]]) else
      utils::type.convert(df[[col]], as.is = TRUE)
    out
  })
  df <- tibble::as_tibble(Reduce(cbind, df))

  # If it is geometry, convert it to sf
  if (sum(grepl("geometry", names(df))) > 0) {
    dfs <- suppressWarnings(split(df, 1:100))

    pb <- progressr::progressor(steps = length(dfs))
    dfs <- future.apply::future_lapply(dfs, \(df) {
      geo_columns <- names(df)[grepl("geometry_", names(df))]

      df$geometry <-
        lapply(seq_len(nrow(df)), \(x) {
          df[, geo_columns][is.na(df[, geo_columns])] <- ""
          paste0(df[x, geo_columns], collapse = "")
        })

      df <- df[, !grepl("geometry_", names(df))]

      df$geometry <-
        sapply(seq_along(df$geometry), \(x) {
          wkb::hex2raw(df$geometry[x])[[1]] |>
            sf::st_as_sfc(crs = 3347)
        })

      pb()
      df
    }, future.seed = NULL)

    df <- data.table::rbindlist(dfs)
    df <- sf::st_as_sf(tibble::as_tibble(df), crs = 3347)
    df <- sf::st_transform(df, crs)

  }

  return(df)
}

#' Write long table to the MySQL database
#'
#' @param df <`data.frame`> A data.frame or an sf data.frame containing a large
#' number of rows, big enough that it will need its own DA dictionary.
#' @param tb_name <`character`> The name of the table to be in the database,
#' e.g. `buildings` or `street`.
#'
#' @return Returns an error or nothing if ran successfully. The `df` is saved
#' and the name of the table corresponds to `tb_name` in the database. And there
#' is another `tb_name_DA_dict` table which is DA dictionary. All ID of `df` are
#' regrouped per DA ID in a JSON column.
#' @export
db_write_long_table <- function(df, tb_name) {

  if (!"DA_ID" %in% names(df))
    stop(paste0("There must be a `DA_ID` column for referent in the `df` ",
                "table to write a long table in the db."))

  # Write normal df table
  db_write_table(df = df, tb_name = tb_name, index = "ID",
                 rows_per_append_wave = 50000)

  # Create a dictionary from which to retrieve all df ID from DA IDs
  df <- sf::st_drop_geometry(df)
  dict <- split(df$ID, df$DA_ID)

  tb <- lapply(seq_along(dict), \(x) {
    y <- dict[[x]]

    if (length(y) > 2000) {

      y <- suppressWarnings(split(y, 1:ceiling(length(y)/2000)))

      out <- lapply(y, \(z) {
        tibble::tibble(DA_ID = names(dict[x]),
                       IDs = jsonlite::toJSON(z))
      })

      tibble::as_tibble(data.table::rbindlist(out))

    } else {
      tibble::tibble(DA_ID = names(dict[x]),
                     IDs = jsonlite::toJSON(y))
    }
  })

  out <- data.table::rbindlist(tb)
  out <- tibble::as_tibble(out)

  db_write_table(df = out,
                 tb_name = paste0(tb_name, "_DA_dict"),
                 index = FALSE)

}

#' Read long table from the MySQL database
#'
#' Read data from a long table. The table must have been previously created
#' using \code{\link[cc.data]{db_write_long_table}}
#'
#' @param table <`character`> The table name in the MySQL database. To list all
#' tables, use \code{\link[DBI]{dbListTables}}.
#' @param DA_ID <`vector of character`> DA IDs from which to retrieve all their
#' spatial features from the `table`.
#'
#' @return A tibble or an sf tibble depending if geometry is available.
#' @export
db_read_long_table <- function(table, DA_ID) {

  # Read from DA dictionary
  ids <- db_read_data(table = paste0(table, "_DA_dict"),
                      column_to_select = "DA_ID",
                      IDs = DA_ID)
  ids <- unlist(sapply(ids$IDs, jsonlite::fromJSON, USE.NAMES = FALSE))

  # Read from the database
  db_read_data(table = table,
               IDs = ids)

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
    db_query(type = "execute", sprintf(
      "CREATE USER %s IDENTIFIED BY '%s';",
      user, password
    ))

  rights <- if (admin) "ALL PRIVILEGES" else "SELECT, SHOW VIEW"

  grant <-
    db_query(type = "execute", sprintf(
      "GRANT %s ON ccdb.* TO %s@'%%';",
      rights, user
    ))
  if (admin) {
    db_query(type = "execute", sprintf(
      "GRANT `rds_superuser_role`@`%%` TO `%s`@`%%`",
      user
    ))
  }
  flush <- db_query(type = "execute", "FLUSH PRIVILEGES;")

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
