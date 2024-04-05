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
    drv = RMySQL::MySQL(max.con = 100),
    username = Sys.getenv("CURBCUT_DB_USER"),
    password = Sys.getenv("CURBCUT_DB_PASSWORD"),
    host = "ccdb-instance-1.cplnwzthenux.us-east-1.rds.amazonaws.com",
    port = 3306,
    dbname = "ccdb"
  )
}

#' List all tables in the MySQL database
#'
#' Connects to the database, lists all tables in it, and then disconnects.
#'
#' @return A character vector of table names.
#' @export
db_list_tables <- function() {

  # Connect to the database
  conn <- db_connect()

  tryCatch({out <- DBI::dbListTables(conn)},
           error = function(e) {
             db_disconnect(conn)
             stop(e)
           }
  )

  # Disconnect
  db_disconnect(conn)

  # Return
  return(out)

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

  if (type %in% c("write", "execute", "append")) {
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

#' Write data frame to database table
#'
#' This function writes a data frame to a database table. If a primary key is
#' provided, the function creates a table with the primary key for faster
#' retrieval and appends the data by waves. If an index is provided, the function
#' creates an index on the index column for faster retrieval.
#'
#' @param df <`data.frame`> A non-sf data.frame to be written to the MySQL
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
#' @return NULL
db_write_table_helper <- function(df, tb_name, primary_key = NULL, index = NULL,
                                 rows_per_append_wave = 1000) {
  if (!is.null(primary_key)) {
    # Create the table with the primary key for faster retrieval
    cols <-
      paste0(paste(
        names(df), "VARCHAR(",
        sapply(seq_len(ncol(df)),
               \(x) {
                 n <- suppressWarnings(max(nchar(df[[x]]), na.rm = TRUE))
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

    # Append the table's data by waves of x rows
    waves <- split(df, 1:ceiling(nrow(df) / rows_per_append_wave)) |>
      suppressWarnings()

    pb <- progressr::progressor(steps = length(waves))
    lapply(waves, \(x) {
      code <- db_query(type = "append", statement = x, name = tb_name)
      db_query(type = "execute", code)
      pb()
    })
  } else {
    db_query(type = "write", statement = df, name = tb_name)
  }

  if (!is.null(index)) {
    # Create an index on ID for faster retrieval
    # Modify the column first so that it's character type
    max_cell_size <- max(nchar(df[[index]]), na.rm = TRUE)
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

    # Keep constant over the database
    df <- sf::st_transform(df, 3347)

    # Get a df without geometry column
    df_no_geo <- sf::st_drop_geometry(df)

    # Get the geometry as hex for better storage. The maximum size of a cell
    # is around 65k characters, so split for as many columns as needed.

    hexes <-
      lapply(seq_len(nrow(df_no_geo)), \(x) {
        hex <- sf::st_as_binary(df$geometry[x], hex = TRUE)
        if (nchar(hex) > 45000) {
          split_every <- 45000
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

  # If there are too many columns and it causes the rows to be too large in size,
  # split the df until it's split small enough to fit.
  succeed <- FALSE
  split_df <- 2
  attempt_count <- 0
  max_attempts <- 5

  while (!succeed & attempt_count < max_attempts) {

    attempt_count <- attempt_count + 1

    tryCatch({
      if (ncol(df_no_geo) > 1017) {
        ind <- suppressWarnings(split(seq_len(ncol(df_no_geo)), 1:split_df))

        # Split the dataframe and keep `ID` in all
        out_df <- lapply(ind, \(i) df_no_geo[, unique(c(1, i))])

        # Split the table names
        new_tb_name <- paste0(tb_name, "_part", 1:split_df)
      } else {
        out_df <- list(df_no_geo)
        new_tb_name <- tb_name
      }

      # Remove if existing table
      if (length(new_tb_name) > 1) {
        all_tb <- db_list_tables()
        shared_name <- all_tb[grepl(tb_name, all_tb)]
        to_delete <- shared_name[grepl("_part\\d*$", shared_name)]
        sapply(to_delete, \(x) db_query("remove", x))
      } else db_query("remove", tb_name)

      # Save all the tables
      mapply(\(df, name) {
        db_write_table_helper(df = df, tb_name = name,
                              primary_key = primary_key,
                              index = index,
                              rows_per_append_wave = rows_per_append_wave)
      }, out_df, new_tb_name)

      succeed <<- TRUE
      break

    }, error = function(e) {
      split_df <<- split_df * 2
      if (!grepl("Row size too large", e)) {
        if (attempt_count >= max_attempts) {
          stop(print(e))
        }
      }
    })
  }


  return(invisible(NULL))
}

#' Write the list of processed census data to the MySQL database
#'
#' @param processed_census_full_geos <`named list`> The finalized process data
#' normally coming out of the final processing, with the full geometries using:
#' \code{\link[cc.data]{census_full_geos}}
#'
#' @return Returns an error or nothing if ran successfully. All tables in the
#' list fed to `processed_census_full_geos` are written to the MySQL database with
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

      # Filter out very large areas to respect the MySQL cell limit
      tb$area <- get_area(tb)
      very_large_geos <- which(tb$area >= 1e11)
      if (length(very_large_geos) > 0) {
        tb <- tb[-very_large_geos, ]
      }
      tb$area <- NULL

      db_write_table(df = tb, tb_name = tb_name, primary_key = "ID",
                     rows_per_append_wave = 100)

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

#' Write a Travel Time Matrix to Database
#'
#' This function writes a travel time matrix (TTM) from a specified mode of transport
#' and zip file to a database, with the option to overwrite existing data.
#' Supported modes are "foot", "bicycle", "car", or "transit". The function first checks
#' if the table for the specified mode exists and, based on the `overwrite` parameter,
#' either updates the table or prompts the user to confirm overwriting.
#'
#' @param mode <`character`> The mode of transport for the TTM. Valid values are
#' "foot", "bicycle", "car", or "transit".
#' @param zip_file_path <`character`> The file path to the zip file containing the TTM data.
#' @param overwrite <`logical`> If `TRUE`, existing data for the specified mode will
#' be overwritten. If `FALSE`, the function will append new data without deleting existing entries.
#'
#' @details The function performs several steps: it first lists and filters files within the zip
#' archive, connects to a database, and then based on the `overwrite` parameter and user confirmation,
#' decides whether to recreate the table for the TTM data. It processes the data in batches for efficiency
#' and uses a retry mechanism for database operations to handle potential transient errors.
#'
#' @return Invisible. The function is called for its side effect of writing data to a database.
#' @export
db_write_ttm <- function(mode, zip_file_path, overwrite) {
  # List and filter your files
  files_in_zip <- unzip(zip_file_path, list = TRUE)$Name
  files_in_mode_folder <- grep(sprintf("^%s/.*\\.qs$", mode), files_in_zip, value = TRUE)

  # Connect to the database
  conn <- db_connect()
  on.exit(db_disconnect(conn), add = TRUE)

  table_name <- sprintf("ttm_%s_DB", mode)

  # Check if table exists
  table_exists <- DBI::dbExistsTable(conn, table_name)

  # If the table doesn't exist
  if (!table_exists || overwrite) {
    if (table_exists) {
      cat(sprintf(paste0("The table `%s` already exists. Recreating the table is a ",
                         "multi-day uploading process. Do you want to remove and ",
                         "recreate it? (y/n): "), table_name))
      confirm <- readline() # Capture user input from the console
      if (confirm != "y") stop()

      DBI::dbRemoveTable(conn, table_name)
    }

    # Create the table
    DBI::dbExecute(conn, sprintf("CREATE TABLE IF NOT EXISTS %s (
  `from` VARCHAR(255),
  `to` VARCHAR(255),
  travel_seconds INT
)", table_name))
  }

  # In the case where overwrite is FALSE, we do not re-upload already present data
  present_froms <- sprintf("SELECT DISTINCT `from` FROM %s ORDER BY `from`", table_name)
  present_froms <- DBI::dbGetQuery(conn, present_froms)
  present_froms_as_path <- sprintf("%s/%s.qs", mode, present_froms$from)

  db_disconnect(conn)

  # Function to process files in batch and insert into the database
  process_files_batch <- function(file_names, zip_file_path, table_name, .progress) {

    batch_csv <- tempfile(fileext = ".csv")
    on.exit(unlink(batch_csv))

    for (file_name in file_names) {
      # If it's already present, pass to the next
      if (!overwrite & file_name %in% present_froms_as_path) next
      tmp <- tempfile(fileext = ".qs")
      on.exit(unlink(tmp), add = TRUE)

      utils::unzip(zipfile = zip_file_path, files = file_name, exdir = tmp)
      dat <- qs::qread(file.path(tmp, file_name))
      if (nrow(dat) == 0) next

      # Assuming 'from' is at dat[2] and 'to' is at dat[1]
      dat <- data.frame(
        from = names(dat)[2],
        to = dat[[1]],
        travel_seconds = dat[[2]],
        stringsAsFactors = FALSE
      )

      # Append to batch CSV
      write.table(dat, file = batch_csv, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE)
    }

    # Bulk insert the batch
    batch_csv <- gsub("\\\\", "/", batch_csv) # Ensure correct path format

    # Retry mechanism for database operation
    max_retries <- 3 # Maximum number of retries
    for (attempt in 1:max_retries) {
      conn <- NULL # Initialize connection outside tryCatch to ensure visibility for on.exit

      tryCatch({
        conn <- db_connect() # Attempt to connect to the database
        on.exit({
          if (!is.null(conn)) {
            db_disconnect(conn) # Ensure disconnection on exit
          }
        }, add = TRUE)

        query <- sprintf("LOAD DATA LOCAL INFILE '%s' INTO TABLE %s FIELDS TERMINATED BY ',' LINES TERMINATED BY '\\n'", batch_csv, table_name)
        if (file.exists(batch_csv)) {
          DBI::dbExecute(conn, query)
          break # Success, exit the loop
        }
      }, error = function(e) {
        if (attempt == max_retries) {
          stop("Failed after max retries: ", e$message)
        } else {
          message(sprintf("Attempt %d failed, retrying... Error: %s", attempt, e$message))
          Sys.sleep(5) # Wait for 5 seconds before retrying
        }
      })
    }

    .progress()

    return()
  }

  # Initialize progressr
  progressr::with_progress({
    p <- progressr::progressor(steps = ceiling(length(files_in_mode_folder)/100))

    # Process files in parallel with progress reporting
    future.apply::future_lapply(split(files_in_mode_folder, ceiling(seq_along(files_in_mode_folder)/100)),
                                process_files_batch, zip_file_path = zip_file_path,
                                table_name = table_name, .progress = p)
  })

  # Construct the SQL statement for adding an index
  ind_name <- sprintf("%s_indexfrom", table_name)
  sql_add_index <- sprintf("ALTER TABLE %s ADD INDEX %s (`from`)", table_name, ind_name)
  conn <- db_connect()
  on.exit(db_disconnect(conn), add = TRUE)
  DBI::dbExecute(conn, sql_add_index)
  db_disconnect(conn)

}

#' Read and return a travel time matrix from a database
#'
#' @param mode <`character`> The mode of transportation (e.g. "car", "bicycle",
#' "foot", "transit_pwd", ...)
#' @param DA_ID <`character`> A vector of DA IDs to retrieve data for
#'
#' @return A data frame containing the travel time matrix
#' @export
db_read_ttm <- function(mode, DA_ID) {

  ttm_mode <- paste("ttm", mode, sep = "_")

  pb <- progressr::progressor(steps = length(DA_ID))

  ttms <- future.apply::future_sapply(DA_ID, \(ID) {
    # Build the name of the dataframe to retrieve
    ttm_mode_id <- paste(ttm_mode, ID, sep = "_")

    # Get
    out <-
      tryCatch(
        db_query(type = "get", statement = paste("SELECT * FROM", ttm_mode_id)),
        error = function(e) {
          # If table is inexistant, create it as empty
          if (grepl("Table '.*' doesn't exist", e)) return(tibble::tibble())
          stop(e)
        })

    # Add self
    self <- tibble::tibble(DA_ID = ID)
    self[[as.character(ID)]] <- 0
    out <- rbind(self, out[, names(out) != "row_names"])

    pb()

    # Return
    return(out)
  }, simplify = FALSE, USE.NAMES = TRUE)

  # Return a traveltime-matrix style data.frame
  return(ttms)

}

#' Helper function to read data from a database table based on selected columns
#' and IDs
#'
#' @param table <`character`> The table name in the MySQL database. To list all
#' tables, use \code{\link[DBI]{dbListTables}}.
#' @param columns <`character vector`> A character vector of all columns to
#' retrieve. By default, all columns (`*`).
#' @param column_to_select <`character vector`> To column from which the `IDs`
#' should be selected. Defaults to `ID`.
#' @param IDs <`character vector`> A character vector of all IDs to retrieve.
#'
#' @return A tibble containing the retrieved data from the database table.
#'
#' @details
#' If columns are specified, this function will retrieve the specified columns
#' and IDs. If "*" is used or the columns argument, all columns will be retrieved.
#'
#' The function will split the IDs into multiple smaller calls if the length of
#' the IDs exceeds 2500.
#'
#' The retrieved data is returned as a tibble. The function converts each column
#' to its right class based on its data type. ID columns are converted to
#' character and all other columns are converted using the utils::type.convert()
#' function.
db_read_data_helper <- function(table, columns = "*", column_to_select = "ID",
                                IDs = NULL) {
  # Determine the selection mode based on presence of IDs
  if (is.null(IDs)) {
    selection_mode <- "ALL"
  } else {
    selection_mode <- "SELECTED"
  }

  # If not retrieve all columns, make sure to retrieve IDs if in SELECTED mode
  if (length(columns) != 1 && all(columns != "*") && selection_mode == "SELECTED") {
    all_cols <- db_query(type = "get", statement = sprintf("SELECT COLUMN_NAME
          FROM INFORMATION_SCHEMA.COLUMNS
          WHERE table_name = '%s'", table))$COLUMN_NAME

    geo_cols <- all_cols[grepl("geometry_", all_cols)]
    columns <- columns[columns %in% all_cols]

    columns <- c(column_to_select, all_cols[grepl("ID$", all_cols)], columns, geo_cols) |> unique()
    columns <- paste0(columns, collapse = ", ")
  }

  # Split in multiple smaller calls if IDs are provided
  if (selection_mode == "SELECTED") {
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
  } else {
    # Construct a single query for all rows
    query <- paste0("SELECT ", columns, " FROM ", table)
  }

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

  # Return
  return(df)
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
#' @param crs <`numeric`> EPSG coordinate reference system to which the data
#' should be transformed.
#' @param keep_geometry <`logical`> Should the geometry column be kept? Defaults
#' to `TRUE`. Much faster if they are not kept.
#'
#' @return A tibble or an sf tibble depending if geometry is available.
#' @export
db_read_data <- function(table, columns = "*", column_to_select = "ID", IDs = NULL,
                         crs = 3347, keep_geometry = TRUE) {

  all_tb <- db_list_tables()
  shared_name <- all_tb[grepl(table, all_tb)]
  to_retrieve <- shared_name[grepl("_part\\d*$", shared_name)]

  df <- if (length(to_retrieve) == 0) {
    out <- db_read_data_helper(table = table, columns = columns,
                               column_to_select = column_to_select, IDs = IDs)
    # Ensure all out are unique
    unique(out)
  } else {
    dfs <- lapply(to_retrieve, \(x) {
      out <- db_read_data_helper(table = x, columns = columns,
                                 column_to_select = column_to_select, IDs = IDs)
      # Ensure all out are unique
      unique(out)
    })
    Reduce(\(x, y) base::merge(x, y, by = "ID", all = TRUE), dfs) |>
      tibble::as_tibble()
  }

  # If it is geometry, convert it to sf
  if (keep_geometry) {
    if (sum(grepl("geometry", names(df))) > 0) {
      dfs <- suppressWarnings(split(df, 1:100))

      pb <- progressr::progressor(steps = length(dfs))
      dfs <- future.apply::future_lapply(dfs, \(df) {

        geo_cols_logic <- grepl("geometry_", names(df))
        geo_columns <- names(df)[geo_cols_logic]

        # Order the geo columns
        ind <- as.numeric(gsub("geometry_", "", geo_columns)) |> order()
        geo_columns <- geo_columns[ind]
        col_order <- c(names(df)[!geo_cols_logic], geo_columns)
        df <- df[, col_order]

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
  }

  if (!keep_geometry) {
    df <- df[-which(grepl("geometry", names(df)))]
  }

  return(df)
}

#' Read all data from a specified table
#'
#' @param table <`character`> String indicating the name of the table to read
#'
#' @return A data frame containing all data from the specified table
#' @export
db_read_all_table <- function(table) {
  tryCatch({
    conn <- db_connect()
    out <- DBI::dbReadTable(conn = conn, name = table)
    DBI::dbDisconnect(conn)
    tibble::as_tibble(out)
  },
  error = function(e) {
    message(paste0("Connection to the database failed or download failed for table `", table ,
                 "`. Retrying... . Error message:", e))
    if (exists("conn") && class(conn) == "MySQLConnection")
      DBI::dbDisconnect(conn)
    cc.data::db_read_all_table(table = table)
  })
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
