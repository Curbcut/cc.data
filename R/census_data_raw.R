#' Get census vectors values
#'
#' @param empty_geometries <`list of sf data.frame`> The output of
#' \code{\link[cc.data]{census_empty_geometries}}.
#' @param census_vectors_table <`data.frame`> Should be equal to
#' \code{\link[cc.data]{census_vectors_table}}
#' @param census_scales <`character vector`> Should be equal to
#' \code{\link[cc.data]{census_scales}}
#' @param census_years <`numeric vector`> Should be equal to
#' \code{\link[cc.data]{census_years}}
#'
#' @return A list of scales and years of census data accompanied with the
#' parent values.
#' @export
census_data_raw <- function(empty_geometries,
                            census_scales = cc.data::census_scales,
                            census_years = cc.data::census_years) {

  pb <- progressr::progressor(steps = length(census_scales) * length(census_years))

  data_raw <-
    sapply(census_scales, \(scale) {
      sapply(as.character(census_years), \(year) {

        # Relevant named vectors
        vecs <-
          cc.data::census_vectors_table[, c("var_code", paste0("vec_", year))]
        vecs <- vecs[!is.na(vecs[[paste0("vec_", year)]]), ]
        var_codes <- vecs[[paste0("vec_", year)]]
        names(var_codes) <- vecs$var_code
        var_codes <- var_codes[!is.na(var_codes)]
        # With names, add a number in cases a variable needs to be a sum
        var_codes <-
          mapply(\(vec, name) {
            vec <- unname(vec)
            names(vec) <- paste0(name, "_", seq_along(vec))
            vec
          }, var_codes, names(var_codes), USE.NAMES = FALSE) |> unlist()

        # Current census dataset
        census_dataset <- paste0("CA", sub("20", "", year))

        # Some vectors are retrieved and are duplicates. Split duplicates in
        # multiple list elements to retrieve them before binding them
        # Find duplicated codes with different names and their corresponding names
        dup_codes <- duplicated(var_codes) | duplicated(var_codes, fromLast = TRUE)
        dup_names <- duplicated(names(var_codes)) | duplicated(names(var_codes), fromLast = TRUE)
        dup_diff <- dup_codes & !dup_names
        split_names <- split(names(var_codes), cumsum(dup_diff))

        # Split the vector into a list of 2 based on the duplicated codes with different names
        split_var_codes <- split(var_codes, cumsum(dup_diff))

        # Combine the split names and vectors into a named list
        named_vecs <- Map(stats::setNames, split_var_codes, split_names)

        # Get the variable values
        dat <-
          lapply(named_vecs, \(var_codes) {

            dat <- if (scale == "DA") {
              # Troubles with getting DA nation-wide. Get provinces and bind.
              pr_codes <- cancensus::list_census_regions(census_dataset, quiet = TRUE)
              pr_codes <- pr_codes$region[pr_codes$level == "PR"]
              pr_codes <- lapply(pr_codes, \(x) list(PR = x))
              all_pr_vecs <- lapply(pr_codes, \(reg) {
                cancensus::get_census(
                  dataset = census_dataset,
                  regions = reg,
                  level = scale,
                  vectors = unlist(var_codes),
                  geo_format = NA,
                  quiet = TRUE
                )
              })
              Reduce(rbind, all_pr_vecs)
            } else {
              cancensus::get_census(
                dataset = census_dataset,
                regions = list(C = "01"),
                level = scale,
                vectors = unlist(var_codes),
                geo_format = NA,
                quiet = TRUE
              )
            }

            dat <- dat[, c("GeoUID", names(var_codes))]
            names(dat)[1] <- "ID"

            return(dat)
          })

        dat <- Reduce(\(x,y) merge(x, y, all = TRUE, by = "ID"), dat)

        # Addition additive variables
        tb <- table(gsub("_[0-9]$", "", names(dat)))
        to_add <- names(tb[tb > 1])

        to_add_done <- sapply(to_add, \(x) {
          dat[grepl(x, names(dat))] |> rowSums()
        }, USE.NAMES = TRUE)

        dat <- dat[, !grepl(paste0(to_add, collapse = "|"), names(dat))]
        dat <- tibble::as_tibble(cbind(dat, to_add_done))

        names(dat) <- gsub("_[0-9]$", "", names(dat))

        # Return
        pb()
        return(dat)
      }, simplify = FALSE, USE.NAMES = TRUE)
    }, simplify = FALSE, USE.NAMES = TRUE)

  # Bind the results to empty geometries
  future.apply::future_mapply(\(data_r, empty_g) {
    future.apply::future_mapply(\(d, e) {
      sf::st_as_sf(tibble::as_tibble(merge(d, e, by = "ID")))
    }, data_r, empty_g, SIMPLIFY = FALSE, USE.NAMES = TRUE, future.seed = NULL)
  }, data_raw, empty_geometries,
  SIMPLIFY = FALSE, USE.NAMES = TRUE,
  future.seed = NULL
  )
}
