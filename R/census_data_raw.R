#' Get census vectors values
#'
#' @param empty_geometries <`list of sf data.frame`> The output of
#' \code{\link[cc.data]{census_empty_geometries}}.
#' @param census_vectors <`data.frame`> Should be equal to
#' \code{\link[cc.data]{census_vectors}}
#' @param census_scales <`character vector`> Should be equal to
#' \code{\link[cc.data]{census_scales}}
#' @param census_years <`numeric vector`> Should be equal to
#' \code{\link[cc.data]{census_years}}
#'
#' @return A list of scales and years of census data accompanied with the
#' parent values.
#' @export
census_data_raw <- function(empty_geometries,
                            census_vectors = cc.data::census_vectors,
                            census_scales = cc.data::census_scales,
                            census_years = cc.data::census_years) {

  # dat <- census_data[census_data$var_code == census_vectors, ]

  pb <- progressr::progressor(steps = length(census_scales) * length(census_years))

  data_raw <-
    future.apply::future_sapply(census_scales, \(scale) {
      future.apply::future_sapply(as.character(census_years), \(year) {

        # Relevant named vectors
        vecs <-
          census_vectors[, c("var_code", paste0("vec_", year), "parent_vectors")]
        vecs <- vecs[!is.na(vecs[[paste0("vec_", year)]]), ]
        var_codes <- vecs[[paste0("vec_", year)]]
        names(var_codes) <- vecs$var_code
        var_codes <- var_codes[!is.na(var_codes)]
        # With names, add a number in cases a variable needs to be a sum
        var_codes <-
          mapply(\(vec, name) {
            vec <- unname(vec)
            sapply(vec, \(v) {
              names(v) <- paste0(name, "_", which(v == vec))
              v
            }, USE.NAMES = FALSE)
          }, var_codes, names(var_codes), USE.NAMES = FALSE) |> unlist()

        # Current census dataset
        census_dataset <- paste0("CA", sub("20", "", year))

        # Get the variable values
        dat <-  if (scale == "DA") {
          # Troubles with getting DA nation-wide. Get provinces and bind.
          pr_codes <- cancensus::list_census_regions(census_dataset)
          pr_codes <- pr_codes$region[pr_codes$level == "PR"]
          pr_codes <- lapply(pr_codes, \(x) list(PR = x))
          all_pr_vecs <- lapply(pr_codes, \(reg) {
            cancensus::get_census(
              dataset = census_dataset,
              regions = reg,
              level = scale,
              vectors = unlist(var_codes),
              geo_format = NA,
              quiet = TRUE)
          })
          Reduce(rbind, all_pr_vecs)
        } else {
          cancensus::get_census(
            dataset = census_dataset,
            regions = list(C = "01"),
            level = scale,
            vectors = unlist(var_codes),
            geo_format = NA,
            quiet = TRUE)
        }

        dat <- dat[, c("GeoUID", names(var_codes))]
        names(dat)[1] <- "ID"

        # Addition additive variables
        tb <- table(gsub("_[0-9]$", "", names(dat)))
        to_add <- names(tb[tb > 1])

        to_add_done <- sapply(to_add, \(x) {
          dat[grepl(x, names(dat))] |> rowSums()
        }, USE.NAMES = TRUE)

        dat <- dat[, !grepl(paste0(to_add, collapse = "|"), names(dat))]
        dat <- tibble::as_tibble(cbind(dat, to_add_done))

        names(dat) <- gsub("_[0-9]$", "", names(dat))

        # Get parent vectors
        pv <- cancensus::list_census_vectors(census_dataset)
        pv <- pv[pv$vector %in% var_codes, ]

        pv_vec <- ifelse(is.na(pv$parent_vector), pv$aggregation, pv$parent_vector)
        pv_vec <- gsub(".* ", "", pv_vec)

        pv <- cbind(pv["vector"], parent_vector = pv_vec)
        pv <- merge(tibble::tibble(var_code = names(var_codes), var_codes), pv,
                    by.x = "var_codes", by.y = "vector")
        pv$var_code <- gsub("_[0-9]+$", "", pv$var_code)

        # Switch parent vectors if need be
        switch_pv <- vecs$parent_vectors
        names(switch_pv) <- vecs$var_code
        switch_pv <- switch_pv[!is.na(switch_pv)]
        switch_pv <- lapply(switch_pv, \(x) x[grepl(census_dataset, x)])
        switch_pv <- switch_pv[sapply(switch_pv, \(x) length(x) != 0)]
        switch_pv <-
          mapply(\(v, n) {
            v_t <- lapply(v, \(z) {
              tibble::tibble(var_code = n,
                             parent_vector = z)
            })
            Reduce(rbind, v_t)
          }, switch_pv, names(switch_pv), SIMPLIFY = FALSE)
        switch_pv <- Reduce(rbind, switch_pv)

        pv <- pv[!pv$var_code %in% switch_pv$var_code, ]

        pv <- rbind(pv[c("var_code", "parent_vector")], switch_pv)
        pv <- unique(pv)

        pv_vecs <- pv$parent_vector
        names(pv_vecs) <- pv$var_code

        unique_pv_vecs <- unique(pv_vecs)
        names(unique_pv_vecs) <- unique_pv_vecs

        # Unique retrieval for parents
        parents_ret <- if (scale == "DA") {
          pr_codes <- cancensus::list_census_regions(census_dataset)
          pr_codes <- pr_codes$region[pr_codes$level == "PR"]
          pr_codes <- lapply(pr_codes, \(x) list(PR = x))
          all_pr_vecs <- lapply(pr_codes, \(reg) {
            cancensus::get_census(
              dataset = census_dataset,
              regions = reg,
              level = scale,
              vectors = unique_pv_vecs,
              geo_format = NA,
              quiet = TRUE)
          })
          Reduce(rbind, all_pr_vecs)
        } else {
          cancensus::get_census(
            dataset = census_dataset,
            regions = list(C = "01"),
            level = scale,
            vectors = unique_pv_vecs,
            geo_format = NA,
            quiet = TRUE)
        }

        pv <- mapply(\(vec_name, vec) {
          out <- parents_ret[, c("GeoUID", vec)]
          names(out) <- c("ID", paste0(vec_name, "_parent"))
          out
        }, names(pv_vecs), pv_vecs, SIMPLIFY = FALSE, USE.NAMES = TRUE)

        # Sum the parents if a vector have multiple parents
        multiple_pv_names <- names(table(names(pv))[table(names(pv)) > 1])
        multiple_pv <- sapply(multiple_pv_names, \(x) {
          to_sum <- pv[names(pv) == x]
          tb <- to_sum[[1]]["ID"]
          psum <- function(x, y) mapply(sum, x, y, na.rm = TRUE)
          tb[[paste0(x, "_parent")]] <- Reduce(psum, lapply(to_sum, `[[`, 2))
          tb
        }, simplify = FALSE, USE.NAMES = TRUE)
        pv <- c(pv[!names(pv) %in% multiple_pv_names], multiple_pv)

        pv <- tibble::as_tibble(Reduce(merge, pv))

        # Bind variables values with parent vectors
        pb()
        tibble::as_tibble(merge(dat, pv, by = "ID"))

      }, simplify = FALSE, USE.NAMES = TRUE, future.seed = NULL)
    }, simplify = FALSE, USE.NAMES = TRUE, future.seed = NULL)

  # Bind the results to empty geometries
  future.apply::future_mapply(\(data_r, empty_g) {
    future.apply::future_mapply(\(d, e) {
      sf::st_as_sf(tibble::as_tibble(merge(d, e, by = "ID")))
    }, data_r, empty_g, SIMPLIFY = FALSE, USE.NAMES = TRUE, future.seed = NULL)
  }, data_raw, empty_geometries, SIMPLIFY = FALSE, USE.NAMES = TRUE,
  future.seed = NULL)

}
