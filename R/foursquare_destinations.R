## ============================================================================
##  foursquare_destinations.R — Foursquare places → Curbcut destination types
##
##  A place belongs to a destination type when one of its Foursquare category
##  IDs is in that type's `include` list, unless it also carries a category in
##  `exclude_if_also` (and none in `keep_if`). The lists live in
##  inst/extdata/foursquare_destinations.json, generated from Foursquare's
##  taxonomy by data-raw/foursquare_destinations.R. Types are independent: a
##  supermarket with a pharmacy counter is both a grocery store and a pharmacy.
##
##  After the category match, two clean-ups run per type:
##    - names: listings whose name shows another kind of business (a liquor
##      store filed as a grocery store, a bitcoin ATM filed as a bank) are
##      dropped, see foursquare_name_rules() in foursquare_name_rules.R;
##    - duplicates: two listings of the same business within 100 m, and for
##      pharmacies and dentists an owner's or practitioner's personal listing
##      within 60 m of the branded one, keep one listing.
##
##  Public functions:
##    foursquare_destination_map()       — read the category map
##    foursquare_destinations_sql()      — Snowflake query for every mapped place
##    foursquare_categories_check_sql()  — Snowflake query for the mapped categories
##    foursquare_check_categories()      — compare the map to the Snowflake share
##    foursquare_get_destinations()      — run the query on a Snowflake connection
##    foursquare_type_flags()            — which types each place belongs to
##    foursquare_classify_names()        — name check for one type
##    foursquare_brand_key()             — brand key used to spot duplicates
##    foursquare_dedup()                 — duplicate listings within one type
##    foursquare_prepare_destinations()  — all of the above, ready to upload
## ============================================================================


#' Foursquare category map for Curbcut destination types
#'
#' @param path Path to `foursquare_destinations.json`. Defaults to the copy
#'   installed with the package.
#'
#' @return A list with `taxonomy` (where the category IDs come from) and
#'   `types`: one entry per destination type with `type_id`, `include`,
#'   `exclude_if_also` and `keep_if`, each a list of `list(id, label)`.
#' @export
foursquare_destination_map <- function(
    path = system.file("extdata", "foursquare_destinations.json", package = "cc.data")) {
  if (!nzchar(path) || !file.exists(path)) {
    stop("foursquare_destinations.json not found; pass `path` (inst/extdata/ in cc.data).")
  }
  jsonlite::read_json(path, simplifyVector = FALSE)
}

.fsq_ids <- function(x) vapply(x, function(e) e$id, character(1))


#' Snowflake query for every place in a mapped Foursquare category
#'
#' Selects open Canadian places (`date_closed IS NULL`) carrying at least one
#' category in any type's `include` list, with their category IDs,
#' `date_refreshed` and `unresolved_flags` (user reports Foursquare hasn't
#' resolved). Type membership, freshness and closure reports are applied
#' afterwards by [foursquare_prepare_destinations()], so they can be measured.
#'
#' @param map Output of [foursquare_destination_map()].
#' @param table Fully qualified places table.
#' @return A character string (one SQL statement).
#' @export
foursquare_destinations_sql <- function(map = foursquare_destination_map(),
                                        table = "FSQ_OPEN_SOURCE_PLACES.FSQ_OS.PLACES") {
  ids <- unique(unlist(lapply(map$types, function(t) .fsq_ids(t$include))))
  sprintf(
    "SELECT fsq_place_id, name, latitude, longitude, address, locality, region, postcode,
       fsq_category_ids, fsq_category_labels, unresolved_flags, date_created, date_refreshed
FROM %s
WHERE country = 'CA'
  AND latitude IS NOT NULL AND longitude IS NOT NULL
  AND date_closed IS NULL
  AND ARRAYS_OVERLAP(fsq_category_ids, ARRAY_CONSTRUCT(%s))",
    table, paste0("'", ids, "'", collapse = ", ")
  )
}


#' Snowflake query for the categories the map uses
#'
#' @inheritParams foursquare_destinations_sql
#' @param table Fully qualified categories table of the same share.
#' @return A character string (one SQL statement).
#' @export
foursquare_categories_check_sql <- function(map = foursquare_destination_map(),
                                            table = "FSQ_OPEN_SOURCE_PLACES.FSQ_OS.CATEGORIES") {
  ids <- unique(unlist(lapply(map$types, function(t)
    c(.fsq_ids(t$include), .fsq_ids(t$exclude_if_also), .fsq_ids(t$keep_if)))))
  sprintf("SELECT category_id, category_label FROM %s WHERE category_id IN (%s)",
          table, paste0("'", ids, "'", collapse = ", "))
}


#' Compare the category map to the Snowflake share's own taxonomy
#'
#' @param categories A data frame with `category_id` and `category_label`, as
#'   returned by running [foursquare_categories_check_sql()].
#' @inheritParams foursquare_destinations_sql
#' @return A data frame of mapped categories whose ID is missing from the share
#'   or whose label differs. Zero rows means the map matches the share.
#' @export
foursquare_check_categories <- function(categories, map = foursquare_destination_map()) {
  names(categories) <- tolower(names(categories))
  mapped <- unique(do.call(rbind, lapply(names(map$types), function(t) {
    e <- c(map$types[[t]]$include, map$types[[t]]$exclude_if_also, map$types[[t]]$keep_if)
    data.frame(category_id = .fsq_ids(e),
               map_label = vapply(e, function(x) x$label, character(1)))
  })))
  m <- merge(mapped, categories, by = "category_id", all.x = TRUE)
  m[is.na(m$category_label) | m$category_label != m$map_label, , drop = FALSE]
}


#' Run the destinations query on a Snowflake connection
#'
#' @param conn A `DBI` connection from [foursquare_connect()].
#' @inheritParams foursquare_destinations_sql
#' @return A data frame, one row per place, with `fsq_category_ids` as a list
#'   column of character vectors.
#' @export
foursquare_get_destinations <- function(conn, map = foursquare_destination_map()) {
  message("Retrieving mapped Foursquare places for Canada...")
  df <- DBI::dbGetQuery(conn, foursquare_destinations_sql(map))
  names(df) <- tolower(names(df))
  df$fsq_category_ids <- .fsq_as_id_list(df$fsq_category_ids)
  df
}

## Snowflake ARRAY columns arrive as JSON text over ODBC and as lists from Arrow.
.fsq_as_id_list <- function(x) {
  if (is.list(x)) return(lapply(x, as.character))
  lapply(x, function(s) if (is.na(s) || !nzchar(s)) character(0) else
    as.character(unlist(jsonlite::fromJSON(s))))
}


#' Which destination types each place belongs to
#'
#' @param category_ids A list of character vectors (each place's
#'   `fsq_category_ids`).
#' @inheritParams foursquare_destinations_sql
#' @return A logical matrix, one row per place and one column per type.
#' @export
foursquare_type_flags <- function(category_ids, map = foursquare_destination_map()) {
  has_any <- function(set) vapply(category_ids, function(ids) any(ids %in% set), logical(1))
  out <- vapply(map$types, function(t) {
    inc  <- has_any(.fsq_ids(t$include))
    excl <- if (length(t$exclude_if_also)) has_any(.fsq_ids(t$exclude_if_also)) else FALSE
    keep <- if (length(t$keep_if)) has_any(.fsq_ids(t$keep_if)) else FALSE
    inc & (!excl | keep)
  }, logical(length(category_ids)))
  if (!is.matrix(out)) out <- matrix(out, nrow = length(category_ids), dimnames = list(NULL, names(map$types)))
  out
}


#' Normalise place names for the name rules
#'
#' Removes accents, lower-cases, and turns punctuation, apostrophes and
#' non-Latin characters into single spaces.
#' @keywords internal
fsq_norm <- function(x) {
  x <- stringi::stri_trans_nfkd(ifelse(is.na(x), "", x))
  x <- stringi::stri_replace_all_regex(x, "\\p{Mn}", "")
  x <- gsub("[^a-z0-9]+", " ", tolower(x))
  trimws(x)
}


#' Check place names against one type's name rules
#'
#' Order: hard exclusions, then banners that always count (`yes_first`), then
#' the other exclusions (groups whose name starts with `~` only flag a name as
#' unclear), then names that read as the type (`yes`). Everything else is
#' `unclear`: an independent business whose name says nothing either way.
#'
#' @param name Character vector of place names.
#' @param type A type name in [foursquare_name_rules()], e.g. `"grocery_store"`.
#' @param rules Output of [foursquare_name_rules()].
#' @return A data frame with `cls` (`"yes"`, `"no"` or `"unclear"`) and `reason`.
#' @export
foursquare_classify_names <- function(name, type, rules = foursquare_name_rules()) {
  r <- rules[[type]]
  if (is.null(r)) stop("No name rules for type '", type, "'.")
  n <- fsq_norm(name)
  cls <- rep(NA_character_, length(n))
  why <- rep(NA_character_, length(n))
  mark <- function(pattern, value, reason) {
    todo <- which(is.na(cls))
    hit <- todo[grepl(pattern, n[todo], perl = TRUE)]
    cls[hit] <<- value
    why[hit] <<- reason
  }
  empty <- n == ""
  cls[empty] <- "unclear"
  why[empty] <- "no_latin_name"
  for (k in r$hard_no) mark(r$no[[k]], "no", k)
  if (!is.null(r$yes_first)) mark(r$yes_first, "yes", "banner")
  for (k in setdiff(names(r$no), r$hard_no)) {
    soft <- startsWith(k, "~")
    mark(r$no[[k]], if (soft) "unclear" else "no", sub("^~", "", k))
  }
  mark(r$yes, "yes", "name")
  why[is.na(cls)] <- "unclassified"
  cls[is.na(cls)] <- "unclear"
  data.frame(cls = cls, reason = why, stringsAsFactors = FALSE)
}


#' Brand key used to spot duplicate listings
#'
#' Known aliases map to one key (Pharmaprix and Shoppers Drug Mart, Proxim and
#' Proximed, Caisse Desjardins and Caisse populaire...). Otherwise the key is
#' the first two significant words of the name, or three when the first is a
#' generic word such as "pizza". Names with no Latin letters key on the raw
#' name, so they only match an identical name.
#'
#' @param name Character vector of place names.
#' @return Character vector of keys.
#' @export
foursquare_brand_key <- function(name) {
  n <- fsq_norm(name)
  key <- rep(NA_character_, length(n))
  for (a in .fsq_brand_aliases) {
    todo <- which(is.na(key))
    key[todo[grepl(a[1], n[todo], perl = TRUE)]] <- a[2]
  }
  todo <- which(is.na(key))
  key[todo] <- vapply(todo, function(i) {
    if (n[i] == "") return(paste0("raw:", tolower(trimws(name[i]))))
    toks <- strsplit(n[i], " ", fixed = TRUE)[[1]]
    toks <- toks[!toks %in% .fsq_brand_stop & !grepl("^[0-9]+$", toks) & nchar(toks) > 1]
    if (!length(toks)) return(n[i])
    paste(utils::head(toks, if (toks[1] %in% .fsq_brand_generic) 3 else 2), collapse = " ")
  }, character(1))
  key
}

## Pairs of points (i < j) within `dist` metres of each other and in the same
## `group`, found through a grid of `dist`-sized cells: each point is compared
## only with the points of its own and the eight neighbouring cells.
.fsq_pairs_within <- function(x, y, dist, group = "") {
  cx <- floor(x / dist)
  cy <- floor(y / dist)
  cell <- paste(group, cx, cy, sep = "|")
  ord <- order(cell)
  runs <- rle(cell[ord])
  start <- cumsum(c(1L, utils::head(runs$lengths, -1L)))
  ii <- jj <- integer(0)
  for (dx in -1:1) for (dy in -1:1) {
    m <- match(paste(group, cx + dx, cy + dy, sep = "|"), runs$values)
    has <- which(!is.na(m))
    len <- runs$lengths[m[has]]
    i <- rep(has, len)
    j <- ord[sequence(len, from = start[m[has]])]
    keep <- i < j
    ii <- c(ii, i[keep])
    jj <- c(jj, j[keep])
  }
  near <- (x[ii] - x[jj])^2 + (y[ii] - y[jj])^2 <= dist^2
  list(i = ii[near], j = jj[near])
}

## Same brand: equal keys, or a one-word key (3+ letters, not generic) equal to
## the other key's first word ("subway" ~ "subway sechelt").
.fsq_keys_match <- function(a, b) {
  raw <- startsWith(a, "raw:") | startsWith(b, "raw:")
  first <- function(k) sub(" .*", "", k)
  one <- function(k) !grepl(" ", k, fixed = TRUE) & nchar(k) >= 3 & !(k %in% .fsq_brand_generic)
  a == b | (!raw & ((one(a) & first(b) == a) | (one(b) & first(a) == b)))
}


#' Duplicate listings within one destination type
#'
#' Two listings are the same site when they are within `same_brand_m` metres
#' and their [foursquare_brand_key()]s match. For pharmacies and dentists, a
#' listing whose name carries no brand or type word (an owner pharmacist, a
#' practitioner) within `owner_m` metres of a branded listing is also folded
#' into it. Each group keeps one listing: a name that reads as the type first,
#' then the most recently refreshed, then the first in input order.
#'
#' @param geometry An `sfc` of points (any CRS; distances use EPSG:3978).
#' @param name,cls,refreshed Place names, their [foursquare_classify_names()]
#'   class, and `date_refreshed` (Date, may be NA).
#' @param type The type name, e.g. `"pharmacy"`.
#' @param same_brand_m,owner_m Distances in metres.
#' @return A data frame with `duplicate` (logical) and `rule`
#'   (`"same_brand"`, `"owner_listing"` or NA).
#' @export
foursquare_dedup <- function(geometry, name, cls, refreshed, type,
                             same_brand_m = 100, owner_m = 60) {
  n <- length(name)
  out <- data.frame(duplicate = rep(FALSE, n), rule = rep(NA_character_, n),
                    stringsAsFactors = FALSE)
  if (n < 2) return(out)
  xy <- sf::st_coordinates(sf::st_transform(geometry, 3978))
  key <- foursquare_brand_key(name)

  ## Same brand: only listings whose keys share a first word can match, so
  ## pair them within that group and within same_brand_m.
  group <- ifelse(startsWith(key, "raw:"), key, sub(" .*", "", key))
  p <- .fsq_pairs_within(xy[, 1], xy[, 2], same_brand_m, group)
  hit <- .fsq_keys_match(key[p$i], key[p$j])
  i <- p$i[hit]
  j <- p$j[hit]

  ## Owner or practitioner listing: an unbranded name next to a branded one.
  if (type %in% c("pharmacy", "dentist")) {
    p <- .fsq_pairs_within(xy[, 1], xy[, 2], owner_m)
    hit <- (cls[p$i] == "yes" & cls[p$j] == "unclear") | (cls[p$i] == "unclear" & cls[p$j] == "yes")
    i <- c(i, p$i[hit])
    j <- c(j, p$j[hit])
  }
  if (!length(i)) return(out)

  parent <- seq_len(n)
  find <- function(x) {
    while (parent[x] != x) {
      parent[x] <<- parent[parent[x]]
      x <- parent[x]
    }
    x
  }
  for (e in seq_along(i)) {
    a <- find(i[e])
    b <- find(j[e])
    if (a != b) parent[a] <- b
  }
  root <- vapply(seq_len(n), find, integer(1))
  rank <- match(cls, c("yes", "unclear", "no"))
  fresh <- -as.numeric(refreshed)
  fresh[is.na(fresh)] <- Inf
  ord <- order(root, rank, fresh, seq_len(n))
  first <- !duplicated(root[ord])
  keeper <- integer(n)
  keeper[ord] <- rep(ord[first], tabulate(match(root[ord], unique(root[ord]))))
  dup <- keeper != seq_len(n)
  out$duplicate <- dup
  out$rule[dup] <- ifelse(.fsq_keys_match(key[dup], key[keeper[dup]]), "same_brand", "owner_listing")
  out
}


#' Build every Foursquare destination type from an extraction
#'
#' @param places A data frame from [foursquare_get_destinations()] (or the
#'   same columns read from a saved extraction): `fsq_place_id`, `name`,
#'   `latitude`, `longitude`, `fsq_category_ids` (list), `date_refreshed`.
#' @inheritParams foursquare_destinations_sql
#' @param refreshed_since A Date. Places Foursquare has not refreshed since
#'   are dropped. `NULL` keeps them all.
#' @param drop_flags Unresolved user reports (`unresolved_flags`) that drop a
#'   place: by default places reported closed, not existing, or to delete.
#' @param rules Output of [foursquare_name_rules()].
#' @return A list: `pois`, one `sf` per `type_id` (WGS 84 points with `name`,
#'   `fsq_place_id`, `fsq_category_ids`, `date_refreshed`), ready for
#'   `cc.pipe::upload_destination_pois()`, which keeps the name and the point;
#'   `summary`, one row per type with
#'   the count at each step; and `dropped`, every place removed by the name
#'   rules or as a duplicate, with the reason.
#' @export
foursquare_prepare_destinations <- function(places, map = foursquare_destination_map(),
                                            refreshed_since = NULL,
                                            drop_flags = c("closed", "doesnt_exist", "delete"),
                                            rules = foursquare_name_rules()) {
  names(places) <- tolower(names(places))
  places$fsq_category_ids <- .fsq_as_id_list(places$fsq_category_ids)
  places$date_refreshed <- as.Date(places$date_refreshed)
  reported <- if ("unresolved_flags" %in% names(places) && length(drop_flags)) {
    vapply(places$unresolved_flags, function(f) any(f %in% drop_flags), logical(1))
  } else rep(FALSE, nrow(places))
  flags <- foursquare_type_flags(places$fsq_category_ids, map)

  pois <- list()
  summary <- list()
  dropped <- list()
  for (t in names(map$types)) {
    p <- places[flags[, t], , drop = FALSE]
    n_category <- nrow(p)
    stale <- if (is.null(refreshed_since)) rep(FALSE, nrow(p)) else
      is.na(p$date_refreshed) | p$date_refreshed < as.Date(refreshed_since)
    off_report <- !stale & reported[flags[, t]]
    p <- p[!stale & !off_report, , drop = FALSE]

    nm <- foursquare_classify_names(p$name, t, rules)
    off <- nm$cls == "no"
    if (any(off)) dropped[[length(dropped) + 1]] <- data.frame(
      type = t, fsq_place_id = p$fsq_place_id[off], name = p$name[off],
      step = "name", reason = nm$reason[off], stringsAsFactors = FALSE)
    p <- p[!off, , drop = FALSE]
    nm <- nm[!off, , drop = FALSE]

    sf_p <- sf::st_as_sf(p, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
    dd <- foursquare_dedup(sf::st_geometry(sf_p), p$name, nm$cls, p$date_refreshed, t)
    if (any(dd$duplicate)) dropped[[length(dropped) + 1]] <- data.frame(
      type = t, fsq_place_id = p$fsq_place_id[dd$duplicate], name = p$name[dd$duplicate],
      step = "duplicate", reason = dd$rule[dd$duplicate], stringsAsFactors = FALSE)
    sf_p <- sf_p[!dd$duplicate, ]

    out <- sf::st_sf(
      name              = sf_p$name,
      fsq_place_id      = sf_p$fsq_place_id,
      fsq_category_ids  = I(sf_p$fsq_category_ids),
      date_refreshed    = sf_p$date_refreshed,
      geometry          = sf::st_geometry(sf_p)
    )
    pois[[map$types[[t]]$type_id]] <- out
    summary[[t]] <- data.frame(
      type_id = map$types[[t]]$type_id, in_categories = n_category,
      not_refreshed = sum(stale), reported_closed = sum(off_report), name_other_business = sum(off),
      duplicates = sum(dd$duplicate), kept = nrow(out))
  }
  list(pois = pois, summary = do.call(rbind, summary),
       dropped = if (length(dropped)) do.call(rbind, dropped) else NULL)
}
