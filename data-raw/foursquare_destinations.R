## Foursquare destination category map ########################################
##
## Builds inst/extdata/foursquare_destinations.json: for each Curbcut
## destination type, the exact Foursquare category IDs that put a place in it
## (`include`), the categories that disqualify a place that also carries them
## (`exclude_if_also`), and the categories that keep a place regardless
## (`keep_if`). The IDs are resolved from Foursquare's published taxonomy so the
## map lists every category by its ID and full label: no substring matching.
##
## Taxonomy: the "Places OS, Pro & Premium Categories" table on
## https://docs.foursquare.com/data-products/docs/categories (an Observable
## notebook; its CSV attachment is the URL below). Re-run this script when
## Foursquare publishes a new taxonomy, then check the result against the
## Snowflake share with foursquare_check_categories().
##
## usage: Rscript data-raw/foursquare_destinations.R [taxonomy.csv or URL]
################################################################################

taxonomy_src <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(taxonomy_src)) {
  taxonomy_src <- paste0(
    "https://static.observableusercontent.com/files/",
    "8bf043336b438a770178f4e9fbacfef6f52fc49c106a199295b42389b920bff1",
    "93b885db1082f575be2b5853cd75119800b6ce6751961091ce659a4617e56585"
  )
}
taxonomy_last_modified <- "2026-09-15"   # HTTP Last-Modified of the CSV above

## The CSV is served gzip-encoded; curl decodes it, base url() does not.
con <- if (grepl("^https?://", taxonomy_src)) curl::curl(taxonomy_src) else file(taxonomy_src)
tax <- utils::read.csv(con, stringsAsFactors = FALSE, encoding = "UTF-8")
stopifnot(identical(names(tax), c("category_id", "category_name", "category_label")))

FBR <- "Retail > Food and Beverage Retail"
HM  <- "Health and Medicine"
DD  <- "Dining and Drinking"

## node(): that category only. tree(): that category and every category below it.
node <- function(...) list(labels = c(...), subtree = FALSE)
tree <- function(...) list(labels = c(...), subtree = TRUE)

spec <- list(
  grocery_store = list(
    include = list(
      node(paste(FBR, c("Supermarket", "Fruit and Vegetable Store",
                        "Imported Food Store", "Kosher Store"), sep = " > ")),
      tree(paste(FBR, "Grocery Store", sep = " > "))
    ),
    ## A grocery listing that is also a convenience store or a gas station is a
    ## depanneur or a gas bar, unless it is also a supermarket.
    exclude_if_also = list(node("Retail > Convenience Store",
                                "Travel and Transportation > Fuel Station")),
    keep_if = list(node(paste(FBR, "Supermarket", sep = " > ")))
  ),
  pharmacy = list(
    include = list(node("Retail > Pharmacy", "Retail > Drugstore"))
  ),
  clinic = list(
    include = list(
      node(paste(HM, c("Healthcare Clinic", "Medical Center", "Urgent Care Center",
                       "Maternity Clinic", "Women's Health Clinic",
                       "Mental Health Service > Mental Health Clinic",
                       "Sports Medicine Clinic"), sep = " > ")),
      tree(paste(HM, "Physician", sep = " > "))
    )
  ),
  allied_health = list(
    include = list(
      node(paste(HM, c("Physical Therapy Clinic", "Chiropractor", "Optometrist",
                       "Mental Health Service", "Mental Health Service > Psychologist",
                       "Podiatrist", "Acupuncture Clinic", "Nutritionist",
                       "Other Healthcare Professional"), sep = " > "))
    )
  ),
  dentist = list(
    include = list(node(paste(HM, "Dentist", sep = " > ")))
  ),
  gas_station = list(
    include = list(node("Travel and Transportation > Fuel Station"))
  ),
  bank = list(
    include = list(node(
      "Business and Professional Services > Financial Service > Banking and Finance > Bank",
      "Business and Professional Services > Financial Service > Banking and Finance > Credit Union"
    )),
    exclude_if_also = list(node(
      "Business and Professional Services > Financial Service > Check Cashing Service",
      "Business and Professional Services > Financial Service > Currency Exchange"
    ))
  ),
  cinema = list(
    include = list(tree("Arts and Entertainment > Movie Theater"))
  ),
  cafe = list(
    include = list(tree(paste(DD, "Cafe, Coffee, and Tea House", sep = " > "))),
    exclude_if_also = list(node("Retail > Convenience Store"))
  ),
  bakery = list(
    include = list(node(paste(DD, "Bakery", sep = " > "))),
    ## Supermarket bakery counters belong to the supermarket (grocery_store).
    exclude_if_also = list(node(paste(FBR, c("Supermarket", "Grocery Store"), sep = " > "),
                                "Retail > Big Box Store"))
  ),
  bar = list(
    include = list(tree(paste(DD, "Bar", sep = " > ")))
  ),
  restaurant = list(
    include = list(tree(paste(DD, "Restaurant", sep = " > ")),
                   node(paste(DD, "Food Truck", sep = " > ")))
  ),
  gym = list(
    include = list(tree("Sports and Recreation > Gym and Studio",
                        "Sports and Recreation > Gymnastics"),
                   node("Sports and Recreation > Martial Arts Dojo"))
  )
)

resolve <- function(parts) {
  if (is.null(parts)) return(list())
  out <- lapply(parts, function(p) {
    missing <- setdiff(p$labels, tax$category_label)
    if (length(missing)) stop("Not in the taxonomy: ", paste(missing, collapse = "; "))
    hit <- tax$category_label %in% p$labels
    if (p$subtree) {
      for (l in p$labels) hit <- hit | startsWith(tax$category_label, paste0(l, " > "))
    }
    tax[hit, c("category_id", "category_label")]
  })
  rows <- unique(do.call(rbind, out))
  rows <- rows[order(rows$category_label), ]
  lapply(seq_len(nrow(rows)), function(i)
    list(id = rows$category_id[i], label = rows$category_label[i]))
}

types <- lapply(names(spec), function(t) {
  s <- spec[[t]]
  list(
    type_id         = paste0(t, "_foursquare"),
    include         = resolve(s$include),
    exclude_if_also = resolve(s$exclude_if_also),
    keep_if         = resolve(s$keep_if)
  )
})
names(types) <- names(spec)

out <- list(
  taxonomy = list(
    name          = "Foursquare Places OS, Pro & Premium Categories",
    docs          = "https://docs.foursquare.com/data-products/docs/categories",
    file          = taxonomy_src,
    last_modified = taxonomy_last_modified,
    retrieved     = format(Sys.Date()),
    n_categories  = nrow(tax)
  ),
  types = types
)

dir.create("inst/extdata", recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(out, "inst/extdata/foursquare_destinations.json",
                     auto_unbox = TRUE, pretty = TRUE)
for (t in names(types)) {
  message(sprintf("%-14s %4d categories (%d exclude, %d keep)", t,
                  length(types[[t]]$include), length(types[[t]]$exclude_if_also),
                  length(types[[t]]$keep_if)))
}
