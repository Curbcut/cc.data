## Scrape NAICS Classification Dictionary (EN/FR) from Statistics Canada
## This function scrapes the hierarchical NAICS classification structure (2-digit to 6-digit levels) 
## from the official Statistics Canada website, in English, French, or both.

naics_scrape_dictionary <- function(langue = NULL) {

  scrape_one_language <- function(main_url) {

    scrape_and_find_next <- function(full_url, description, level) {
      response <- httr::GET(full_url)
      if (httr::http_error(response)) return(list(data = NULL, new_links = NULL))

      html_page <- rvest::read_html(httr::content(response, as = "text", encoding = "UTF-8"))
      table_node <- rvest::html_node(html_page, "table")
      if (inherits(table_node, "xml_missing")) return(list(data = NULL, new_links = NULL))

      extracted_data <- table_node |>
        rvest::html_table(fill = TRUE) |>
        dplyr::rename(code = 1, title = 2) |>
        dplyr::mutate(
          code = stringr::str_extract(code, "^[0-9]{3,6}"),
          title = stringr::str_trim(title),
          description = description,
          level = level
        ) |>
        dplyr::filter(!is.na(code) & code != "" & title != "") |>
        dplyr::select(code, title, description, level)

      new_links <- table_node |>
        rvest::html_nodes("a") |>
        purrr::map_df(~tibble::tibble(
          link_text = rvest::html_text(.x, trim = TRUE),
          href = rvest::html_attr(.x, "href")
        )) |>
        dplyr::mutate(
          code = stringr::str_extract(link_text, "^[0-9]{3,6}"),
          full_url = dplyr::if_else(
            stringr::str_detect(href, "^http"), href,
            paste0("https://www23.statcan.gc.ca", href)
          )
        ) |>
        dplyr::filter(!is.na(code)) |>
        dplyr::distinct(code, .keep_all = TRUE) |>
        dplyr::select(full_url, description = code) |>
        dplyr::mutate(level = level + 1)

      return(list(data = extracted_data, new_links = new_links))
    }

    html_content <- httr::GET(main_url) |>
      httr::content(as = "text", encoding = "UTF-8") |>
      rvest::read_html()

    level2_full_data <- html_content |>
      rvest::html_node("table") |>
      rvest::html_nodes("a") |>
      purrr::map_df(~tibble::tibble(
        link_text = rvest::html_text(.x, trim = TRUE),
        href = rvest::html_attr(.x, "href")
      )) |>
      dplyr::mutate(
        code = stringr::str_extract(link_text, "^[0-9]{2}(?:-[0-9]{2})?"),
        title = stringr::str_remove(link_text, "^[0-9]{2}(?:-[0-9]{2})?") |> stringr::str_trim(),
        full_url = dplyr::if_else(
          stringr::str_detect(href, "^http"), href,
          paste0("https://www23.statcan.gc.ca", href)
        )
      ) |>
      dplyr::filter(!is.na(code)) |>
      dplyr::distinct(code, .keep_all = TRUE)

    tasks <- level2_full_data |>
      dplyr::select(full_url, description = code) |>
      dplyr::mutate(level = 3)

    naics_registry <- list()
    naics_registry[["digt2"]] <- level2_full_data |>
      dplyr::transmute(
        code,
        description = title,
        level = 2
      )

    while (nrow(tasks) > 0) {
      current_level_num <- tasks$level[1]
      message(sprintf("--- scraping for level %d (%d pages) ---", current_level_num, nrow(tasks)))

      level_results_list <- furrr::future_pmap(
        tasks,
        scrape_and_find_next
      )

      all_data_from_level <- purrr::map_dfr(level_results_list, "data")
      tasks <- purrr::map_dfr(level_results_list, "new_links")

      if (nrow(all_data_from_level) > 0) {
        list_name <- paste0("digt", current_level_num)
        naics_registry[[list_name]] <- all_data_from_level |>
          dplyr::arrange(code)
      }
    }

    return(naics_registry)
  }

  urls <- list(
    en = "https://www23.statcan.gc.ca/imdb/p3VD.pl?Function=getVD&TVD=1181553",
    fr = "https://www23.statcan.gc.ca/imdb/p3VD_f.pl?Function=getVD&TVD=1181553"
  )

  if (!is.null(langue)) {
    if (!langue %in% c("en", "fr")) stop("langue must be 'en', 'fr', or NULL")
    result <- scrape_one_language(urls[[langue]])
    return(result)
  } else {
    message("Scraping both English and French...")
    return(list(
      en = scrape_one_language(urls$en),
      fr = scrape_one_language(urls$fr)
    ))
  }
}

naics_dictionary <- naics_scrape_dictionary()  # par défaut langue = NULL → scrape en +fr

usethis::use_data(naics_dictionary, overwrite = TRUE)
