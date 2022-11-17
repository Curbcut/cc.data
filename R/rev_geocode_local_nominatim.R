#' Create a docker local Nominatim instance for faster reverse geocoding
#'
#' @return Opens a terminal from which the docker image is created.
#' @export
rev_geocode_local_nominatim <- function() {

  if (Sys.info()["sysname"] != "Windows")
    stop("As of now, this function is only adapted for Windows.")

  cancel <- readline(prompt =
                       paste0("Pursuing will open a terminal to create the ",
                              "docker image. Write 'ok' to proceed: "))
  if (cancel != "ok") return(cat("Aborted succesfully."))

  # Create the docker image
  local_nominatim <-
    paste0("docker pull mediagis/nominatim\n",
           "docker run -it -e PBF_URL=",
           "https://download.geofabrik.de/north-america/canada-latest.osm.pbf",
           " -e REPLICATION_URL=",
           gsub("-latest.osm.pbf", "-updates/",
                "https://download.geofabrik.de/north-america/canada-latest.osm.pbf"),
           " -p 8080:8080 --name nominatim-canada",
           " mediagis/nominatim:4.1",
           "\n exit")
  tmp <- tempfile(fileext = ".ps1")
  writeLines(local_nominatim, tmp)

  shell(paste0("start cmd.exe @cmd /k powershell -ExecutionPolicy Bypass -File ",
               gsub("/", "\\\\", paste0(tmp))))

}
