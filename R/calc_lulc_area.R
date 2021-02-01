
#' @export
calc_lulc_area <- function(image, features, scale = 30) {

  bands <- image$bandNames()$getInfo()

  features <-
    features %>%
    sf::st_as_sf() %>%
    dplyr::mutate(id = row_number()) %>%
    dplyr::rename(geom = x)

  # Calculate area for each value, band, and municipality
  area_collection <-
    map2_df(
      .x = features$geom,
      .y = features$id,
      function(geometry, id) {

        cat("\n", id, " ")

        # Load one feature into GEE
        ee_geometry <-
          rgee::sf_as_ee(geometry)

        # Get area for each band in the state
        return(
          map_df(
            .x = bands,
            function(b) {

              cat(".")

              # Calculate area by group (discrete values of each band)
              areas <-
                ee$Image$
                pixelArea()$
                addBands(image$select(b))$
                reduceRegion(
                  reducer = ee$Reducer$sum()$group(
                    groupField = 1L,
                    groupName = "value"
                  ),
                  geometry = ee_geometry,
                  scale = scale,
                  maxPixels = 1e13
                )$
                get("groups")$
                getInfo()

              # Return values as a table
              return(
                tibble::tibble(
                  key = map(areas, "value"),
                  area = map(areas, "sum")
                ) %>%
                  tidyr::unnest(cols = c(key, area)) %>%
                  dplyr::mutate(id = id, layer = b)
              )

            }
          )
        )

      }
    )

  return(area_collection)

}
