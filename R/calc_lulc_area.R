
library(rgee)
library(sf)
library(tidyverse)


calc_area <- function(image, bands_list) {

  bands <- bands_list

  img <- image

  ## Calculate area for each value, band, and municipality ----
  area_collection <-
    map_df(
      .x = municip_intersection$code_muni,
      function(muni) {

        cat("\n", muni, "\n")

        ## Load one state into GEE ----
        ee_state <-
          sf_as_ee(
            municip_intersection %>% filter(code_muni == muni)
          )

        ## Get area for each band in the state -----
        return(
          map_df(
            .x = bands,
            function(b) {

              cat(b, " ", sep = " ")

              ## Calculate area by group (discrete values of each band) ----
              areas <-
                ee$Image$
                pixelArea()$
                addBands(img$select(b))$
                reduceRegion(
                  reducer = ee$Reducer$sum()$group(
                    groupField = 1L,
                    groupName = "value"
                  ),
                  geometry = ee_state$first()$geometry(),
                  scale = 30,
                  maxPixels = 1e13
                )$
                get("groups")$
                getInfo()

              ## Return values as a table ----
              return(
                tibble(key = map(areas, "value"), area = map(areas, "sum")) %>%
                  unnest(cols = c(key, area)) %>%
                  mutate(muni_code = muni, layer = b)
              )

            }
          )
        )

      }
    )

  return(area_collection)

}
