

remap_bands <- function() {

  img_collection <-
    ee$ImageCollection$fromImages(
      map(bands, function(band_name) {
        values <- c(3, -1) # The "-1" avoids an error with ee.List())
        return(
          mapbiomas$
            select(band_name)$
            remap(
              from = values,
              to = rep(1, length(values)),
              defaultValue = 0
            )
        )
      })
    )

}
