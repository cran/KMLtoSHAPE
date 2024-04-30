#' Conversion from KML to Shapefile after Extracting Attributes from a Description String based on a List of Attribute Names
#'
#' @param data_kml Path to the KML file
#' @param attribute_names A character vector of attribute names to extract
#' @param outputShape Path to save the output shapefile
#' @return A shapefile with extracted attributes
#' @examples
#' \donttest{
#' library(KMLtoSHAPE)
#' # Example usage:
#' data_kml <- system.file("extdata", "testkml.kml", package = "KMLtoSHAPE")
#' outputShape <- file.path(tempdir(), "shapedata.shp")
#' # Delete existing shapefile if it exists
#' if(file.exists(outputShape)) file.remove(outputShape)
#' # Specify attribute names
#' attribute_names <- c("pH", "EC", "OC", "N", "P", "K")
#' # convert kml to shapefile
#' test_data<-kml_to_shape_gen(data_kml, outputShape,attribute_names)
#' }
#' @references
#' 1. Flores, G. & Gallardo, C. (2021). Creating Shapefile Files in ArcMap from KML File Generated in My Maps. In Advances in Emerging Trends and Technologies: Proceedings of ICAETT 2020 (pp. 193-204). Springer International Publishing..<DOI:10.1007/978-3-030-63665-4_15>
#' 2. Kumar et al. (2023). SpatGRID:Spatial Grid Generation from Longitude and Latitude List. R package version 0.1.0.
#' @export
#' @import stringr
#' @import sf
#' @import raster
#'
kml_to_shape_gen <- function(data_kml, outputShape,attribute_names) {

  # Function to extract attributes
  extract_attributes <- function(description, attribute_names) {
    attributes <- numeric(length(attribute_names))
    for (i in seq_along(attribute_names)) {
      pattern <- paste0("<td>", attribute_names[i], "</td>\\s*<td>([0-9.]+)</td>")
      match <- str_match(description, pattern)
      if (!is.na(match[2])) {
        attributes[i] <- as.numeric(match[2])
      }
    }
    names(attributes) <- attribute_names
    return(attributes)
  }
  # Read the KML file
  x <- read_sf(data_kml)
  # Extract attributes from Description column
  descriptions <- x$Description
  # Apply the function to each description
  attributes_list <- lapply(descriptions, extract_attributes, attribute_names)
  # Convert the list to a data frame
  attributes_df <- do.call(rbind, attributes_list)
  # Convert attributes to numeric
  attributes_df <- apply(attributes_df, 2, as.numeric)
  # Convert the geometry to 2D
  xx <- st_zm(x)
  # Combine the extracted attributes with the original sf object
  x_new <- cbind(xx, attributes_df)
  # Save the modified sf object as a shapefile
  shape<-st_write(x_new, outputShape)
  return(shape)
}
