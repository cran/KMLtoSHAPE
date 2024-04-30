#' Conversion from KML to Shapefile While Preserving Attribute Values
#'
#' @param data_kml Path to the KML file
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
#' # convert kml to shapefile
#' test_data<-kml_to_shape(data_kml, outputShape)
#' }
#' @references
#' 1. Flores, G. & Gallardo, C. (2021). Creating Shapefile Files in ArcMap from KML File Generated in My Maps. In Advances in Emerging Trends and Technologies: Proceedings of ICAETT 2020 (pp. 193-204). Springer International Publishing..<DOI:10.1007/978-3-030-63665-4_15>
#' 2. Kumar et al. (2023). SpatGRID:Spatial Grid Generation from Longitude and Latitude List. R package version 0.1.0.
#' @export
#' @import stringr
#' @import sf
#' @import raster
#'
kml_to_shape <- function(data_kml, outputShape) {
  # Read the KML file
  x <- read_sf(data_kml)
  # Extract attributes from Description column
  descriptions <- x$Description
  # Function to extract attributes
  extract_attributes <- function(description) {
    pH <- str_match(description, "<td>pH</td>\\s*<td>([0-9.]+)</td>")
    EC <- str_match(description, "<td>EC</td>\\s*<td>([0-9.]+)</td>")
    OC <- str_match(description, "<td>OC</td>\\s*<td>([0-9.]+)</td>")
    N <- str_match(description, "<td>N</td>\\s*<td>([0-9.]+)</td>")
    P <- str_match(description, "<td>P</td>\\s*<td>([0-9.]+)</td>")
    K <- str_match(description, "<td>K</td>\\s*<td>([0-9.]+)</td>")

    attributes <- c(pH[2], EC[2], OC[2], N[2], P[2], K[2])
    names(attributes) <- c("pH", "EC", "OC", "N", "P", "K")
    return(attributes)
  }
  # Apply the function to each description
  attributes_list <- lapply(descriptions, extract_attributes)
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
