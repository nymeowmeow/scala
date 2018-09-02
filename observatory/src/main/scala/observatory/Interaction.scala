package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import Math.{atan, PI, pow, sinh}
import Visualization.{interpolateColor, predictTemperature, normalize}

/**
  * 3rd milestone: interactive visualization
  */
object Interaction {
  val power = 8
  val width = 256
  val height = 256
  val alpha = 127

  /**
    * @param tile Tile coordinates
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(tile: Tile): Location = {
    val len = pow(2, tile.zoom)
    val lon = (tile.x.toDouble * 360.0)/len - 180.0
    val lat = ((atan(sinh(PI * (1.0 - 2.0 *tile.y.toDouble/len)))*180.0/PI + 90) % 180) - 90
    Location(lat, lon)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param tile Tile coordinates
    * @return A 256×256 image showing the contents of the given tile
    */
  def tile(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)], tile: Tile): Image = {

    val pixels: Array[Pixel] = new Array(width * height)
    for (row <- 0 until height) {
      for (col <- 0 until width) {
        val pixelX = col + tile.x * width
        val pixelY = row + tile.y * height

        val loc = tileLocation(Tile(pixelX, pixelY, tile.zoom + 8))
        val temp = predictTemperature(temperatures, loc)
        val c = interpolateColor(colors, temp)
        pixels((width * row) + col) = Pixel(c.red, c.green, c.blue, alpha)
      }
    }

    Image(width, height, pixels)
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    * @param yearlyData Sequence of (year, data), where `data` is some data associated with
    *                   `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
    yearlyData: Iterable[(Year, Data)],
    generateImage: (Year, Tile, Data) => Unit
  ): Unit = {
    for (year <- yearlyData) {
      for (zoomLevel <- 0 to 3) {
        val tilesNo: Int = Math.pow(2, zoomLevel).toInt
        for (x <- 0 until tilesNo) {
          for (y <- 0 until tilesNo) {
            generateImage(year._1, Tile(x, y, zoomLevel), year._2)
          }
        }
      }
    }
  }

}
