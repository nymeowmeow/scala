package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import scala.math.{ceil, pow, floor}

/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 {
  val width = 256
  val height = 256

  /**
    * @param point (x, y) coordinates of a point in the grid cell
    * @param d00 Top-left value
    * @param d01 Bottom-left value
    * @param d10 Top-right value
    * @param d11 Bottom-right value
    * @return A guess of the value at (x, y) based on the four known values, using bilinear interpolation
    *         See https://en.wikipedia.org/wiki/Bilinear_interpolation#Unit_Square
    */
  def bilinearInterpolation(
    point: CellPoint,
    d00: Temperature,
    d01: Temperature,
    d10: Temperature,
    d11: Temperature
  ): Temperature = {
    d00 * (1 - point.x) * (1 - point.y) + d10 * point.x * (1 - point.y) + d01 * (1 - point.x) * point.y + d11 * point.x * point.y
  }

  /**
    * @param grid Grid to visualize
    * @param colors Color scale to use
    * @param tile Tile coordinates to visualize
    * @return The image of the tile at (x, y, zoom) showing the grid using the given color scale
    */
  def visualizeGrid(
    grid: GridLocation => Temperature,
    colors: Iterable[(Temperature, Color)],
    tile: Tile
  ): Image = {
    val pixels : Array[Pixel] = new Array(width * height)
    val side = pow(2, tile.zoom)

    for (col <- 0 until width) {
      for (row <- 0 until height) {
        val pixelX = col + tile.x * width
        val pixelY = row + tile.y * height

        val loc = Interaction.tileLocation(Tile(pixelX, pixelY, tile.zoom + 8))
        val lat = loc.lat.toInt
        val lon = loc.lon.toInt
        val value = bilinearInterpolation(CellPoint(loc.lon - lon, loc.lat - lat),
          grid(GridLocation(lat, lon)),
          grid(GridLocation(lat+1, lon)),
          grid(GridLocation(lat, lon + 1)),
          grid(GridLocation(lat + 1, lon + 1)))
        val c = Visualization.interpolateColor(colors, value)
        pixels((width * row) + col) = Pixel(c.red, c.green, c.blue, 127)
      }
    }
    Image(width, height, pixels)
  }

}
