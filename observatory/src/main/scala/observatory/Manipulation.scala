package observatory

import Visualization._
import scala.collection.mutable.HashMap

/**
  * 4th milestone: value-added information
  */
object Manipulation {
  //val gridCache = new HashMap[(Int, Int), Temperature]()
  val averageCache = new HashMap[(Int, Int), Temperature]()
  val deviationCache = new HashMap[(Int, Int), Temperature]()

  /**
    * @param temperatures Known temperatures
    * @return A function that, given a latitude in [-89, 90] and a longitude in [-180, 179],
    *         returns the predicted temperature at this location
    */
  def makeGrid(temperatures: Iterable[(Location, Temperature)]): GridLocation => Temperature = gloc => {
    //gridCache.getOrElseUpdate((gloc.lat, gloc.lon), predictTemperature(temperatures, Location(gloc.lat, gloc.lon)))
    predictTemperature(temperatures, Location(gloc.lat, gloc.lon))
  }

  /**
    * @param temperaturess Sequence of known temperatures over the years (each element of the collection
    *                      is a collection of pairs of location and temperature)
    * @return A function that, given a latitude and a longitude, returns the average temperature at this location
    */
  def average(temperaturess: Iterable[Iterable[(Location, Temperature)]]): GridLocation => Temperature = gloc => {
    //averageCache.getOrElseUpdate((gloc.lat, gloc.lon), {
      //val t = temperaturess.map(tlist => makeGrid(tlist)(gloc)).toList
      //t.sum / t.length
    //)
    val t = temperaturess.map(tlist => makeGrid(tlist)(gloc)).toList
    t.sum / t.length
  }

  /**
    * @param temperatures Known temperatures
    * @param normals A grid containing the “normal” temperatures
    * @return A grid containing the deviations compared to the normal temperatures
    */
  def deviation(temperatures: Iterable[(Location, Temperature)], normals: GridLocation => Temperature): GridLocation => Temperature = gloc => {
    //deviationCache.getOrElseUpdate((gloc.lat, gloc.lon), {makeGrid(temperatures)(gloc) - normals(gloc)})
    makeGrid(temperatures)(gloc) - normals(gloc)
  }


}

