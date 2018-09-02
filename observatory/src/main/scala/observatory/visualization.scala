package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import scala.math.{abs, asin, sin, cos, pow, round, sqrt, toRadians}

/**
  * 2nd milestone: basic visualization
  */
object Visualization {
  val EarthRadius = 6371
  val width = 360
  val height = 180
  val pvalue = 5

  def isAntipodes(first: Location, second: Location) : Boolean = {
    (first.lat == -second.lat) && (abs(first.lon - second.lon) == 180)
  }

  def distance(first : Location, second : Location) : Double = {
    if (first == second) 0.0
    else if (isAntipodes(first, second)) EarthRadius * math.Pi
    else {
      val deltaLon = toRadians(abs(first.lon - second.lon))
      val firstLat = toRadians(first.lat)
      val secondLat = toRadians(second.lat)
      val deltaLat = abs(firstLat - secondLat)
      //from wiki
      val deltaSigma = 2*asin(sqrt(pow(sin(deltaLat/2), 2) + cos(firstLat) * cos(secondLat) * pow(sin(deltaLon/2), 2)))
      EarthRadius * deltaSigma
    }
  }

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {
    val dist = temperatures.map(t => (distance(location, t._1), t._2))
    val smallest = dist.minBy(_._1)
    if (smallest._1 < 1) {
      smallest._2
    } else {
      val w = dist.map(entry => (1/pow(entry._1, pvalue), entry._2))
      val total = w.map(_._1).sum
      w.map(entry => entry._1 * entry._2).sum/total
    }
  }

  def linearInterpolation(first : Option[(Double, Color)], second : Option[(Double, Color)], value : Double) : Color =
    (first, second) match {
      case (Some((firstvalue, firstcolor)), Some((secondvalue, secondcolor))) => {
        val factor = (value - firstvalue)/(secondvalue - firstvalue)
        Color(round(firstcolor.red + factor*(secondcolor.red - firstcolor.red)).toInt,
          round(firstcolor.green + factor * (secondcolor.green - firstcolor.green)).toInt,
          round(firstcolor.blue + factor * (secondcolor.blue - firstcolor.blue)).toInt)
      }
      case (Some(first), None) => first._2
      case (None, Some(second)) => second._2
      case _ => Color(0, 0, 0)
    }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {
    points.find(_._1 == value) match {
      case Some((_, color)) => color
      case _ => {
        val (lower, higher) = points.toList.sortBy(_._1).partition(_._1 < value)
        linearInterpolation(lower.reverse.headOption, higher.headOption, value)
      }
    }
  }

  def normalize(loc : (Int, Int)) : Location = {
    val lon = (loc._2 - width/2) * 360/width
    val lat = -(loc._1 - height/2)* 180/height

    Location(lat, lon)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {
    val coords = for {
      i <- 0 until height
      j <- 0 until width
    } yield (i, j)

    val pixels = coords.par.map(normalize).map(predictTemperature(temperatures, _))
                         .map(interpolateColor(colors, _)).map(c => Pixel(c.red, c.green, c.blue, 255)).toArray
    Image(width, height, pixels)
  }

}

