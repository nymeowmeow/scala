package observatory

import java.time.LocalDate
import org.apache.spark.sql.SparkSession
import java.nio.file.Paths

/**
  * 1st milestone: data extraction
  */
object Extraction {
  val spark : SparkSession = SparkSession.builder().appName("Extraction").config("spark.master", "local").getOrCreate()

  def convertToDegree(fahrenheit : Temperature) : Temperature =
    (fahrenheit - 32)*5/9

  def getPath(resource : String) : String =
    Paths.get(getClass.getResource(resource).toURI).toString

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] = {
    val station = spark.sparkContext.textFile(getPath(stationsFile)).map(_.split(',')).filter(_.length == 4)
                             .map(data => ((data(0), data(1)), Location(data(2).toDouble, data(3).toDouble)))
    val temperature = spark.sparkContext.textFile(getPath(temperaturesFile)).map(_.split(',')).filter(_.length == 5)
                             .map(data => ((data(0), data(1)), (LocalDate.of(year, data(2).toInt, data(3).toInt), convertToDegree(data(4).toDouble))
                             ))
    station.join(temperature).mapValues(v => (v._2._1, v._1, v._2._2)).values.collect().toSeq
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] = {
    records.par.groupBy(_._2).mapValues(l => l.foldLeft(0.0)((acc, t) => acc + t._3)/l.size).seq
  }

}
