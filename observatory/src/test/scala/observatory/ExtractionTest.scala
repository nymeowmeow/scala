package observatory

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import java.time.LocalDate

@RunWith(classOf[JUnitRunner])
class ExtractionTest extends FunSuite {
    test("locate temperatures") {
      Extraction.locateTemperatures(2003, "/stations.csv", "/2003.csv")
    }

    test("location yearly average records") {
      val loc1 = Location (1, 1)
      val loc2 = Location (2, 2)
      val loc3 = Location (3, 3)
      val result = Extraction.locationYearlyAverageRecords(Seq(
        (LocalDate.of(2018, 1, 1), loc1, 10),
        (LocalDate.of(2018, 6, 1), loc1, 20),
        (LocalDate.of(2018, 12, 1), loc1, 30),
        (LocalDate.of(2018, 1, 1), loc2, 5),
        (LocalDate.of(2018, 6, 1), loc2, 10),
        (LocalDate.of(2018, 9, 1), loc2, 15),
        (LocalDate.of(2018, 6, 1), loc3, 25)
      ))
      assert(result.toList === Seq((loc1, 20), (loc2, 10), (loc3, 25)))
    }
}