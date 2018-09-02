package observatory

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.collection.concurrent.TrieMap

@RunWith(classOf[JUnitRunner])
trait InteractionTest extends FunSuite with Checkers {
  test("tile location") {
    assert(Interaction.tileLocation(Tile(0, 0, 0)) === Location(85.05112877980659, -180))
    assert(Interaction.tileLocation(Tile(10, 10, 10)) === Location(84.7383871209534, -176.484375))
  }


}
