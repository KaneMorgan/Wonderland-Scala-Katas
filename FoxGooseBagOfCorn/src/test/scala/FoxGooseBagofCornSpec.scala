import FoxGooseBagofCorn._
import org.scalatest._

class FoxGooseBagofCornSpec extends WordSpec {

  import FoxGooseCornYou._

  "river crossing plan" when {
    val crossingPlan = riverCrossingPlan

    "you begin with the fox, goose and corn on one side of the river" in {
      assert(crossingPlan.head == Seq(you + fox + goose + corn, empty, empty))
    }

    "you end with the fox, goose and corn on one side of the river" in {
      assert(crossingPlan.last == Seq(empty, empty, you + fox + goose + corn))
    }

    "things are safe" when {
      val leftBank = crossingPlan map (_.head)
      val rightBank = crossingPlan map (_.last)

      "the fox and the goose should never be left alone together" in {
        assert(!((leftBank ++ rightBank) contains (fox + goose)))
      }

      "the goose and the corn should never be left alone together" in {
        assert(!((leftBank ++ rightBank) contains (goose + corn)))
      }
    }

    "The boat can carry only you plus one other" in {
      val boatPositions = crossingPlan map (_ (2))
      assert(!(boatPositions exists (_.size > 2)))
    }

    "moves are valid" in {
      val leftMoves = crossingPlan map (_.head)
      val middleMoves = crossingPlan map (_ (2))
      val rightMoves = crossingPlan map (_.last)

      def validateMove(step1: ValueSet, step2: ValueSet) {
        val diff1 = step1 diff step2
        val diff2 = step2 diff step1
        val diffs = diff1 ++ diff2
        val diffNum = diffs.size
        assert(diffNum < 3, "only you and another thing can move")
        if (diffNum > 0)
          assert(diffs contains you, "only you and another thing can move")
      }
      def validateMoves(moves: Seq[ValueSet]) {
        moves sliding 2 foreach {
          case Seq(step1, step2) => validateMove(step1, step2)
          case _ =>
        }
      }

      validateMoves(leftMoves)
      validateMoves(middleMoves)
      validateMoves(rightMoves)
    }
  }
}