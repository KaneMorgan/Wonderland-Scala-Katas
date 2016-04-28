import scala.util.Random

case class Card(suit: String, rank: String) {
  def equals(other:Card) = other.suit == suit && other.rank == rank
}

case class Deck(cards: List[Card]) {
  def isEmpty = this.cards.isEmpty

//  def addCard(card: Card) =>(cards.reverse)
}

case class Player(name: String, deck: Deck)

object CardGameWar {
  // Feel free to use these cards or use your own data structure
  val suits = List("Spade", "Club", "Diamond", "Heart")
  val ranks = List("2", "3", "4", "5", "6", "7", "8", "9", "10", "Jack", "Queen", "King", "Ace")

  // Creates two shuffled decks of cards
  def createDecks: (Deck, Deck) = {
    val allCards =
      new Random shuffle (for {
        suit <- suits
        rank <- ranks
      } yield Card(suit, rank.toString))

    val List(d1, d2) = allCards.grouped(allCards.length / 2).toList
    (Deck(d1), Deck(d2))
  }

  def playRound(player1: Card, player2: Card): Card = {
    if(ranks.indexOf(player1.rank) == ranks.indexOf(player2.rank))
      { return checkSuits(player1, player2)}
    if(ranks.indexOf(player1.rank) > ranks.indexOf(player2.rank)) player1 else player2
  }

  def checkSuits(player1:Card, player2:Card):Card =
    if(suits.indexOf(player1.suit) > suits.indexOf(player2.suit)) player1 else player2

  def playGame(player1: Player, player2: Player): String = {
    (player1, player2) match{
      case _ if player1.deck.isEmpty => player2.name
      case _ if player2.deck.isEmpty == true => player1.name
      case (player1, player2) => {
        val card1 = player1.deck.cards.head
        val card2 = player2.deck.cards.head
        val winner = playRound(card1,card2)
        if(winner == card1)
          playGame(Player(player1.name,Deck(player1.deck.cards.tail :+ card1:+ card2)),Player(player2.name,Deck(player2.deck.cards.tail)))
        else
          playGame(Player(player1.name,Deck(player1.deck.cards.tail)),Player(player2.name,Deck(player2.deck.cards.tail:+ card1:+card2)))
      }
    }

  }


}

