import cats.{Eq, Show}

object CatsImplicits {

//  given signalEq: Eq[Signal] = Eq.fromUniversalEquals
//  given signalShow: Show[Signal] = Show.fromToString
  implicit val signalEq: Eq[Signal] = Eq.fromUniversalEquals
  implicit val signalShow: Show[Signal] = Show.fromToString
}
