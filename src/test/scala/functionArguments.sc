
abstract class Block
case class Loop(b: Block) extends Block
case class Check() extends Block
case class If(b: Block) extends Block
case class Right() extends Block
case class Left() extends Block


trait Simulation {

  type Action = () => Unit
  case class Event(time: Int, action: Action)
  private type Agenda = List[Event]
  private var agenda: Agenda = List.empty

  private var curTime = 0

  def currentTime: Int = curTime
  def afterDelay(delay: Int)(block: => Unit): Unit = {
    val item = Event(currentTime + delay, () => block)
    agenda = insert(agenda, item)
  }

  private def insert(agenda: Agenda, item: Event) : List[Event] = agenda match {
    case first :: rest if first.time <= item.time =>
        first :: insert(rest , item)

    case _ => item :: agenda
  }

  private def loop() : Unit = agenda match {
    case first :: rest =>
      agenda = rest
      curTime = first.time
      first.action()
      loop()
    case Nil =>
  }

  def run() : Unit = {
    afterDelay(0) {
      println("*** simulation started, time = " + currentTime + " ***")
    }

    loop()
  }


}

abstract class MoonLanding extends Simulation {
  type Land = Set[Int] // every integer represents the position of a parachute

  class Robot(var position: Int) {
    private var actions: List[Action] = List.empty[Action]

    def right : Unit = position = position + 1
    def left : Unit = position = position - 1
    def check(l: Land) = l contains position

    def execActions : Unit = for (a <- actions) a()

    def addAction(a: Action): Unit = {
      actions = actions :+ a
      a()
    }
  }

}


