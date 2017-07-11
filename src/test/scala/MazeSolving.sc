// A Cell represents the status of a grid location in the maze
object Cell extends Enumeration {

  type Cell = Value

  val Empty = Value(".")
  val Blocked = Value("X")
  val Start = Value("S")
  val Goal = Value("G")
  val Path = Value("P")
}

import Cell._

type Maze = Array[Array[Cell]]

val seed = 8273739
val rnd = new scala.util.Random(seed)

/// this is how to produce random values from an Enumeration

Cell(rnd.nextInt(Cell.maxId))
Cell(rnd.nextInt(Cell.maxId))
Cell(rnd.nextInt(Cell.maxId))
Cell(rnd.nextInt(Cell.maxId))

rnd.nextDouble()
rnd.nextDouble


def randomCell = Cell(rnd.nextInt(Cell.maxId))

def generateMaze(rows: Int, columns: Int, sparseness: Double) : Maze = {
  import Cell._

  var maze = Array.ofDim[Cell](rows, columns)

  for {
    row <- 0 to rows - 1
    col <- 0 to columns - 1
  } maze(row)(col) = if (rnd.nextDouble() < sparseness) Blocked else Empty

  maze
}

def printMaze(maze: Maze) = {
  import Cell._
    maze.foreach( (a: Array[Cell]) => {
      a foreach ( print _)
      println()
    })
}


val maze = generateMaze(20, 20, 0.25)
printMaze(maze)

case class MazeLocation(row: Int, col: Int) {
  def left = MazeLocation(row, col - 1)
  def right = MazeLocation(row, col + 1)
  def up = MazeLocation(row - 1, col)
  def down = MazeLocation(row + 1, col)

  def isDefined(m: Maze) = row >= 0 && row < m.length && col >= 0 && col < m(0).length
}


val goal = MazeLocation(19, 19)

def goalTest(location: MazeLocation) : Boolean = location == goal

def successorsForMaze(mz: Maze) : (MazeLocation) =>  List[MazeLocation] = {

  def successor(location: MazeLocation) : List[MazeLocation] = {
    //val moves = List(location.up, location.down, location.left, location.right)
    val moves = List(location.left, location.right, location.up, location.down)

    val validMoves = moves filter ( move => move.isDefined(mz) && mz(move.row)(move.col) != Cell.Blocked)

    validMoves
  }

  successor
}

val start = MazeLocation(0, 0)
successorsForMaze(maze)(start)


class Stack[T](s: List[T]) {
  def isEmpty: Boolean = s.isEmpty
  def top = s.head
  def push(t: T) = new Stack( t :: s)
  def pushAll(ts: List[T]) = new Stack( ts.reverse ::: s)
  def pop = (s. head, new Stack(s.tail))

  override def toString: String = if (s.isEmpty) "Stack: EMPTY" else "Stack: " + s.head + "*, " + s.tail
}

object Stack {
  def empty[T]: Stack[T] = new Stack(List.empty[T])

  def apply[T](xs: T*): Stack[T] = xs.foldLeft(Stack.empty[T])((s, t) => s.push(t))
}

val s = Stack('a, 'b, 'c, 'd, 'e)

println(s)
println(s.pop)
println(s.pop._2.top)

println(s.pushAll(List('f, 'g, 'h)))

case class Node[T](state: T, parent: Option[Node[T]])


def dfs[T](initialState: T, isGoal: (T) => Boolean,
           expandState: T => List[T], maxDepth: Int) : Option[Node[T]] = {

  def loop(open: Stack[Node[T]], closed: Set[Node[T]], depth: Int, maxDepth: Int) : Option[Node[T]] = {
//    println("------------------------")
//    println("OPEN: " + open)
//    println("CLOSED: " + closed)
//    println("CURRENT PATH: " + currentPath)

    if (open.isEmpty) {
      println("No more nodes in OPEN")
      None //currentPath
    }
    else {

      val (node, ns) = open.pop
      if (isGoal(node.state)) return Some(node)//currentPath :+ node

      val newClosed = closed + node                      // mark n as visited node
      val expanded : List[T] = expandState(node.state)   // list of possible moves from n

      //println("Expanded: " + expanded)

      val closedStates: Set[T] = newClosed map { _.state}
      val newStates: List[T]= expanded filter { !closedStates.contains(_) }

      //println("New States: " + newStates)

      val newNodes = newStates.map{ s => Node(s, Some(node))}
      val newOpen = ns.pushAll(newNodes)

      if (depth + 1 > maxDepth) {
        println("MaxDepth reached: " + maxDepth)
        None // currentPath
      }
      else
        loop(newOpen, newClosed, depth + 1, maxDepth)//, currentPath :+ node)

    }
  }

  val closed = Set.empty[Node[T]]
  val path = List.empty[Node[T]]

  val startNode = Node(initialState, None)


  loop(Stack(startNode), closed, 0, maxDepth)//, path)

}

def nodeToPath[T](node: Option[Node[T]]) : List[T] =  {

  def loop(node: Node[T], currentPath: List[T]) : List[T] = {
    node.parent match {
      case None => node.state :: currentPath
      case Some(parent) => loop(parent, node.state :: currentPath)
    }
  }

  node match {
    case None => List.empty[T]
    case Some(node) => loop(node, List.empty[T])
  }

}

def markMaze(maze: Maze, path: List[MazeLocation], start: MazeLocation, goal: MazeLocation) : Maze = {
  import Cell._

  for (ml <- path) {
    maze(ml.row)(ml.col) = Path
  }

  maze(start.row)(start.col) = Start
  maze(goal.row)(goal.col) = Goal
  maze
}



markMaze(generateMaze(15,15,0.2), List(MazeLocation(1,0),MazeLocation(1,1),MazeLocation(1,2)), start, MazeLocation(14,14))
markMaze(maze, List.empty, start, goal)



def makeGoalF(m: Maze) : MazeLocation => Boolean = (l: MazeLocation) => m(l.row)(l.col) == Cell.Goal

val solution = dfs[MazeLocation](start, makeGoalF(maze), successorsForMaze(maze), 1000)
println(solution)

printMaze(maze)

println(solution)

val solPath: Seq[MazeLocation] = nodeToPath(solution)
println(solPath)


printMaze(markMaze(maze, nodeToPath(solution), start, goal))