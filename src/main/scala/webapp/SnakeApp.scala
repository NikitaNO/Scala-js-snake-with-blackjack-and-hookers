package webapp

import org.scalajs.dom.Element
import org.scalajs.dom.html.{ Canvas}
import org.scalajs.dom.raw.KeyboardEvent
import webapp.Direction.DirVal

import scala.scalajs.js.JSApp
import org.scalajs.dom
import org.scalajs.dom.ext._
import scala.scalajs.js.annotation.JSExport
import scala.util.Random

case class Block(var x: Int,var y : Int){
  def move(direction : DirVal) = {
   val (newX, newY) =  direction match {
      case Direction.Right => (x + 1, y)
      case Direction.Left => (x - 1, y)
      case Direction.Up => (x , y - 1)
      case Direction.Down => (x, y + 1)
      case _ => (x + 1, y)
    }
    Block(newX, newY)
  }
}

trait Move

object Direction extends Enumeration with Move{
  class DirVal extends Val with Move

  val Left = new DirVal
  val Up = new DirVal
  val Right = new DirVal
  val Down = new DirVal

  def random = Random.nextInt(4) match {
    case 1 => Left
    case 2 => Right
    case 3 => Up
    case 4 => Down
  }

  def opposite(d: DirVal) = d match {
    case Up => Down
    case Down => Up
    case Left => Right
    case Right => Left
  }
}

object Shot extends Move

object HitType extends Enumeration{
  val Self, Wall, Snakes = Value
}

object KeyBoard extends Enumeration{
  val Up1 = Value(38, "Up1")
  val Right1 = Value(39, "Right1")
  val Down1 = Value(40, "Down1")
  val Left1 = Value(37, "Left1")
  val Shot1 = Value(97, "Shot1") //numpad 1

  val Up2 = Value(87, "Up2") //w
  val Right2 = Value(68, "Right2") //r
  val Down2 = Value(83, "Down2") //s
  val Left2 = Value(65, "Left2") //a
  val Shot2 = Value(89, "Shot2") //y
}

object SnakeApp extends JSApp {
  val document = dom.document
  val window = dom.window
  val canvas = document.getElementById("canvas").cast[Canvas]
  val ctx = canvas.getContext("2d")
  val w = window.innerWidth
  val h = window.innerHeight
  val size = 10
  var snake1 = initSnake(1)
  var snake2 = initSnake(2)
  var bullets : Array[Bullet] = Array()
  var over = 0
  var hitType = HitType.Wall
  var food = new Food()
  var score = 0
  var game_loop = 0
  val reMenu = document.getElementById("reMenu")
  val menu = document.getElementById("menu")
  var speed = 10

  case class Bullet(val block : Block, val dir : DirVal) {
    def move() = block.move(dir)
  }

  case class Snake(var blocks: Array[Block] = initBlocks(),var direction: DirVal = Direction.Right,
                   val controls: Map[Move, KeyBoard.Value], var score : Int = 0, val isUser : Boolean = true, val scoreEl : Element) {
    def length = blocks.length

    def head = blocks.head

    def tail = blocks.tail

    def move(): Unit = {
      for(i <- (blocks.length - 1) to 1 by -1){
        blocks(i) = blocks(i - 1).copy()
      }
      blocks(0) = head.move(direction)
    }

    def fire() {
      println("fire!")
      bullets = bullets :+ Bullet(head.move(direction), direction)
      score -= 5
    }

    def afterFiredBlocks(b: Block) = {
      val (bef, aft) = blocks.splitAt(blocks.indexOf(b))
      bef ++ aft.tail
    }

    def eat(f: Food): Unit ={
      score += 10
    }
  }

  class Food {
    val x: Int = Math.round(Math.random() * (w - size) / size).toInt
    val y: Int = Math.round(Math.random() * (h - size) / size).toInt
    def draw(): Unit = {
      ctx.fillStyle = "white"
      ctx.fillRect(this.x*size, this.y*size, size, size)
    }
  }

  def initSnake(ind : Int) = ind match {
    case 1 =>
      new Snake(controls = Map(Direction.Up -> KeyBoard.Up1, Direction.Right -> KeyBoard.Right1,
        Direction.Down -> KeyBoard.Down1, Direction.Left -> KeyBoard.Left1, Shot -> KeyBoard.Shot1), scoreEl = document.getElementById("score1"))
    case 2 =>
      new Snake(blocks = initBlocks().map(_.copy(y = size)),
        controls =  Map(Direction.Up -> KeyBoard.Up2, Direction.Right -> KeyBoard.Right2,
          Direction.Down -> KeyBoard.Down2, Direction.Left -> KeyBoard.Left2, Shot -> KeyBoard.Shot2), scoreEl = document.getElementById("score2"))
    case 3 =>
      new Snake(controls = Map(), isUser = false, scoreEl = document.getElementById("score2")) // bot
    case 4 => throw new IllegalArgumentException("Only 3 snakes in current version!")
  }


  @JSExport
  def initCanvas() = {
    canvas.height = h
    canvas.width = w
  }

  def paintCanvas() {
    ctx.fillStyle = "black"
    ctx.fillRect(0, 0, w, h)
  }

  def initBlocks() : Array[Block] = {
    val length = 10
    val res = for(i <- (length - 1) to 0 by -1 ) yield new Block(i, 0)
    res.toArray
  }

  def paintSnake(s : Snake) {
    s.blocks.foreach{ s =>
      ctx.fillStyle = "white"
      ctx.fillRect(s.x * size, s.y * size, size, size)
    }
  }

  def paintBullets(): Unit ={
    bullets.map(_.block).foreach{ b =>
      ctx.fillStyle = "yellow"
      ctx.fillRect(b.x * size, b.y * size, size, size)
    }
  }

  def update(s: Snake) : Snake = {
    def onKeyDown = (e: KeyboardEvent) => {
      List(snake1, snake2).foreach { sn =>
        sn.controls.find(_._2.id == e.keyCode).map(_._1).foreach { dir =>
          if (dir.isInstanceOf[DirVal]) {
            if (sn.direction != Direction.opposite(dir.cast[DirVal]))
              sn.direction = dir.cast[DirVal]
          }
          else sn.fire()
        }
      }
      e.preventDefault()
    }
    val wallCollision = s.head.x >= w/size || s.head.x <= -1 || s.head.y >= h/size || s.head.y <= -1
    val foodCollision = s.head.x == food.x && s.head.y == food.y
    val selfCollision = s.blocks.tail.map(b => (b.x, b.y)).contains((s.head.x,s.head.y))
    val snakes = List(snake1, snake2)
    val snakesCollisions = snakes.map { sn =>
      val res = snakes.filterNot(_ == sn).map { sn2 =>
        sn.blocks.map(bl => (bl.x, bl.y)).contains(sn2.head.x, sn2.head.y)
      }
        res.filter(b => b)
    }.flatten.nonEmpty
    bullets.foreach(b => {
      val nextB = b.move()
      b.block.x = nextB.x
      b.block.y = nextB.y
    })
    val bulletsCollisions = bullets.map { b =>
      snakes.map { s =>
        s.blocks.map(b => (b.x, b.y)).find(_ == (b.block.x, b.block.y)).foreach { tup =>
          println("Block " + tup._1 + " " + tup._2 + " was fired!")
          if(s.blocks.size > 1) {
            s.blocks = s.afterFiredBlocks(b.block)
          } else gameover()
        }
      }
    }.flatten.nonEmpty
    document.onkeydown = onKeyDown
    if(!s.isUser){
      val x = food.x - s.head.x
      val horDir = if(x > 0) Some(Direction.Right)
      else if(x < 0 ) Some(Direction.Left)
      else None
      val y = food.y - s.head.y
      val vertDir = if(y > 0 ) Some(Direction.Up)
      else if(y < 0) Some(Direction.Down)
      else None
      val dirs = List(horDir,vertDir).filter(_.isDefined).filter(_ != s.direction)
      val nextDir = dirs(Random.nextInt(dirs.size)) getOrElse Direction.Down
      s.direction = nextDir

      snakes.filterNot(snake => snake != s).foreach{snakeUser =>
        val onTailX = snakeUser.head.x == s.head.x && snakeUser.direction == s.direction
        val onTailY = snakeUser.head.y == s.head.y && snakeUser.direction == s.direction
        s.direction = if(onTailX) List(Direction.Up, Direction.Down)(Random.nextInt(2))
        else if(onTailY) List(Direction.Right, Direction.Left)(Random.nextInt(2))
        else nextDir
      }
    }
    s.move()
    //Wall Collision
    if(wallCollision) {
        println("wall collision")
        hitType = HitType.Wall
        gameover()
        s
    }
    else if(foodCollision) {
      s.eat(food)
      food = new Food()
//      scoreText.innerHTML = "Score: "+score;
      speed = if(speed <= 45) speed + 1 else speed
      window.clearInterval(game_loop)
      game_loop = window.setInterval(draw, 1000/ speed)
      s.copy(blocks = s.blocks :+ new Block(food.x, food.y))
    }
    else if(selfCollision) {
            println("self collision")
            println(s.blocks.toList)
            hitType = HitType.Self
            gameover()
            s
        }
    else if(snakesCollisions) {
      println("snakes collision")
      hitType = HitType.Snakes
      gameover()
      s
    }
    else if(bulletsCollisions){
      println("bullet collision")
      s
    } else s
  }

  def draw = () => {
    paintCanvas()
    paintSnake(snake1)
    paintSnake(snake2)
    paintBullets()
    snake1 = update(snake1)
    snake2 = update(snake2)
    snake1.scoreEl.innerHTML = snake1.score.toString
    snake2.scoreEl.innerHTML = snake2.score.toString
    food.draw()
  }

  def reset() {
    snake1 = initSnake(1)
    snake2 = initSnake(2)
    food = new Food()
    over = 0
    if(game_loop != 0)  window.clearInterval(game_loop)
    game_loop = window.setInterval(draw, 1000 / speed)
    score = 0
//    scoreText.innerHTML = "Score: "+score
//    mainMusic.currentTime = 0
//    mainMusic.play()
  }

  def gameover() {
    println("gameover()")
    window.clearInterval(game_loop)
    reMenu.removeAttribute("hidden")
    //Show the messages
    if(hitType == HitType.Wall) {
//      goText.innerHTML = msgsWall[Math.floor(Math.random() * msgsWall.length)];
    }
    else if(hitType == HitType.Self) {
//      goText.innerHTML = msgsSelf[Math.floor(Math.random() * msgsSelf.length)];
    }
//    reMenu.style.zIndex = "1";
  }

  @JSExport
  def init() {
    println("init()")
    reMenu.setAttribute("hidden","true")
    menu.setAttribute("hidden","true")
    initCanvas()
    food = new Food()
    reset()
  }

  @scala.scalajs.js.annotation.JSExport
  override def main(): Unit = init()
}