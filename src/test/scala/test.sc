import webapp.Block

val ar: Array[Int] = Array(1,2)
ar(0) = 2
ar.toList
def initSnake() : Array[Block] = {
  val length = 10
  val res = for(i <- (length - 1) to 0 by -1 ) yield new Block(i, 0)
  res.toArray
}

val blocks = initSnake()
blocks.toList
for(i <- (blocks.length - 1) until 0 by -1){
  blocks(i) = blocks(i - 1)
}
blocks.toList
var length = 10
val v = for(i <- (length - 1) to 0 by -1 ) yield new Block(i, 0)
v.toList
initSnake().map(_.x).toList