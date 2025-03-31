import std/os/file
import std/os/path
import std/text/parse

val file-path = path("./2021-koka-redo/day02/data.txt")

fun main() : _ ()
  val instructions = read-text-file(file-path).parse-instructions
  val part1-answer = part1(instructions)
  println("PART 1: " ++ part1-answer.show)
  val part2-answer = part2(instructions)
  println("PART 2: " ++ part2-answer.show)

fun part1( instructions : list<instruction> ) : int
  val start-coord = (0, 0)
  val end-coord = instructions.foldl(start-coord, move)
  val (i, j) = end-coord
  i * j

alias coord = (int, int)

fun coord/move( (i, j) : coord, ins : instruction ) : coord
  match ins.direction
    Forward -> (i, j + ins.distance)
    Down -> (i + ins.distance, j)
    Up -> (i - ins.distance, j)

fun part2( instructions : list<instruction> ) : int
  val start-pos = Position(coord=(0,0), aim=0)
  val end-pos = instructions.foldl(start-pos, move)
  val (i, j) = end-pos.coord
  i * j

struct position
  coord : coord
  aim : int

fun position/move( pos : position, instruction : instruction ) : position
  val (i, j) = pos.coord
  val aim = pos.aim
  val distance = instruction.distance
  match instruction.direction
    Forward -> pos(coord=(
                      i + (aim * distance),
                      j + distance))
    Down -> pos(aim = aim + distance)
    Up -> pos(aim = aim - distance)

value struct instruction
  direction : direction
  distance : int

type direction
  Forward
  Down
  Up

fun instruction/show( i : instruction ) : string
  "instruction{direction: " ++ i.direction.show ++ ", distance: " ++ i.distance.show ++ "}"

fun direction/show( d : direction ) : string
  match d
    Forward -> "Forward"
    Down -> "Down"
    Up -> "Up"

fun parse-instructions( s : string ) : exn list<instruction>
  val perr = s.slice.parse{ sep-by1(pinstruction, { char('\n') }) }
  match perr
    ParseOk(ret, _) -> ret
    ParseError(msg, _) -> throw-exn(Exception(msg, ExnError))

fun sep-by1(p : parser<e, a>, sep : parser<e, b>) : <parse | e> list<a>
  Cons(p(), many{ sep(); p(); })

fun pinstruction() : <parse|e> instruction
  val dir = pdirection()
  char(' ')
  val dist = pnat()
  Instruction(dir, dist)

fun pdirection() : <parse|e> direction
  choose([
    { pstring("forward"); Forward },
    { pstring("down"); Down },
    { pstring("up"); Up },
  ])