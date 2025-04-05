import std/os/file
import std/text/parse
import std/os/path

import std/data/hash
import std/data/hashmap

val file-path = path("./2021-koka-redo/day04/data.txt")

fun main() : _ ()
  val (moves, boards) = read-text-file(file-path).parse.unright
  val part1-answer = part1(moves, boards)
  println("PART 1: " ++ part1-answer.show)
  val part2-answer = part2(moves, boards)
  println("PART 2: " ++ part2-answer.show)

fun part1( moves : list<int>, boards : list<board> ) : exn int
  val (board, move) = first-winning-board-and-move(moves, boards)
  score(board, move)

fun part2( moves : list<int>, boards : list<board> ) : exn int
  val (board, move) = last-winning-board-and-move(moves, boards)
  score(board, move)

struct board
  val-to-coords : box<hash-map<int, list<coord>>>
  marked : box<hash-map<coord, bool>>
  i-max : int
  j-max : int

alias coord = (int, int)

fun score( board : board, move : int ) : int
  move * board.unmarked-nums.sum

fun unmarked-nums( board : board ) : list<int>
  var ret := []
  board.val-to-coords.unbox.foreach fn(value, coords)
    coords.foreach fn(coord)
      val is-marked = board.marked.unbox.get(coord, ?hash=coord/hash).default(False)
      if !is-marked
      then ret := value.Cons(ret)
  ret

fun first-winning-board-and-move( moves : list<int>, boards : list<board> ) : exn (board, int)
  match moves
    Cons(move, movess) ->
      val boards' = boards.map fn(board) { board.mark(move) }
      match boards'.list/find(has-won)
        Just(board) -> (board, move)
        Nothing -> first-winning-board-and-move(movess, boards')

fun last-winning-board-and-move( moves : list<int>, boards : list<board> ) : exn (board, int)
  match moves
    Cons(move, movess) ->
      val boards' = boards.map fn(board) { board.mark(move) }
      val (winners, losers) = boards'.partition(has-won)
      match (winners, losers)
        ([winner], []) -> (winner, move)
        _ -> last-winning-board-and-move(movess, losers)

fun mark( board : board,  num : int ) : board
  match board.val-to-coords.unbox.get(num)
    Nothing -> board
    Just(coords) ->
      coords.foldl(board) fn(acc, coord)
        board( marked = board.marked.unbox.insert(coord, True, ?hash=coord/hash).Box )

fun has-won( board : board ) : bool
  val is = range(board.i-max + 1)
  val js = range(board.j-max + 1)
  // check rows
  var foundMatch := is.any fn(i)
    js.all fn(j)
      board.marked.unbox.get((i, j)).default(False)
  if foundMatch then return True
  // check cols
  foundMatch := js.any fn(j)
    is.all fn(i)
      board.marked.unbox.get((i, j)).default(False)
  foundMatch

fun rows-to-board( rows : list<list<int>> ) : either<string, board>
  match rows
    [] -> Left("board requires at least one row")
    Cons(first-row, _) ->
      Right(
        Board(
          val-to-coords = Box(build-val-to-coords(rows)),
          marked = Box(build-marked(rows)),
          i-max = rows.length - 1,
          j-max = first-row.length - 1
        )
      )

fun build-val-to-coords( rows : list<list<int>> ) : hash-map<int, list<coord>>
  var kvPairs := []
  foreach-indexed(rows) fn(i, row)
    foreach-indexed(row) fn(j, value)
      val kvPair = (value, [(i, j)])
      kvPairs := kvPair.Cons(kvPairs)
  std/data/hashmap/thread/list/hash-map(kvPairs)

fun build-marked( rows : list<list<int>> ) : hash-map<coord, bool>
  var kvPairs := []
  foreach-indexed(rows) fn(i, row)
    foreach-indexed(row) fn(j, value)
      val kvPair = ((i,j), False)
      kvPairs := kvPair.Cons(kvPairs)
  std/data/hashmap/thread/list/hash-map(kvPairs, ?hash=coord/hash)

fun coord/hash( (x, y) : coord, seed : int64 ) : int
  std/data/hash/list/hash([x, y], std/data/hash/int/hash)
 
fun range( n : int ) : list<int>
  var ret := []
  for(n) fn(i)
    ret := Cons(i, ret)
  ret

fun unright( e : either<string,b> ) : exn b
  match e
    Left(msg) -> throw(msg)
    Right(b) -> b

// PARSING
fun parse( s : string ) : either<string, (list<int>, list<board>)>
  val ret = s.slice.parse
    val moves = pmoves()
    char('\n'); char('\n')
    val boards = sep-by1(pboard) { char('\n'); char('\n') }
    (moves, boards)
  ret.either

fun pmoves() : parse list<int>
  sep-by1( pnat, { char(',') } )

fun pboard() : parse board
  val rows = prows()
  val board = rows-to-board(rows)
  match board
    Left(msg) -> fail(msg)
    Right(board) -> board

fun prows() : parse list<list<int>>
  sep-by1( prow, { char('\n') } )

fun prow() : parse list<int>
  optional([], spaces1)
  sep-by1( pnat, spaces1 )

fun spaces1() : parse list<char>
  many1 { char(' ') }

fun sep-by1(p : parser<e, a>, sep : parser<e, b>) : <parse | e> list<a>
  Cons(p(), many{ sep(); p(); })