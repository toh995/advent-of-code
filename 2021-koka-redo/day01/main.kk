import std/core/undiv
import std/os/file
import std/os/path
import std/text/parse

val file-path = path("./2021-koka-redo/day01/data.txt")

fun main() : _ ()
  val ints : list<int> = read-text-file(file-path).parse-ints
  val part1-answer = part1(ints)
  print("PART 1: "); println(part1-answer)
  val part2-answer = part2(ints)
  print("PART 2: "); println(part2-answer)

fun part1(ints : list<int>) : int
  ints
    .adj-pairs
    .filter(fn((x, y)){ y > x })
    .length

fun part2(ints : list<int>) : int
  ints
    .adj-triples
    .map(fn((x,y,z)){ x + y + z })
    .adj-pairs
    .filter(fn((x,y)){ y > x })
    .length

fun adj-pairs(xs : list<a>) : list<(a, a)>
  match xs
    Cons(x, Cons(y, rest)) -> Cons((x,y), Cons(y, rest).pretend-decreasing.adj-pairs)
    _ -> []

fun adj-triples(xs : list<a>) : list<(a, a, a)>
  match xs
    Cons(x, Cons(y, Cons(z, rest))) -> Cons((x,y,z), [y,z].append(rest).pretend-decreasing.adj-triples)
    _ -> []

fun parse-ints(s : string) : exn list<int>
  val perr = s.slice.parse{ sep-by1(pnat, { char('\n') }) }
  match perr
    ParseOk(is, _) -> is
    ParseError(msg, _) -> throw-exn(Exception(msg, ExnError))

fun sep-by1(p : parser<e, a>, sep : parser<e, b>) : <parse | e> list<a>
  Cons(p(), many{ sep(); p(); })