import std/core/exn
import std/os/file
import std/os/path

val file-path = path("./2021-koka-redo/day03/data.txt")

fun main() : _ ()
  val rows = read-text-file(file-path).parse-rows
  val part1-answer = part1(rows)
  println("PART1: " ++ part1-answer.show)
  val part2-answer = part2(rows)
  println("PART2: " ++ part2-answer.show)

fun part1( rows : list<bitstring> ) : int
  val cols = rows.transpose
  val gamma = cols.map(most-common-bit)
  val epsilon = gamma.inverse
  gamma.to-decimal * epsilon.to-decimal

fun part2( rows : list<bitstring> ) : pure int
  val o = oxygen-rating(rows).to-decimal
  val co2 = co2-rating(rows).to-decimal
  o * co2

fun oxygen-rating( rows : list<bitstring> ) : pure bitstring
  fun oxygen-rating-recursive( curr-rows : list<vector<bit>>, i : int ) : pure bitstring
    match curr-rows
      Cons(row, []) -> row.list
      Cons(_, _) -> ({
        val b = curr-rows.map(fn(row) { row[i] }).most-common-bit
        val next-rows = curr-rows.filter fn(row) { row[i] == b }
        oxygen-rating-recursive(next-rows, i + 1)
      })()
  val start-rows = rows.map fn(row) { row.vector }
  oxygen-rating-recursive(start-rows, 0)

fun co2-rating( rows : list<bitstring> ) : pure bitstring
  fun co2-rating-recursive( curr-rows : list<vector<bit>>, i : int ) : pure bitstring
    match curr-rows
      Cons(row, []) -> row.list
      Cons(_, _) -> ({
        val b = curr-rows.map(fn(row) { row[i] }).most-common-bit.inverse
        val next-rows = curr-rows.filter fn(row) { row[i] == b }
        co2-rating-recursive(next-rows, i + 1)
      })()
  val start-rows = rows.map fn(row) { row.vector }
  co2-rating-recursive(start-rows, 0)

fun most-common-bit( bs : list<bit> ) : bit
  var ones := 0
  var zeros := 0
  foreach (bs) fn(b)
    match b
      One -> ones := ones + 1
      Zero -> zeros := zeros + 1
  if ones >= zeros
    then One
    else Zero

fun parse-rows( s : string ) : exn list<list<bit>>
  s.lines.map(parse-row)

fun parse-row( s : string ) : exn list<bit>
  s.list.map(parse-bit)

fun parse-bit( c : char ) : exn bit
  match c
    '1' -> One
    '0' -> Zero

type bit
  One
  Zero

fun bit/inverse( b : bit ) : bit
  match b
    One -> Zero
    Zero -> One

fun bit/(==)( b1 : bit, b2 : bit ) : bool
  (b1.is-one && b2.is-one)
    || (b1.is-zero && b2.is-zero)

alias bitstring = list<bit>

fun bitstring/inverse( bs : bitstring ) : bitstring
  bs.map(inverse)

fun to-decimal( bs : bitstring ) : int
  var ret := 0
  var pow := 0
  bs.reverse.foreach fn(b)
    if b.is-one
      then ret := ret + 2^pow
    pow := pow + 1
  ret 

fun transpose( rows : list<list<a>> ) : list<list<a>>
  fun transpose-nonempty( first-row : list<a>, rest : list<list<a>> ) : list<list<a>>
    val init = first-row.map(single)
    rest
      .foldl(init, fn(acc, row){ zipwith(row, acc, Cons) })
      .map(reverse)
  match rows
    Cons(first-row, rest) -> transpose-nonempty(first-row, rest)
    Nil -> []