package main

import (
	"fmt"
	"math"
	"os"
	"sort"
	"strings"
)

type ColString = string
type RowString = string

type Seat struct {
	rowNum, colNum int
}

func main() {
	const Filename = "data_test.txt"
	// const Filename = "data.txt"
	s, _ := os.ReadFile(Filename)
	seats := parse(string(s))

	var part1Answer int = part1(seats)
	fmt.Println("Part1 Answer:", part1Answer)

	var part2Answer int = part2(seats)
	fmt.Println("Part2 Answer:", part2Answer)
}

func part1(seats []Seat) int {
	var maxId int = -1
	for _, seat := range seats {
		maxId = int(
			math.Max(
				float64(maxId),
				float64(computeId(seat)),
			),
		)
	}
	return maxId
}

func part2(seats []Seat) int {
	var seatIds []int
	for _, seat := range seats {
		seatIds = append(seatIds, computeId(seat))
	}
	sort.Ints(seatIds)

	for i := 0; i < len(seats)-1; i++ {
		lower, upper := seatIds[i], seatIds[i+1]

		if lower+1 != upper {
			return lower + 1
		}
	}
	panic("couldn't find an answer for part2")
}

func parse(s string) []Seat {
	var ret []Seat
	var lines []string = strings.Split(s, "\n")

	for _, l := range lines {
		if l == "" {
			continue
		}

		rs := []rune(l)
		seat := Seat{
			rowNum: binaryToInt(runesToBinary(rs[:len(rs)-3])),
			colNum: binaryToInt(runesToBinary(rs[len(rs)-3:])),
		}

		ret = append(ret, seat)
	}

	return ret
}

func runesToBinary(rs []rune) []rune {
	var ret []rune
	for _, r := range rs {
		var digit rune
		switch r {
		case 'F', 'L':
			digit = '0'
		case 'B', 'R':
			digit = '1'
		}
		ret = append(ret, digit)
	}
	return ret
}

func binaryToInt(rs []rune) int {
	ret := 0
	exp := len(rs) - 1
	for _, digit := range rs {
		if digit == '1' {
			ret += int(math.Pow(2, float64(exp)))
		}
		exp--
	}
	return ret
}

func computeId(seat Seat) int {
	return (seat.rowNum * 8) + seat.colNum
}
