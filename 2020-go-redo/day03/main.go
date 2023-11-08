package main

import (
	"fmt"
	"os"
	"strings"
)

type Coord struct {
	i int
	j int
}

type Grid = [][]CellType

type CellType = rune

const (
	Open CellType = '.'
	Tree CellType = '#'
)

func main() {
	const Filename = "data.txt"
	// const Filename = "data_test.txt"
	s, _ := os.ReadFile(Filename)
	g := buildGrid(string(s))

	var part1Answer int = part1(g)
	fmt.Println("Part1 Answer:", part1Answer)

	var part2Answer int = part2(g)
	fmt.Println("Part2 Answer:", part2Answer)
}

func part1(g *Grid) int {
	return countTrees(g, 1, 3)
}

func part2(g *Grid) int {
	return countTrees(g, 1, 1) *
		countTrees(g, 1, 3) *
		countTrees(g, 1, 5) *
		countTrees(g, 1, 7) *
		countTrees(g, 2, 1)
}

func Print(g *Grid) {
	for _, row := range *g {
		fmt.Println(string(row))
	}
}

func buildGrid(s string) *Grid {
	var ret [][]rune
	var lines []string = strings.Split(s, "\n")
	for _, l := range lines {
		if l == "" {
			continue
		}
		ret = append(ret, []rune(l))
	}
	return &ret
}

func maxI(g *Grid) int {
	return len(*g)
}

func maxJ(g *Grid) int {
	if len(*g) <= 0 {
		return 0
	}
	return len((*g)[0])
}

func countTrees(g *Grid, iDelta, jDelta int) (count int) {
	coord := Coord{0, 0}
	count = 0
	for k := 0; k < maxI(g)/iDelta; k++ {
		if getCellType(g, coord) == Tree {
			count++
		}
		// move to the next coord
		coord = NextCoord(g, iDelta, jDelta, coord)
		fmt.Println(coord)
	}
	return count
}

func NextCoord(g *Grid, iDelta, jDelta int, c Coord) Coord {
	newI := (c.i + iDelta) % maxI(g)
	newJ := (c.j + jDelta) % maxJ(g)
	return Coord{newI, newJ}
}

func getCellType(g *Grid, c Coord) CellType {
	return (*g)[c.i][c.j]
}
