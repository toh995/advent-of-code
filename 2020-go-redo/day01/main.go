package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

func main() {
	const Filename = "data.txt"
	// const Filename = "data_test.txt"
	var nums []int = parse(Filename)

	var part1Answer int = part1(nums)
	fmt.Println("Part1 Answer:", part1Answer)

	var part2Answer int = part2(nums)
	fmt.Println("Part2 Answer:", part2Answer)
}

func parse(filename string) []int {
	data, _ := os.ReadFile(filename)
	var lines []string = strings.Split(string(data), "\n")

	// convert each line to an int
	var ret []int
	for _, l := range lines {
		if l == "" {
			continue
		}
		n, err := strconv.Atoi(l)
		if err != nil {
			panic("couldn't convert string to int")
		}
		ret = append(ret, n)
	}
	return ret
}

func part1(nums []int) int {
	const TargetSum = 2020
	var n1, n2 int = findPair(TargetSum, nums)
	return n1 * n2
}

func part2(nums []int) int {
	const TargetSum = 2020
	var n1, n2, n3 int = findTrio(TargetSum, nums)
	return n1 * n2 * n3
}

func findPair(targetSum int, nums []int) (int, int) {
	// iterate through the nums, and
	// make a hashmap of all nums
	m := make(map[int]bool, len(nums))
	for _, n := range nums {
		m[n] = true
	}
	// now check for matches
	for _, n := range nums {
		other := targetSum - n
		if _, ok := m[other]; ok {
			return n, other
		}
	}
	panic("couldn't find pair")
}

func findTrio(targetSum int, nums []int) (int, int, int) {
	// iterate through the nums, and
	// make a hashmap of all nums
	m := make(map[int]bool, len(nums))
	for _, n := range nums {
		m[n] = true
	}
	// now check for a match
	for i := range nums {
		for j := range nums[i+1:] {
			n := targetSum - (nums[i] + nums[j])
			if _, ok := m[n]; ok {
				return n, nums[i], nums[j]
			}
		}
	}
	panic("couldn't find trio")
}
