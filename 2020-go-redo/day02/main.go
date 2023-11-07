package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
	"unicode/utf8"
)

type Password = string

type Policy struct {
	char rune
	min  int
	max  int
}

func (p Policy) String() string {
	return "{" +
		"char: " + string(p.char) +
		", min: " + strconv.Itoa(p.min) +
		", max: " + strconv.Itoa(p.max) +
		"}"
}

func main() {
	const Filename = "data.txt"
	// const Filename = "data_test.txt"
	m := parse(Filename)

	var part1Answer int = part1(m)
	fmt.Println("Part1 Answer:", part1Answer)

	var part2Answer int = part2(m)
	fmt.Println("Part2 Answer:", part2Answer)
}

func part1(m map[Policy][]Password) int {
	return countValid(isValid1, m)
}
func part2(m map[Policy][]Password) int {
	return countValid(isValid2, m)
}

func countValid(
	validator func(Policy, Password) bool,
	m map[Policy][]Password,
) int {
	ret := 0
	for policy, passwords := range m {
		for _, password := range passwords {
			if validator(policy, password) {
				ret++
			}
		}
	}
	return ret
}

func isValid1(policy Policy, password Password) bool {
	count := strings.Count(password, string(policy.char))
	return policy.min <= count && count <= policy.max
}

func isValid2(policy Policy, password Password) bool {
	rs := []rune(password)
	// adjust for 0-based indexing
	r1 := rs[policy.min-1]
	r2 := rs[policy.max-1]

	count := 0
	if r1 == policy.char {
		count++
	}
	if r2 == policy.char {
		count++
	}
	return count == 1
}

func parse(filename string) map[Policy][]Password {
	data, _ := os.ReadFile(filename)
	var lines []string = strings.Split(string(data), "\n")

	// build up the map and return
	ret := make(map[Policy][]Password)

	for _, l := range lines {
		if l == "" {
			continue
		}
		split := strings.Split(l, ": ")
		if len(split) != 2 {
			panic("malformed line")
		}

		policy := parsePolicy(split[0])
		password := split[1]

		ret[policy] = append(ret[policy], password)
	}

	return ret
}

// Takes a string of the form i.e. "1-2 a"
func parsePolicy(s string) Policy {
	var split []string
	split = strings.Split(s, " ")
	charStr := split[1]

	split = strings.Split(split[0], "-")
	minStr := split[0]
	maxStr := split[1]

	// convert + validate
	var char rune
	var minInt, maxInt int
	var err error

	char, _ = utf8.DecodeRuneInString(charStr)
	if char == utf8.RuneError ||
		utf8.RuneCountInString(charStr) != 1 {
		panic("malformed policy")
	}

	minInt, err = strconv.Atoi(minStr)
	if err != nil {
		panic("malformed policy")
	}

	maxInt, err = strconv.Atoi(maxStr)
	if err != nil {
		panic("malformed policy")
	}

	return Policy{char: char, min: minInt, max: maxInt}
}
