package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

type Passport = map[string]string

func main() {
	// const Filename = "data_test.txt"
	const Filename = "data.txt"
	s, _ := os.ReadFile(Filename)
	passports := Parse(string(s))

	var part1Answer int = part1(passports)
	fmt.Println("Part1 Answer:", part1Answer)

	var part2Answer int = part2(passports)
	fmt.Println("Part2 Answer:", part2Answer)
}

func part1(passports []Passport) int {
	var ret int = 0
	for _, p := range passports {
		if isValid1(p) {
			ret++
		}
	}
	return ret
}

func part2(passports []Passport) int {
	var ret int = 0
	for _, p := range passports {
		if isValid1(p) && isValid2(p) {
			ret++
		}
	}
	return ret
}

func isValid1(p Passport) bool {
	var requiredFields = map[string]bool{
		"byr": true,
		"iyr": true,
		"eyr": true,
		"hgt": true,
		"hcl": true,
		"ecl": true,
		"pid": true,
	}
	for k := range p {
		delete(requiredFields, k)
	}
	return len(requiredFields) == 0
}

func isValid2(p Passport) bool {
	for k, v := range p {
		if !validateKV2(k, v) {
			return false
		}
	}
	return true
}

func validateKV2(k, v string) bool {
	switch k {
	case "byr":
		num, err := strconv.Atoi(v)
		if err != nil {
			return false
		}
		return 1920 <= num && num <= 2002
	case "iyr":
		num, err := strconv.Atoi(v)
		if err != nil {
			return false
		}
		return 2010 <= num && num <= 2020
	case "eyr":
		num, err := strconv.Atoi(v)
		if err != nil {
			return false
		}
		return 2020 <= num && num <= 2030
	case "hgt":
		asRunes := []rune(v)
		fst := asRunes[:len(asRunes)-2]
		snd := asRunes[len(asRunes)-2:]
		unit := string(snd)
		num, err := strconv.Atoi(string(fst))
		if err != nil {
			return false
		}
		switch unit {
		case "cm":
			return 150 <= num && num <= 193
		case "in":
			return 59 <= num && num <= 76
		}
	case "hcl":
		asRunes := []rune(v)
		if asRunes[0] != '#' {
			return false
		}
		// todo: move validRunes outside of this fn.
		// it's inefficient to re-allocate memory each time
		validRunes := make(map[rune]bool)
		for _, r := range [...]rune{'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'} {
			validRunes[r] = true
		}
		for _, r := range asRunes[1:] {
			if !validRunes[r] {
				return false
			}
		}
		return true
	case "ecl":
		return v == "amb" ||
			v == "blu" ||
			v == "brn" ||
			v == "gry" ||
			v == "grn" ||
			v == "hzl" ||
			v == "oth"
	case "pid":
		// todo: move digits outside of this fn.
		// it's inefficient to re-allocate memory each time
		digits := map[rune]bool{
			'0': true,
			'1': true,
			'2': true,
			'3': true,
			'4': true,
			'5': true,
			'6': true,
			'7': true,
			'8': true,
			'9': true,
		}
		if len(v) != 9 {
			return false
		}
		asRunes := []rune(v)
		for _, r := range asRunes {
			if !digits[r] {
				return false
			}
		}
		return true
	case "cid":
		return true
	}
	return false
}

func Parse(s string) []Passport {
	var ret []Passport
	var subs = strings.Split(s, "\n\n")
	for _, sub := range subs {
		passport := BuildPassport(sub)
		ret = append(ret, passport)
	}
	return ret
}

func BuildPassport(s string) Passport {
	var ret = make(map[string]string)

	// split the string into pairs
	splitter := func(r rune) bool {
		return r == ' ' || r == '\n'
	}
	var pairs []string = strings.FieldsFunc(s, splitter)

	// insert each pair into the map
	for _, pair := range pairs {
		k, v, _ := strings.Cut(pair, ":")
		ret[k] = v
	}

	return ret
}
