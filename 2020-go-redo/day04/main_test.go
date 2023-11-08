package main

import (
	"fmt"
	"reflect"
	"testing"
)

func TestBuildPassport(t *testing.T) {
	s := "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd" +
		"\n" +
		"byr:1937 iyr:2017 cid:147 hgt:183cm"
	actual := BuildPassport(s)
	expected := map[string]string{
		"ecl": "gry",
		"pid": "860033327",
		"eyr": "2020",
		"hcl": "#fffffd",
		"byr": "1937",
		"iyr": "2017",
		"cid": "147",
		"hgt": "183cm",
	}

	if !reflect.DeepEqual(actual, expected) {
		t.Error("expected:", expected, "actual:", actual)
	}
}

func TestIsValid2(t *testing.T) {
	tests := []struct {
		s        string
		expected bool
	}{
		{
			"eyr:1972 cid:100 hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926",
			false,
		},
		{
			"iyr:2019 hcl:#602927 eyr:1967 hgt:170cm ecl:grn pid:012533040 byr:1946",
			false,
		},
		{
			"hcl:dab227 iyr:2012 ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277",
			false,
		},
		{
			"hgt:59cm ecl:zzz eyr:2038 hcl:74454a iyr:2023 pid:3556412378 byr:2007",
			false,
		},
		{
			"pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980 hcl:#623a2f",
			true,
		},
		{
			"eyr:2029 ecl:blu cid:129 byr:1989 iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm",
			true,
		},
		{
			"hcl:#888785 hgt:164cm byr:2001 iyr:2015 cid:88 pid:545766238 ecl:hzl eyr:2022",
			true,
		},
		{
			"iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719",
			true,
		},
	}

	for i, tt := range tests {
		testname := fmt.Sprintf("test %d", i)
		t.Run(testname, func(t *testing.T) {
			passport := BuildPassport(tt.s)
			actual := isValid2(passport)
			if actual != tt.expected {
				t.Error()
			}
		})
	}
}
