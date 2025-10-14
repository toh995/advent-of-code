import itertools
import os
from collections import Counter
from typing import Final, Literal

type Char = str
type Index = int

type PixelVal = Literal["0", "1", "2"]

NUM_ROWS: Final = 6
NUM_COLS: Final = 25

BLACK: Final = "0"
WHITE: Final = "1"
TRANSPARENT: Final = "2"

ALL_PIXEL_VALS = frozenset[PixelVal]({BLACK, WHITE, TRANSPARENT})


class EncodedImage:
    _pixel_arr: list[PixelVal]

    def __init__(self, digit_str: str) -> None:
        self._pixel_arr = []
        for digit in digit_str:
            assert digit in ALL_PIXEL_VALS
            self._pixel_arr.append(digit)

    def get_pixel_val(self, *, i: int, j: int, layer: int) -> PixelVal:
        arr_idx = j + (i * NUM_COLS)
        arr_idx += layer * NUM_ROWS * NUM_COLS
        return self._pixel_arr[arr_idx]


def main() -> None:
    data_path = os.path.join(os.path.dirname(__file__), "data.txt")
    with open(data_path) as file:
        input_str = file.read().strip("\n")

    part1_answer = part1(input_str)
    print(f"PART1: {part1_answer}")

    encoded_image = EncodedImage(input_str)
    part2(encoded_image)
    # part2_answer = part2(program)
    # print(f"PART2: {part2_answer}")


def part1(input_str: str) -> int:
    layer_size = NUM_ROWS * NUM_COLS
    layers = itertools.batched(input_str, n=layer_size, strict=True)

    min_num_zeros = None
    target_counter = None
    for layer in layers:
        curr_counter = Counter(layer)
        has_new_minimum = min_num_zeros is None or curr_counter["0"] < min_num_zeros
        if has_new_minimum:
            min_num_zeros = curr_counter["0"]
            target_counter = curr_counter
    assert target_counter is not None
    return target_counter["1"] * target_counter["2"]


def part2(encoded_image: EncodedImage) -> None:
    image = decode_image(encoded_image)
    for row in image:
        for i, tile_val in enumerate(row):
            if tile_val == "0":
                row[i] = " "
        row_str = "".join(row)
        print(row_str)


def decode_image(encoded_image: EncodedImage) -> list[list[Char]]:
    ret = [[""] * NUM_COLS for _ in range(NUM_ROWS)]
    for i in range(NUM_ROWS):
        for j in range(NUM_COLS):
            ret[i][j] = get_final_pixel_val(i, j, encoded_image)
    return ret


def get_final_pixel_val(i: int, j: int, encoded_image: EncodedImage) -> PixelVal:
    layer = 0
    TARGET_PIXEL_VALS = {BLACK, WHITE}
    while True:
        curr_pixel_val = encoded_image.get_pixel_val(i=i, j=j, layer=layer)
        if curr_pixel_val in TARGET_PIXEL_VALS:
            return curr_pixel_val
        layer += 1


if __name__ == "__main__":
    main()
