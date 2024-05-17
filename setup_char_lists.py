from wcwidth.table_zero import ZERO_WIDTH
from wcwidth.table_wide import WIDE_EASTASIAN

from pathlib import Path

UNICODE_VERSION = '15.1.0'

OUTPUT_FILE = Path(__file__).parent / "wcwidth" / "char_list.ml"

zero_width_tuples = "\n   ; ".join([
    f"0x{t[0]:05x}, 0x{t[1]:05x}"
    for t in ZERO_WIDTH[UNICODE_VERSION]
])

wide_tuples = "\n   ; ".join([
    f"0x{t[0]:05x}, 0x{t[1]:05x}"
    for t in WIDE_EASTASIAN[UNICODE_VERSION]
])

with open(OUTPUT_FILE, "w") as f:
    print(f"""
(** Zero-width characters as of Unicode {UNICODE_VERSION}.
    Taken from the Python wcwidth package:
    https://pypi.org/project/wcwidth/ *)
let zero_width_chars =
  [| {zero_width_tuples}
  |]
;;

(** Wide characters as of Unicode {UNICODE_VERSION}.
    Taken from the Python wcwidth package:
    https://pypi.org/project/wcwidth/ *)
let wide_chars =
  [| {wide_tuples}
  |]
;;
""".strip(), file=f)
