# wcwidth

Implementation of the `wcwidth(3)` and `wcswidth(3)` functions for OCaml, which lets you calculate the number of columns a string occupies in a terminal.

## Usage

Add `wcwidth` to your project dependencies.

```
# Wcwidth.wcswidth "hello";;
- : int = 5
# Wcwidth.wcswidth "🦆";;
- : int = 2
```

## Update the character lists

The Unicode version the library uses is documented in `wcwidth/char_list.ml`.

This file also contains lists of zero-width and wide characters, and is derived from the Python [`wcwidth` package](https://pypi.org/project/wcwidth/).
This file can be updated (e.g. to use a new Unicode version) using the Python script `setup_char_lists.py`:

```
python -m venv .venv
source .venv/bin/activate
python -m pip install wcwidth
python setup_char_lists.py
```
