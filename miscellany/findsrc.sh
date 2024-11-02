#!/bin/bash
# a find for source code in projects using git and cmake
find . \( -path \*/.git\* -prune -o -path \*/build\* -prune -o -path \*/.cache\* -prune \) -o -name "$@" -print
