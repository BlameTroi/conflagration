# this find works to filter out .git and .cache and other cmake artifacts, now we just
# need to do a grep with that info.
find . \( -path \*/.git\* -prune -o -path \*/build\* -prune -o -path \*/.cache\* -prune \) -o -name "$@" -print
