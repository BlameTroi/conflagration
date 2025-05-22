# `pycodestyle` exclusions

What kind of configuration file doesn't accept comments?

I mean really.

I'm aware of Python's preferred standards but I don't agree with them. I have a
very few outright violations of PEP-8 ignored, but most of the items ignored
below are noise warnings that are distracting while editing.

If I'm working in a foreign code base, I will adhere to its standards. In my
own code I will adhere to my standards.

As I can't insert this screed in the real `pycodestyle` file, what follows are
the exclusions and any additional commentary.

## Intentional violations

```
E111 Indentation is not a multiple of four
E128 Continuation line under-indented for visual indent
W191 Indentation contains tabs.
```

Indents should use tabs. See Rob Pike, the Go Language, and others for
justification. I'm not using proportional fonts, but if I did tabs would be
even more important.

Visually I find four spaces per tab too wide and two spaces per tab too narrow.
Three is the Goldilocks zone.

Formatters for Python have difficulty with alignment on continuation. Since I'm
already going against the grain, I'll just accept what the formatters do once
I've set indents and tab-width as I like them and turned off noise messages
that formatting will fix.

## White Space

### Horizontal White Space

```
E231 Missing whitespace after ,;:
E251 Unexpected spaces around keyword/parameter
E261 At least two spaces before inline comment
W291 Trailing whitespace.
W293 Blank line contains whitespace.
```

`yapf` fixes these.

### Vertical White Space

```
E301 Various misplaced or miscounted blank lines
E302
E303
E304
E305
E306
```

`yapf` fixes these.

### Line Length

```
E501 Line too long.
W505 Doc line too long.
```

`yapf` fixes these.

## Line Break or Continuation

```
W503 Line break before binary operator.
W504 Line break after binary operator.
```

I can't get `yapf` to reliably break -after- a binary operator, which in my
opinion is the correct way to do it. `yapf` will do what it does and I'll
ignore these warnings.
