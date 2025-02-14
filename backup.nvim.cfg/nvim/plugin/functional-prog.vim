" functional-prog.vim

" ===========================================================================
" functional programming helpers from Losh's lvsthw plus whatever
" ideas working through it inspires.
"
" Troy Brumley
" blametroi@gmail.com
"  
" Copyright 2023 by Troy Brumley, all rights reserved.
" ===========================================================================

" ===========================================================================
" for most of these, think of the functional wrappers as "the list if it was
" <action>ed.
"
" never modify the original data. instead, copy and return new.
"
" immutable is the word.
"
" function pointers or references are stored in variables. variables names
" for those referencing functions must begin with a capital letter. 
"
" Losh's originals all worked with lists, but making these functions work
" for dictionaries was left as an exercise. after review, strings and
" sometimes blobs also make sense. i worked through a few and getting filter
" to work with strings should establish my bona fides so i'm reverting back
" to mostly just lists for now.
" ===========================================================================


" ===========================================================================
" a proper sorter
" ===========================================================================
function! Sorted(l)
  let dcl = deepcopy(a:l)
  call sort(dcl)
  return dcl
endfunction


" ===========================================================================
" a reversal
" ===========================================================================
function! Reversed(l)
  let dcl = deepcopy(a:l)
  call reverse(dcl)
  return dcl
endfunction


" ===========================================================================
" append an item returning a new list
" ===========================================================================
function! Append(l, val)
  let dcl = deepcopy(a:l)
  call add(dcl, a:val)
  return dcl
endfunction


" ===========================================================================
" associate returns a new list with item at i set to val
"
" i don't like the name Assoc, though i see how he gets to it.
" ===========================================================================
function! Assoc(l, i, val)
  let dcl = deepcopy(a:l)
  let dcl[a:i] = a:val
  return dcl
endfunction

" using a function reference for a better name
let Modifier = function("Assoc")


" ===========================================================================
" return a new list with item at i removed, or a new dictionary with the
" entry at key i removed.
"
" i don't like the name Pop for this, i think of Push and Pop in terms of
" stacks and queues.
" ===========================================================================
function! Pop(l, i)
  let dcl = deepcopy(a:l)
  call remove(dcl, a:i)
  return dcl
endfunction

" using a function reference for a better name
let Remover = function("Pop")


" ============================================================================
" high order functions:
"
" functions that accept function references as parameters.
" ============================================================================


" ============================================================================
" map every item in list with function, returning a list of the results
"
" strings aren't always treated as lists of characters, so some gyrations are
" needed to map each character in a string. there might be a better way to
" do this, but it works for now.
" ============================================================================
function! Mapped(fn, l)
  let dcl = deepcopy(a:l)
  if type(dcl) == v:t_list || type(dcl) == v:t_blob
    call map(dcl, string(a:fn) . "(v:val)")
  elseif type(dcl) == v:t_dict
    call map(dcl, string(a:fn) . "(v:key, v:val)")
  elseif type(dcl) == v:t_string
    let dcl = []
    for c in a:l
      call add(dcl, c)
    endfor
    call map(dcl, string(a:fn) . "(v:val)")
    let s = ""
    for c in dcl
      let s = s . c
    endfor
    let dcl = s
  endif  
  return dcl
endfunction


" ============================================================================
" return all the items in the list that answer not false to predicate function
"
" blobs or dictionaries pass index or key so i don't see this as useful in
" the sense of producing a new blob or dictionary since the actual blob or
" dictionary is not accessible.
" ============================================================================
function! Filtered(fn, l)
  let dcl = deepcopy(a:l)
  call filter(dcl, string(a:fn) . "(v:val)")
  endif
  return dcl
endfunction


" ============================================================================
" return all the items in the list that answer false to predicate function
" ============================================================================
function! Removed(fn, l)
  let dcl = deepcopy(a:l)
  call filter(dcl, "!" . string(a:fn) . "(v:val"))
  return dcl
endfunction


" ============================================================================
" reduce or fold (foldl i think) given an initial accumulator or result
" ============================================================================
function! Reduced(fn, l, a)
  let dcl = deepcopy(a:l)
  let res = call reduce(dcl, string(a:fn) . "(v:val)", a:a)
  return res
endfunction


" ============================================================================
" end functional-prog.vim
" ============================================================================
" vim: ts=2 sts=2 sw=2 et
