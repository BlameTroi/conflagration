" Vim syntax file
" Language:	ObjectPascal
" Previous Maintainer: (pascal.vim) Xavier Crégut <xavier.cregut@enseeiht.fr>	
" Current  Maintainer: Thorsten Maerz <info@netztorte.de>
" Last Change:	2010 April 30
" And now ripping out a lot of stuff to get down to a minimal
" iso pascal syntax file.
" t.brumley, january 2019.

" Remove any old syntax stuff hanging around
syn clear
syn case ignore
syn sync lines=250

if !exists("pascal_traditional")
endif

if !exists("did_pascal_syntax_inits")
  let did_pascal_syntax_inits = 1
  " The default methods for highlighting.  Can be overridden later
endif

" ------------------------------------------------------------------------------

syn keyword pascalTodo 			contained	TODO
syn region  pascalString		start=+'+	end=+'+
"syn match  pascalIdentifier		"\<[a-zA-Z_][a-zA-Z0-9_]*\>"
syn match  pascalDelimiter		"[()]"
syn match  pascalMatrixDelimiter	"[][]"

"if you prefer you can highlight the range
"syn match  pascalMatrixDelimiter	"[\d\+\.\.\d\+]"

"syn match  pascalNumber		"-\=\<\d\+\.\d\+[dD]-\=\d\+\>"
syn match  pascalNumber		"-\=\<\d\+\.\d\+[eE]-\=\d\+\>"
"syn match  pascalNumber		"-\=\<\d\+\.\d\+\>"
"syn match  pascalNumber			"-\=\<\d\+\>"
syn match  pascalByte		"\$[0-9a-fA-F]\+\>"

syn match pascalShowTab 	"\t"
syn match pascalShowTabc 	"\t"

syn region pascalComment	start="(\*"	end="\*)"	contains=pascalTodo
syn region pascalComment	start="{"	end="}"		contains=pascalTodo
syn region pascalComment	start="//"	end="$"		contains=pascalTodo
syn region pascalDefine		start="{\$" end="}"

syn match pascalAssignment	":="

syn keyword pascalConstant	false maxint nil true

syn keyword pascalConditional	case else if of on then

syn keyword pascalRepeat	do downto for repeat to until while

syn keyword pascalComparator	in is
syn match pascalComparator 	"<"
syn match pascalComparator 	"="
syn match pascalComparator 	">"

syn keyword pascalOperator	and as div mod not of or shl shr with xor

syn keyword pascalType          integer char real boolean text

syn keyword pascalKeyword       array file input output set
syn keyword pascalKeyword       program procedure function
syn keyword pascalKeyword	record type var const label
syn keyword pascalKeyword       begin end goto

" --- stanrdard functions and procedures ---

" file
syn keyword pascalFunction      put get reset rewrite page
syn keyword pascalFunction      read readln write writeln

" dynamic allocation
syn keyword pascalFunction      new dispose

" data transfer functions
syn keyword pascalFunction      pack unpack

" arithmetic functions
syn keyword pascalFunction      abs sqr sin cos 
syn keyword pascalFunction      arctan exp ln sqrt

" predicates (booleans)
syn keyword pascalFunction      odd eoln eof

" transfer functions
syn keyword pascalFunction      trunc round ord chr

" additional standard functions
syn keyword pascalFunction      succ pred

" not sure seek is in the standard
syn keyword pascalFunction      seek

" common extensions worthy of inclusion
syn keyword pascalFunction      halt


" ------------------------------------------------------------------------------

hi def link pascalAccess       		Statement
hi def link pascalAssignment 	        Operator
hi def link pascalByte			Number
hi def link pascalComment      		Comment
hi def link pascalComparator	        Conditional
hi def link pascalConditional		Conditional
hi def link pascalConstant 	        Statement
hi def link pascalDelimiter		Identifier
hi def link pascalDefine		Macro
hi def link pascalException		Exception
hi def link pascalFuncModifier		Type
hi def link pascalFunction		Function
hi def link pascalIdentifier		Identifier
hi def link pascalLabel			Label
hi def link pascalMatrixDelimiter      	Identifier
hi def link pascalModifier		Type
hi def link pascalNumber	       	Number
hi def link pascalObjModifier		Type
hi def link pascalObject	       	Type
hi def link pascalOperator		Operator
hi def link pascalRepeat	       	Repeat
hi def link pascalShowTab		Error
hi def link pascalShowTabc		Error
hi def link pascalKeyword		Keyword
hi def link pascalString	       	String
hi def link pascalStructure		Structure
hi def link pascalTodo			Todo
hi def link pascalType			Type
hi def link pascalUnclassified		Statement

" ------------------------------------------------------------------------------

let b:current_syntax = "isopascal"

" vim: ts=8 sw=2
