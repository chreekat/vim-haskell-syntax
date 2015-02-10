" Vim syntax file
" Language: Haskell
" Maintainer: Bryan Richter <bryan.richter@gmail.com>
" Last Change: 2015 Feb 01
" Remark: A focus on semantic elements

" Haskell syntax is tough to tease out with vim's regexes. I'm going to
" make some opinionated assumptions. Hopefully nothing shocking. Here's a
" list:
"
" 1. The LHS of a declaration, up to the '=', is all on the same line.
"    YES: foo a b = ...
"     NO: foo
"             a
"             b = ...
"     NO: foo a b
"             = ...
"
"    This rule applies to both top level declarations and let/while
"    declarations.
"
" 2. The '=' of any let, where, or top-level binding must be surrounded by whitespace.
"    YES: foo a = ...
"     NO: foo a= ...
"     NO: foo=bar

if exists("b:current_syntax")
    finish
endif
syntax case match

" Module name:
"
" A dotted name (starts uppercase) that follows the "module" keyword.
syn keyword hsModuleKeyword module nextgroup=hsModule skipwhite skipnl
syn match   hsModule /\u\w*\%(\.\u\w*\)*/ display contained

" Top-level declaration:
"
" A region that starts at the beginning of a line and ends at a single '='
syn match  hsTopLevelDecl /^\l\w*\_.\{-}=/
    \ contains=hsTopLevelName,hsTopLevelArg
" Order matters here
syn match  hsTopLevelArg /\l\w*/ display contained
syn match  hsTopLevelName /^\l\w*/ display contained

" Top-level expression:
"
" A region that starts at the beginning of a line and has no '=' anywhere
syn match  hsTopLevelExpr /^\S[^=]*\ze\(\n\|\n\s[^=]*\)*\(\n\S\|\%$\)/

let b:current_syntax = "haskell"
