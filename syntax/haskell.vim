" Vim syntax file
" Language: Haskell
" Maintainer: Bryan Richter <bryan.richter@gmail.com>
" Last Change: 2015 Feb 01
" Remark: A focus on semantic elements

if exists("b:current_syntax")
    finish
endif
syntax case match

" Module name:
"
" A name (starts uppercase) that follows the "module" keyword.
syn keyword hsModuleKeyword module nextgroup=hsModule skipwhite skipnl
syn match   hsModule /\u\w*/ display contained

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
