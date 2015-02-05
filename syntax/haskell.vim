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

" Top-level binding:
"
" A name (starts lowercase) that precedes a single '='
syn match   hsTopLevel /^\l\w*\ze\_s\{-}=/ display

let b:current_syntax = "haskell"
