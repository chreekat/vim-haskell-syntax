" Vim syntax file
" Language: Haskell
" Maintainer: Bryan Richter <bryan.richter@gmail.com>
" Last Change: 2015 Feb 01
" Remark: A focus on semantic elements

if exists("b:current_syntax")
    finish
endif
syntax case match

" Module name
syn keyword hsModuleKeyword module nextgroup=hsModule skipwhite skipnl
syn match   hsModule /\u\w*/ display contained

" Top-level binding
syn match   hsTopLevel /^\U\w*\ze\s*=/

let b:current_syntax = "haskell"
