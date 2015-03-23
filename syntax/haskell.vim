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
" 2. The '=' of any let, where, or top-level binding must be surrounded by
"    whitespace. (List comprehensions are specifically excluded; but then,
"    I don't have highlighting plans for list comprehensions just yet.)
"    YES: foo a = ...
"     NO: foo a= ...
"     NO: foo=bar

if exists("b:current_syntax")
    finish
endif
syntax case match

" Language for defining these syntax items
" ----------------------------------------
"
" TODO: Catch Unicode symbols and punctuation. In Lexeme.hs in ghc, this
" corresponds to having Pc, Pd, Po, Sm, Sc, Sk, or So in column 3 of
" http://www.unicode.org/Public/UNIDATA/UnicodeData.txt.
"
" For now, I'll use these ASCII definitions, since vim supports it best.
let symList = '-!#$%&*+./<=>?@\\\^|~:'
let varSym = '[' . symList . ']'
let notVarSym = '[^' . symList . ']'
" No ':' for the start symbol for vars.
let varSymStart = substitute(varSym, ':', '', '')
let op = varSymStart . varSym . '*'
" op sans one of the reserved ops: '='. This matches any single symbol
" except '=', or it matches '=S' for some symbol S.
let notEqOp =
    \ '\%(=' . varSym . '\|' . substitute(varSymStart, '=', '', '') . '\)'

let tld_end = ' =\S\@!'

func! s:match(name, match, args)
    exec "syn match " . a:name . ' ' . a:match . ' display ' . a:args
endfunc

func! s:region(name, mg_start, start, end, args)
    let l:mg_start = ""
    if strlen(a:mg_start) > 0
        let l:mg_start = "matchgroup=" . a:mg_start
    endif
    exec "syn region " . a:name
        \ . ' ' . l:mg_start . ' start=' . a:start
        \ . ' end=' . a:end
        \ . ' display ' . a:args
endfunc

" Nested declaration {{{1
" ------------------
" This takes something like "let x a = ..." and gives the following syntax
" highlights: x : hsNestedName, a : hsNestedArg.
"
" NOTE: This is very fragile and took a couple days to figure out!
"
" Cribbing off of the example given in nextgroup documentation, here is how
" it works. The example is a two-step, going from Foo to Bar to Foo, But
" what I want a right-recursive nesting of Args (Bars) following the
" initial Name (Foo).
"
" To be right-recursive, each match or region will always end at ' ='.
"
" The top-most, outer region will have to jump over 'let' and 'where'. Two
" varieties of that: jump over any amount of crap followed by let and
" where, or start at the beginning of a (non-top-level) line that has NO
" let or where.
syn match hsNestedDecl
    \ /\%(let\|where\)\s\+\zs.\{-} =[[:punct:]]\@!/
    \ display contains=hsNestedNameArg
syn match hsNestedDeclCont
    \ /^\%(.*\%(let\|where\)\)\@!\s.* =[[:punct:]]\@!/ 
    \ display contains=hsNestedNameArg
" This is analogous to "ccFoobar" in the nextgroup example. It matches
" everything from the beginning of the name being declared to the ' =' at
" the end.
syn match hsNestedNameArg
    \ /\l\w*.\{-} =[[:punct:]]\@!/ display contained contains=hsNestedName
" This is "ccFoo" in the example. It matches just the head of the list, and
" links to the tail.
syn match hsNestedName /\l\w*/ display contained nextgroup=hsNestedArgRec
" This is where we deviate from the example. Rather than ending at the next
" "Foo", we continue to the " =" at the end of the original match. In other
" words, we match the whole tail.
"
" NOTE: Probably a bug: If 'oneline' is removed, the following group goes active
" even at the top level (i.e. not contained in any group), in spite of
" being marked 'contained'.
syn region hsNestedArgRec start=/\s/ end=/ =[[:punct:]]\@!/
    \ oneline contained contains=hsNestedArg display
" Now we match the 'head' arg inside of ArgRec, then recurse to the next
" ArgRec. Think (head (tail ...))
syn match hsNestedArg /\<\l\w*/ display contained nextgroup=hsNextedArgRec

" Top-level declarations {{{1
" ---------------------

" This catch-all region may be overkill, but it makes me sleep better
" knowing that all the top-level matches below are contained within it. It
" doesn't really help much, since an inner region can easily escape!
call s:match('hsTopLevelReg',
    \ '/^\S.\{-}' . tld_end .'/',
    \ 'contains=hsTopLevelDecl,hsTopLevelDeclInfixPunct')

" A region that starts at the beginning of a line and ends at the first
" single '='. {{{2
call s:match('hsTopLevelDecl',
    \ '/^\l.\{-}' . tld_end . '/',
    \ 'contained contains=hsTopLevelName,hsTopLevelArg')
" Order matters here: \l\w* matches both args and names, so args must be
" listed first to have lower precedence.
syn match hsTopLevelArg /\<\l\w*/ display contained
syn match hsTopLevelName /^\l\w*/ display contained

" Top-level operator declaration {{{2
" ---------------------------
"
" Infix punctuation.
call s:match('hsTopLevelDeclInfixPunct',
    \ '/^.\{-}' . notEqOp . '.\{-}\ze' . tld_end . '/',
    \ 'contained contains=hsTopLevelArg,hsTopLevelPunctName')
" This is a hacky way to match [[:punct:]] and highlight it as
" hsTopLevelName without changing hsTopLevelName's definition.
call s:region('hsTopLevelPunctName',
    \ 'hsTopLevelName', '/' . varSymStart . '/',
    \ '/' . varSym . '*/',
    \ 'oneline contained')
" Backtick operator. Similar to the above, but ` is a lot easier to write
" than [[:punct:]], and there's no trick false positives caused by the '='
" at the end of the declaration. {{{2
syn match hsTopLevelDeclInfixBacktick /^\k.\{-}`.\{-} =[[:punct:]]\@!\&^\k*.\{-} =[[:punct:]]\@!/ display
    \ contains=hsTopLevelInfixName,hsTopLevelArg
syn region hsTopLevelInfixName start=/`/ end=/`/ display contained oneline
    \ contains=hsTopLevelName
syn match  hsTopLevelName /`\zs\l\w*/ display contained

" Module name {{{1
" -----------
"
" Need a region. I thought this would work, but it doesn't:
"     syn match hsModule /^module\_s*\zs\u\w*.../
syn region hsModuleRegion
    \ start=/^module\>/ end=/\<where\>/
    \ contains=hsModule,hsExports display
syn match hsModule /\u\w*\%(\.\u\w*\)*/ contained display
syn region hsExports
    \ start=/(/ skip=/([[:punct:]]*)/ end=/)/ contained display

" }}}1

" TODO: Relax this requirement.
syn sync fromstart

let b:current_syntax = "haskell"
" vim: fdm=marker :
