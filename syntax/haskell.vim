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
" The top-most, outer region will have to jump over 'let' and 'where'. That
" is accomplished by making them keywords.
syn keyword hsDeclKeyword let where
" This is analogous to "ccFoobar" in the nextgroup example. It matches
" everything from the beginning of the name being declared to the ' =' at
" the end.
syn match hsNestedNameArg /\l\w*.\{-} =[[:punct:]]\@!/ contains=hsNestedName
" This is "ccFoo" in the example. It matches just the head of the list, and
" links to the tail.
syn match hsNestedName /\l\w*/ contained nextgroup=hsNestedArgRec
" This is where we deviate from the example. Rather than ending at the next
" "Foo", we continue to the " =" at the end of the original match. In other
" words, we match the whole tail.
"
" NOTE: Probably a bug: If 'oneline' is removed, the following group goes active
" even at the top level (i.e. not contained in any group), in spite of
" being marked 'contained'.
syn region hsNestedArgRec start=/\s/ end=/ =[[:punct:]]\@!/
    \ oneline contained contains=hsNestedArg
" Now we match the 'head' arg inside of ArgRec, then recurse to the next
" ArgRec. Think (head (tail ...))
syn match hsNestedArg /\<\l\w*/ contained nextgroup=hsNextedArgRec

" Top-level declaration {{{1
" ---------------------
" A region that starts at the beginning of a line and ends at the first
" single '='.
syn match  hsTopLevelDecl /^\l.\{-} =[[:punct:]]\@!/ display
    \ contains=hsTopLevelName,hsTopLevelArg
" Order matters here: \l\w* matches both args and names, so args must be
" listed first to have lower precedence.
syn match  hsTopLevelArg /\<\l\w*/ display contained
syn match  hsTopLevelName /^\l\w*/ display contained

" Top-level infix declaration {{{1
" ---------------------------
"
" The first has parentheses and punctuation at the start.
syn match hsTopLevelDeclParen /([[:punct:]]\+).* =[[:punct:]]\@!/ display
    \ contains=hsTopLevelName,hsTopLevelArg
syn match hsTopLevelName /([[:punct:]]\+)/ display contained
" The second has infix punctuation. Any punctuation operator has at least
" two chars, I think.
syn match hsTopLevelDeclInfixPunct
    \ /^\l.*[[:punct:]]\{2,}.*\ze =[[:punct:]]\@!/
    \ display contains=hsTopLevelArg,hsTopLevelPunctName
" This is a hacky way to match [[:punct:]] and highlight it as
" hsTopLevelName without changing hsTopLevelName's definition.
syn region hsTopLevelPunctName matchgroup=hsTopLevelName
    \ start=/[[:punct:]]/ end=/[[:punct:]]*/ oneline display contained
" The third has infix backticks. Similar to the above, but ` is a lot
" easier to write than [[:punct:]], and there's no trick false positives
" caused by the '=' at the end of the declaration.
syn match hsTopLevelDeclInfixBacktick /^.*`\&^\l\w*.* =/ display
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
