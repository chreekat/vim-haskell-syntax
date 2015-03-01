" Programmatic colors for testing

let s:counter = 0

func! s:getHigh(name)
    exec "hi hs" . a:name . " ctermfg=" . s:counter
    let s:counter = s:counter + 1
endfunc

for name in
    \ [ "Module"
    \ , "TopLevelName"
    \ , "TopLevelArg"
    \ , "TopLevelExpr"
    \ , "NestedName"
    \ , "NestedArg"
    \ ]
    call s:getHigh(name)
endfor
