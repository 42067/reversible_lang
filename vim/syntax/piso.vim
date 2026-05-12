if exists("b:current_syntax") | finish | endif

syntax keyword PisoKeyword unit let iso in fix type rec of fun case match with begin end
syntax match PisoSymbol "\((\|)\|\[\|\]\|\.\|\*\|+\||>\||\|,\|;\|::\|\->\|<\->\|=\)"
syntax match PisoNat "\<\d\+\>"
syntax match PisoCtor "\<[A-Z][a-zA-Z0-9]*\>"
syntax match PisoComment "(\*\(\_[^\*]\|\*\_[^)]\)\{-}\*)"

hi def link PisoKeyword @keyword
hi def link PisoSymbol @punctuation
hi def link PisoNat @constant
hi def link PisoCtor @constant
hi def link PisoComment @comment

let b:current_syntax = "piso"
