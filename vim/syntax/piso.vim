if exists("b:current_syntax") | finish | endif

syntax keyword PisoKeyword unit let iso in fix type rec of fun case match with begin end char
syntax match PisoSymbol "\((\|)\|\[\|\]\|\.\|\*\|+\||>\||\|,\|;\|::\|\->\|<\->\|=\)"
syntax match PisoNat "\<\d\+\>"
syntax match PisoChar "'.'"
syntax region PisoString start="\"" end="\""
syntax match PisoCtor "\<[A-Z][a-zA-Z0-9]*\>"
syntax region PisoComment start="(\*" end="\*)"

hi def link PisoKeyword @keyword
hi def link PisoSymbol @punctuation
hi def link PisoNat @constant
hi def link PisoChar @constant
hi def link PisoString @constant
hi def link PisoCtor @constant
hi def link PisoComment @comment

let b:current_syntax = "piso"
