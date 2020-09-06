" Match functions for VimL (source https://machakann.hatenablog.com/entry/2019/06/09/180737)
let b:sandwich_magicchar_f_patterns = [
        \   {
        \     'header' : '\C\<\%(\h\|[sa]:\h\|g:[A-Z]\)\k*',
        \     'bra'    : '(',
        \     'ket'    : ')',
        \     'footer' : '',
        \   },
        \ ]
