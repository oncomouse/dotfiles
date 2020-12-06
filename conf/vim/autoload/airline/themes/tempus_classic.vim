" MIT License. Copyright (c) 2013-2020 Bailey Ling et al.
" vim: et ts=2 sts=2 sw=2 tw=80

scriptencoding utf-8

" Airline themes are generated based on the following concepts:
"   * The section of the status line, valid Airline statusline sections are:
"       * airline_a (left most section)
"       * airline_b (section just to the right of airline_a)
"       * airline_c (section just to the right of airline_b)
"       * airline_x (first section of the right most sections)
"       * airline_y (section just to the right of airline_x)
"       * airline_z (right most section)
"   * The mode of the buffer, as reported by the :mode() function.  Airline
"     converts the values reported by mode() to the following:
"       * normal
"       * insert
"       * replace
"       * visual
"       * inactive
"       * terminal
"       The last one is actually no real mode as returned by mode(), but used by
"       airline to style inactive statuslines (e.g. windows, where the cursor
"       currently does not reside in).
"   * In addition to each section and mode specified above, airline themes
"     can also specify overrides.  Overrides can be provided for the following
"     scenarios:
"       * 'modified'
"       * 'paste'
"
" Airline themes are specified as a global viml dictionary using the above
" sections, modes and overrides as keys to the dictionary.  The name of the
" dictionary is significant and should be specified as:
"   * g:airline#themes#<theme_name>#palette
" where <theme_name> is substituted for the name of the theme.vim file where the
" theme definition resides.  Airline themes should reside somewhere on the
" 'runtimepath' where it will be loaded at vim startup, for example:
"   * autoload/airline/themes/theme_name.vim
"
" For this, the dark.vim, theme, this is defined as
let g:airline#themes#tempus_classic#palette = {}

" Keys in the dictionary are composed of the mode, and if specified the
" override.  For example:
"   * g:airline#themes#tempus_classic#palette.normal
"       * the colors for a statusline while in normal mode
"   * g:airline#themes#tempus_classic#palette.normal_modified
"       * the colors for a statusline while in normal mode when the buffer has
"         been modified
"   * g:airline#themes#tempus_classic#palette.visual
"       * the colors for a statusline while in visual mode
"
" Values for each dictionary key is an array of color values that should be
" familiar for colorscheme designers:
"   * [guifg, guibg, ctermfg, ctermbg, opts]
" See "help attr-list" for valid values for the "opt" value.
"
" Each theme must provide an array of such values for each airline section of
" the statusline (airline_a through airline_z).  A convenience function,
" airline#themes#generate_color_map() exists to mirror airline_a/b/c to
" airline_x/y/z, respectively.

" The tempus_classic theme:
let s:kitty_colors = ['#232323','#d2813d','#8c9e3d','#b1942b','#6e9cb0','#b58d88','#6da280','#949d9f','#312e30','#d0913d','#96a42d','#a8a030','#8e9cc0','#d58888','#7aa880','#aeadaf'] 
let s:airline_a_normal   = [ '#312e30' , '#6e9cb0' , 8  , 4   ]
let s:airline_b_normal   = [ '#aeadaf' , '#4a4a4b' , 0  , 238 ]
let s:airline_c_normal   = [ '#949d9f' , '#3a3a3a' , 15 , 237 ]
let g:airline#themes#tempus_classic#palette.normal = airline#themes#generate_color_map(s:airline_a_normal, s:airline_b_normal, s:airline_c_normal)

" It should be noted the above is equivalent to:
" let g:airline#themes#tempus_classic#palette.normal = airline#themes#generate_color_map(
"    \  [ '#00005f' , '#dfff00' , 17  , 190 ],  " section airline_a
"    \  [ '#ffffff' , '#444444' , 255 , 238 ],  " section airline_b
"    \  [ '#9cffd3' , '#202020' , 85  , 234 ]   " section airline_c
"    \)
"
" In turn, that is equivalent to:
" let g:airline#themes#tempus_classic#palette.normal = {
"    \  'airline_a': [ '#00005f' , '#dfff00' , 17  , 190 ],  "section airline_a
"    \  'airline_b': [ '#ffffff' , '#444444' , 255 , 238 ],  "section airline_b
"    \  'airline_c': [ '#9cffd3' , '#202020' , 85  , 234 ],  "section airline_c
"    \  'airline_x': [ '#9cffd3' , '#202020' , 85  , 234 ],  "section airline_x
"    \  'airline_y': [ '#ffffff' , '#444444' , 255 , 238 ],  "section airline_y
"    \  'airline_z': [ '#00005f' , '#dfff00' , 17  , 190 ]   "section airline_z
"    \}
"
" airline#themes#generate_color_map() also uses the values provided as
" parameters to create intermediary groups such as:
"   airline_a_to_airline_b
"   airline_b_to_airline_c
"   etc...

" Here we define overrides for when the buffer is modified.  This will be
" applied after g:airline#themes#tempus_classic#palette.normal, hence why only certain keys are
" declared.
"       \ 'airline_c': [ '#ffffff' , '#5f005f' , 255     , 53      , ''     ] ,

let g:airline#themes#tempus_classic#palette.normal_modified = {
      \ 'airline_c': g:airline#themes#tempus_classic#palette.normal['airline_c']
      \ }

let s:airline_a_insert = [ s:airline_a_normal[0] , '#96a42d' , s:airline_a_normal[2]  , 2  ]
let s:airline_b_insert = s:airline_b_normal
let s:airline_c_insert = s:airline_c_normal
let g:airline#themes#tempus_classic#palette.insert = airline#themes#generate_color_map(s:airline_a_insert, s:airline_b_insert, s:airline_c_insert)
let g:airline#themes#tempus_classic#palette.insert_modified = g:airline#themes#tempus_classic#palette.normal_modified
let g:airline#themes#tempus_classic#palette.insert_paste = {
      \ 'airline_a': [ s:airline_a_insert[0]   , '#d0913d' , s:airline_a_insert[2] , 9     , ''     ] ,
      \ }

let s:airline_a_replace = [ s:airline_a_normal[0]   , '#d58888' , s:airline_a_normal[2] , 13     , ''     ]
let s:airline_b_replace = s:airline_b_normal
let s:airline_c_replace = s:airline_c_normal
let g:airline#themes#tempus_classic#palette.replace = airline#themes#generate_color_map(s:airline_a_replace, s:airline_b_replace, s:airline_c_replace)
let g:airline#themes#tempus_classic#palette.replace_modified = g:airline#themes#tempus_classic#palette.normal_modified


let s:airline_a_visual = [ s:airline_a_normal[0] , '#d2813d' , s:airline_a_normal[2] , 1 ]
let s:airline_b_visual = s:airline_b_normal
let s:airline_c_visual = s:airline_c_normal
let g:airline#themes#tempus_classic#palette.visual = airline#themes#generate_color_map(s:airline_a_visual, s:airline_b_visual, s:airline_c_visual)
let g:airline#themes#tempus_classic#palette.visual_modified = g:airline#themes#tempus_classic#palette.normal_modified

let s:airline_a_inactive = [ '#4e4e4e' , '#1c1c1c' , 239 , 234 , '' ]
let s:airline_b_inactive = [ '#4e4e4e' , '#262626' , 239 , 235 , '' ]
let s:airline_c_inactive = [ '#4e4e4e' , '#303030' , 239 , 236 , '' ]
let g:airline#themes#tempus_classic#palette.inactive = airline#themes#generate_color_map(s:airline_a_inactive, s:airline_b_inactive, s:airline_c_inactive)
let g:airline#themes#tempus_classic#palette.inactive_modified = g:airline#themes#tempus_classic#palette.normal_modified

" For commandline mode, we use the colors from normal mode, except the mode
" indicator should be colored differently, e.g. light green
let s:airline_a_commandline = [ s:airline_a_normal[0] , '#6da280' , s:airline_a_normal[2]  , 6  ]
let s:airline_b_commandline = s:airline_b_normal
let s:airline_c_commandline = s:airline_c_normal
let g:airline#themes#tempus_classic#palette.commandline = airline#themes#generate_color_map(s:airline_a_commandline, s:airline_b_commandline, s:airline_c_commandline)
" Terminal
let s:airline_a_terminal = [ s:airline_a_normal[0] , '#C594C5' , s:airline_a_normal[2]  , 182  ]
let s:airline_b_terminal = s:airline_b_normal
let s:airline_c_terminal = s:airline_c_normal
let g:airline#themes#tempus_classic#palette.terminal = airline#themes#generate_color_map(s:airline_a_terminal, s:airline_b_insert, s:airline_c_insert)


" Accents are used to give parts within a section a slightly different look or
" color. Here we are defining a "red" accent, which is used by the 'readonly'
" part by default. Only the foreground colors are specified, so the background
" colors are automatically extracted from the underlying section colors. What
" this means is that regardless of which section the part is defined in, it
" will be red instead of the section's foreground color. You can also have
" multiple parts with accents within a section.
let g:airline#themes#tempus_classic#palette.accents = {
      \ 'red': [ '#d58888' , '' , 13 , ''  ]
      \ }
" Customize error and warning highlights:
let s:tempus_error_warning = {
      \ 'airline_error': ['#312e30', '#d2813d', 8, 1],
      \ 'airline_warning': ['#312e30', '#b1942b', 8, 3],
      \ 'airline_term': s:airline_c_normal,
      \ }
let s:airline_modes = [
      \'normal',
      \'normal_modified',
      \'insert',
      \'insert_modified',
      \'replace',
      \'replace_modified',
      \'commandline',
      \'terminal'
      \]
for m in s:airline_modes
  call extend(g:airline#themes#tempus_classic#palette[m], s:tempus_error_warning)
endfor
