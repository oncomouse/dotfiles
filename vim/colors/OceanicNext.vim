" ===============================================================
" Adapted to only use cterm colors from:
"   https://github.com/mhartington/oceanic-next/
" ===============================================================
" {{{ Setup
  set background=dark
  hi clear
  if exists('syntax_on')
    syntax reset
  endif
  let g:colors_name='OceanicNext'
" }}}
" Default Colors {{{
  if &termguicolors == 1
    " Use the fish function kitty-color to change this when you update the
    " terminal:
    let s:kitty_colors = ['#1B2B34','#EC5F67','#99C794','#FAC863','#6699CC','#C594C5','#5FB3B3','#C0C5CE','#65737E','#F99157','#343D46','#4F5B66','#A7ADBA','#CDD3DE','#AB7967','#D8DEE9'] 
    let s:cterms = map(s:kitty_colors, {i,color -> [i, color]})
  else
    let s:cterms = map(range(0,15), {i -> [i]})
  endif

  let s:ctermNONE = ['NONE', 'NONE']
" }}}
" Set Terminal Colors {{{
  if has('nvim') && &termguicolors == 1
    let g:terminal_color_0=s:cterms[00][1]
    let g:terminal_color_1=s:cterms[01][1]
    let g:terminal_color_2=s:cterms[02][1]
    let g:terminal_color_3=s:cterms[03][1]
    let g:terminal_color_4=s:cterms[04][1]
    let g:terminal_color_5=s:cterms[05][1]
    let g:terminal_color_6=s:cterms[06][1]
    let g:terminal_color_7=s:cterms[07][1]
    let g:terminal_color_8=s:cterms[00][1]
    let g:terminal_color_9=s:cterms[01][1]
    let g:terminal_color_10=s:cterms[02][1]
    let g:terminal_color_11=s:cterms[03][1]
    let g:terminal_color_12=s:cterms[04][1]
    let g:terminal_color_13=s:cterms[05][1]
    let g:terminal_color_14=s:cterms[06][1]
    let g:terminal_color_15=s:cterms[07][1]
    let g:terminal_color_16=s:cterms[09][1]
    let g:terminal_color_17=s:cterms[14][1]
    let g:terminal_color_foreground=s:cterms[07][1]
    let g:terminal_color_background=s:cterms[00][1]
  elseif exists('*term_setansicolors') && &termguicolors == 1
    let g:terminal_ansi_colors = repeat([0], 16)
    let g:terminal_ansi_colors[0]=s:cterms[00][1]
    let g:terminal_ansi_colors[1]=s:cterms[01][1]
    let g:terminal_ansi_colors[2]=s:cterms[02][1]
    let g:terminal_ansi_colors[3]=s:cterms[03][1]
    let g:terminal_ansi_colors[4]=s:cterms[04][1]
    let g:terminal_ansi_colors[5]=s:cterms[05][1]
    let g:terminal_ansi_colors[6]=s:cterms[06][1]
    let g:terminal_ansi_colors[7]=s:cterms[07][1]
    let g:terminal_ansi_colors[8]=s:cterms[00][1]
    let g:terminal_ansi_colors[9]=s:cterms[09][1]
    let g:terminal_ansi_colors[10]=s:cterms[10][1]
    let g:terminal_ansi_colors[11]=s:cterms[11][1]
    let g:terminal_ansi_colors[12]=s:cterms[12][1]
    let g:terminal_ansi_colors[13]=s:cterms[13][1]
    let g:terminal_ansi_colors[14]=s:cterms[14][1]
    let g:terminal_ansi_colors[15]=s:cterms[15][1]
  endif
" }}}
" Color Function {{{
  function! <sid>hi(group, fg, bg, attr)
    " fg, bg, attr, attrsp
    if !empty(a:fg)
      exec 'hi ' . a:group . ' ctermfg=' . a:fg[0]
      if &termguicolors == 1
        exec 'hi ' . a:group . ' guifg=' .  a:fg[1]
      endif
    endif
    if !empty(a:bg)
      exec 'hi ' . a:group . ' ctermbg=' . a:bg[0]
      if &termguicolors == 1
        exec 'hi ' . a:group . ' guibg=' .  a:bg[1]
      endif
    endif
    if a:attr !=# ''
      exec 'hi ' . a:group . ' cterm=' . a:attr
      if &termguicolors == 1
        exec 'hi ' . a:group . ' gui=' .   a:attr
      endif
    endif
  endfunction
" }}}
" Statusline {{{
  call <sid>hi('VisualMode',s:cterms[10],s:cterms[09],'bold')
  call <sid>hi('InsertMode',s:cterms[10],s:cterms[02],'bold')
  call <sid>hi('ReplaceMode',s:cterms[10],s:cterms[01],'bold')
  call <sid>hi('CommandMode',s:cterms[10],s:cterms[05],'bold')
  call <sid>hi('NormalMode',s:cterms[10],s:cterms[04],'bold')
  call <sid>hi('StatusLine',s:cterms[10],s:ctermNONE,'NONE')
  call <sid>hi('StatusLineNC', s:cterms[11],s:cterms[10], 'NONE')
  call <sid>hi('StatusLineTerm',s:cterms[10],s:cterms[02],'NONE')
  call <sid>hi('StatusLineTermNC', s:cterms[11],s:cterms[10], 'NONE')
  call <sid>hi('User1', '', s:cterms[08], 'bold')
  call <sid>hi('User2', '', s:cterms[08], 'NONE')
  call <sid>hi('User3', s:cterms[10], s:cterms[03], 'NONE')
  call <sid>hi('User4', s:cterms[10], s:cterms[01], 'NONE')
  call <sid>hi('User5', s:cterms[15], s:cterms[08], 'none')
  call <sid>hi('User6', s:cterms[08], s:cterms[00], 'NONE')
  call <sid>hi('TabLineFill', s:cterms[11], s:ctermNONE, 'NONE')
  call <sid>hi('TabLineSel',s:cterms[13],s:cterms[11],'bold')
  call <sid>hi('TabLine', s:cterms[08], s:cterms[10], '')
" }}}
" Basics {{{
call <sid>hi('Bold','','','bold')
call <sid>hi('Debug',s:cterms[01],'','')
call <sid>hi('Directory',s:cterms[04],'','')
call <sid>hi('ErrorMsg',s:cterms[01],s:ctermNONE,'')
call <sid>hi('Error', s:cterms[15],s:cterms[09],'')
call <sid>hi('Exception',s:cterms[01],'','')
call <sid>hi('FoldColumn',s:cterms[04],s:ctermNONE,'')
call <sid>hi('Folded',s:cterms[08],s:cterms[10],'italic')
call <sid>hi('IncSearch',s:cterms[10],s:cterms[09],'NONE')
call <sid>hi('Italic','','','italic')

call <sid>hi('Macro',s:cterms[01],'','')
call <sid>hi('MatchParen',s:cterms[07],s:cterms[08],'')
call <sid>hi('ModeMsg',s:cterms[02],'','')
call <sid>hi('MoreMsg',s:cterms[02],'','')
call <sid>hi('Question',s:cterms[04],'','')
call <sid>hi('Search',s:cterms[08],s:cterms[03],'')
call <sid>hi('SpecialKey',s:cterms[08],'','')
call <sid>hi('TooLong',s:cterms[01],'','')
call <sid>hi('Underlined',s:cterms[01],'','')
call <sid>hi('Visual','',s:cterms[11],'')
call <sid>hi('VisualNOS',s:cterms[01],'','')
call <sid>hi('WarningMsg',s:cterms[01],'','')
call <sid>hi('WildMenu',s:cterms[15],s:cterms[04],'')
call <sid>hi('Title',s:cterms[04],'','')
call <sid>hi('Conceal',s:cterms[04],s:ctermNONE,'')
call <sid>hi('Cursor',s:cterms[00],s:cterms[07],'')
call <sid>hi('NonText',s:cterms[08],'','')
call <sid>hi('Normal',s:cterms[07],s:ctermNONE,'')
call <sid>hi('EndOfBuffer',s:cterms[07],s:ctermNONE,'')
call <sid>hi('LineNr',s:cterms[08],s:ctermNONE,'')
call <sid>hi('SignColumn',s:cterms[00],s:ctermNONE,'')
call <sid>hi('VertSplit',s:cterms[00],s:cterms[11],'')
call <sid>hi('ColorColumn','',s:cterms[10],'')
call <sid>hi('CursorColumn','',s:cterms[11],'')
call <sid>hi('CursorLine','',s:cterms[10],'NONE')
call <sid>hi('CursorLineNR',s:cterms[00],s:ctermNONE,'')
call <sid>hi('CursorLineNr',s:cterms[08],s:cterms[10],'')
call <sid>hi('PMenu',s:cterms[12],s:cterms[10],'')
call <sid>hi('PMenuSel',s:cterms[15],s:cterms[04],'')
call <sid>hi('PmenuSbar','',s:cterms[11],'')
call <sid>hi('PmenuThumb','',s:cterms[07],'')
call <sid>hi('helpExample',s:cterms[03],'','')
call <sid>hi('helpCommand',s:cterms[03],'','')
call <sid>hi('ToolbarButton',s:cterms[00], s:cterms[07], '')
" }}}
" Syntax Highlighting {{{
call <sid>hi('Boolean',s:cterms[09],'','')
call <sid>hi('Character',s:cterms[01],'','')
call <sid>hi('Comment',s:cterms[08],'','italic')
call <sid>hi('Conditional',s:cterms[05],'','')
call <sid>hi('Constant',s:cterms[09],'','')
call <sid>hi('Define',s:cterms[05],'','')
call <sid>hi('Delimiter',s:cterms[14],'','')
call <sid>hi('Float',s:cterms[09],'','')
call <sid>hi('Function',s:cterms[04],'','')
call <sid>hi('Identifier',s:cterms[06],'','bold')
call <sid>hi('Include',s:cterms[04],'','')
call <sid>hi('Keyword',s:cterms[05],'','')
call <sid>hi('Label',s:cterms[03],'','')
call <sid>hi('Number',s:cterms[09],'','')
call <sid>hi('Operator',s:cterms[07],'','')
call <sid>hi('PreProc',s:cterms[03],'','')
call <sid>hi('Repeat',s:cterms[03],'','')
call <sid>hi('Special',s:cterms[06],'','')
call <sid>hi('SpecialChar',s:cterms[14],'','')
call <sid>hi('Statement',s:cterms[01],'','NONE')
call <sid>hi('StorageClass',s:cterms[03],'','')
call <sid>hi('String',s:cterms[02],'','')
call <sid>hi('Structure',s:cterms[05],'','')
call <sid>hi('Tag',s:cterms[03],'','')
call <sid>hi('Todo',s:cterms[03],s:cterms[10],'')
call <sid>hi('Type',s:cterms[03],'','')
call <sid>hi('Typedef',s:cterms[03],'','')
call <sid>hi('Noise',s:cterms[06],'','')
" }}}
" Spelling & Diffing {{{
call <sid>hi('SpellBad','',s:cterms[11],'undercurl')
call <sid>hi('SpellLocal','','','undercurl')
call <sid>hi('SpellCap','',s:cterms[10],'undercurl')
call <sid>hi('SpellRare','','','undercurl')

call <sid>hi('DiffAdd',s:cterms[02],s:cterms[10],'bold')
call <sid>hi('DiffChange',s:cterms[08],s:cterms[10],'')
call <sid>hi('DiffDelete',s:cterms[01],s:cterms[10],'')
call <sid>hi('DiffText',s:cterms[04],s:cterms[10],'')
call <sid>hi('DiffAdded',s:cterms[15],s:cterms[02],'bold')
call <sid>hi('DiffFile',s:cterms[01],s:ctermNONE,'')
call <sid>hi('DiffNewFile',s:cterms[02],s:ctermNONE,'')
call <sid>hi('DiffLine',s:cterms[04],s:ctermNONE,'')
call <sid>hi('DiffRemoved',s:cterms[15],s:cterms[01],'bold')
" }}}
" Languages {{{
call <sid>hi('cssColor',s:cterms[06],'','')
call <sid>hi('cssBraces',s:cterms[07],'','')
call <sid>hi('cssClassName',s:cterms[05],'','')

call <sid>hi('gitCommitOverflow',s:cterms[01],'','')
call <sid>hi('gitCommitSummary',s:cterms[02],'','')

call <sid>hi('htmlBold',s:cterms[03],'','')
call <sid>hi('htmlItalic',s:cterms[05],'','')
hi link htmlTag Noise
hi link htmlEndTag Noise
call <sid>hi('htmlArg',s:cterms[09],'','')
call <sid>hi('htmlTagName',s:cterms[01],'','')

call <sid>hi('jsGlobalObjects',s:cterms[09],'','')
call <sid>hi('jsNumber',s:cterms[09],'','')
call <sid>hi('jsBraces',s:cterms[07],'','')
call <sid>hi('jsFuncCall',s:cterms[04],'','')
call <sid>hi('jsStorageClass',s:cterms[05],'','')
hi link jsxAttrib htmlArg
hi link jsxComponentName htmlTagName
hi link jsxEndComponentName htmlTagName

call <sid>hi('markdownCode',s:cterms[02],'','')
call <sid>hi('markdownCodeBlock',s:cterms[02],'','')
call <sid>hi('markdownHeadingDelimiter',s:cterms[04],'','')
call <sid>hi('markdownItalic',s:cterms[05],'','italic')
call <sid>hi('markdownBold',s:cterms[03],'','bold')
call <sid>hi('markdownCodeDelimiter',s:cterms[14],'','italic')
call <sid>hi('markdownError',s:cterms[07],s:cterms[10],'')

call <sid>hi('pythonRepeat',s:cterms[05],'','')
call <sid>hi('pythonOperator',s:cterms[05],'','')

call <sid>hi('rubyConstant',s:cterms[03],'','')
call <sid>hi('rubySymbol',s:cterms[02],'','')
call <sid>hi('rubyAttribute',s:cterms[04],'','')
call <sid>hi('rubyInterpolation',s:cterms[02],'','')
call <sid>hi('rubyInterpolationDelimiter','',s:cterms[14],'')
call <sid>hi('rubyStringDelimiter',s:cterms[02],'','')
call <sid>hi('rubyRegexp',s:cterms[06],'','')

call <sid>hi('sassidChar',s:cterms[01],'','')
call <sid>hi('sassClassChar',s:cterms[09],'','')
call <sid>hi('sassInclude',s:cterms[05],'','')
call <sid>hi('sassMixing',s:cterms[05],'','')
call <sid>hi('sassMixinName',s:cterms[04],'','')

call <sid>hi('xmlTag',s:cterms[06],'','')
call <sid>hi('xmlTagName',s:cterms[07],'','')
call <sid>hi('xmlEndTag',s:cterms[06],'','')
"}}}
" Plugins {{{
call <sid>hi('ALEErrorSign',s:cterms[01],s:ctermNONE,'bold')
call <sid>hi('ALEWarningSign',s:cterms[03],s:ctermNONE,'bold')
call <sid>hi('ALEInfoSign',s:cterms[15],s:ctermNONE,'bold')
" }}}
" Unused Tags {{{

" call <sid>hi('Error',s:cterms[01],s:ctermNONE,'undercurl')
" call <sid>hi('NvimInternalError',s:cterms[01],s:ctermNONE,'')
" call <sid>hi('NvimInternalError',s:cterms[01],s:ctermNONE,'')

" call <sid>hi('NeomakeErrorSign',s:cterms[01],s:ctermNONE,'')
" call <sid>hi('NeomakeWarningSign',s:cterms[03],s:ctermNONE,'')
" call <sid>hi('NeomakeInfoSign',s:cterms[15],s:ctermNONE,'')
" call <sid>hi('NeomakeError',s:cterms[01],'','undercurl')
" call <sid>hi('NeomakeWarning',s:cterms[01],'','undercurl')

" call <sid>hi('NERDTreeExecFile',s:cterms[07],'','')
" call <sid>hi('NERDTreeDirSlash',s:cterms[04],'','')
" call <sid>hi('NERDTreeOpenable',s:cterms[04],'','')
" call <sid>hi('NERDTreeFile','',s:cterm:NONE,'','')
" call <sid>hi('NERDTreeFlags',s:cterms[04],'','')

" call <sid>hi('phpComparison',s:cterms[07],'','')
" call <sid>hi('phpParent',s:cterms[07],'','')
" call <sid>hi('phpMemberSelector',s:cterms[07],'','')
" call <sid>hi('vimfilerLeaf',s:cterms[07],'','')
" call <sid>hi('vimfilerNormalFile',s:cterms[07],s:ctermNONE,'')
" call <sid>hi('vimfilerOpenedFile',s:cterms[04],'','')
" call <sid>hi('vimfilerClosedFile',s:cterms[04],'','')

" call <sid>hi('GitGutterAdd',s:cterms[02],s:ctermNONE,'bold')
" call <sid>hi('GitGutterChange',s:cterms[04],s:ctermNONE,'bold')
" call <sid>hi('GitGutterDelete',s:cterms[01],s:ctermNONE,'bold')
" call <sid>hi('GitGutterChangeDelete',s:cterms[05],s:ctermNONE,'bold')

" call <sid>hi('SignifySignAdd',s:cterms[02],s:ctermNONE,'bold')
" call <sid>hi('SignifySignChange',s:cterms[04],s:ctermNONE,'bold')
" call <sid>hi('SignifySignDelete',s:cterms[01],s:ctermNONE,'bold')
" call <sid>hi('SignifySignChangeDelete',s:cterms[05],s:ctermNONE,'bold')
" call <sid>hi('SignifySignDeleteFirstLine',s:cterms[01],s:ctermNONE,'bold')

" call <sid>hi('csClass',s:cterms[03],'','')
" call <sid>hi('csAttribute',s:cterms[03],'','')
" call <sid>hi('csModifier',s:cterms[05],'','')
" call <sid>hi('csType',s:cterms[01],'','')
" call <sid>hi('csUnspecifiedStatement',s:cterms[04],'','')
" call <sid>hi('csContextualStatement',s:cterms[05],'','')
" call <sid>hi('csNewDecleration',s:cterms[01],'','')
" call <sid>hi('cOperator',s:cterms[06],'','')
" call <sid>hi('cPreCondit',s:cterms[05],'','')
" }}}
