scriptencoding=utf8
function! dotfiles#autocomplete#denite#init() abort
  call denite#custom#option('default', 'winheight', 10)
  call denite#custom#option('default', 'direction', 'rightbelow')
  call denite#custom#option('default', 'statusline', v:false)
  call denite#custom#option('default', 'highlight_matched_char', 'QuickFixLine')
  call denite#custom#option('default', 'highlight_matched_range', 'Visual')
  call denite#custom#option('default', 'highlight_window_background', 'Visual')
  call denite#custom#option('default', 'highlight_filter_background', 'DiffAdd')
  call denite#custom#var('file/rec', 'command', ['ag', '--follow', '--nocolor', '--nogroup', '-g', ''])
  call denite#custom#option('default', 'prompt', 'λ')
  call denite#custom#var('grep', 'command', ['ag'])
  call denite#custom#var('grep', 'default_opts', ['-i', '--vimgrep'])
  call denite#custom#var('grep', 'recursive_opts', [])
  call denite#custom#var('grep', 'pattern_opt', [])
  call denite#custom#var('grep', 'separator', ['--'])
  call denite#custom#var('grep', 'final_opts', [])
  call denite#custom#source('file_rec', 'sorters', ['sorter/sublime'])
  call denite#custom#filter('matcher_ignore_globs', 'ignore_globs',
        \ [ '.git/', '.ropeproject/', '__pycache__/*', '*.pyc', 'node_modules/',
        \   'venv/', 'images/', '*.min.*', 'img/', 'fonts/', '*.png'])
  command! -bang -nargs=? -complete=dir Files exe 'Denite -auto-action=preview -floating-preview -match-highlight -split=floating -vertical-preview -start-filter file/rec '.<q-args>
  command! -nargs=+ -complete=custom,dotfiles#rg_args Rg exe 'Denite grep:::'.<q-args>
  " Open location list:
  " Old FZF Interface:
  command! Buffers :exe 'Denite -split=floating buffer'
  command! Windows :exe 'Denite window'
  command! BLines :exe 'Denite line'
  command! Commands :exe 'Denite command_history'
  command! LocationList :exe 'Denite -split=floating -vertical-preview -auto-action=preview -floating-preview location_list'
  command! QuickfixList :exe 'Denite -split=floating -vertical-preview -auto-action=preview -floating-preview quickfix_list'
  command! Yanks :exe 'Denite -split=floating neoyank'
endfunction
