" Initialize Autocomplete:
function! dotfiles#autocomplete#init() abort
  if g:complete_package ==# 'coc.nvim'
    " Combined Functions:
    call dotfiles#autocomplete#coc_nvim#init()
    " Writing:
    call dotfiles#autocomplete#coc_nvim#writing()
  else
    " Language Server Client:
    if has('nvim-0.5')
      call dotfiles#autocomplete#nvim_lsp#init()
    else
      call dotfiles#autocomplete#LanguageClient#init()
    endif
    " Linter:
    call dotfiles#autocomplete#ale#init()
    " List Management:
    call dotfiles#autocomplete#{substitute(g:complete_package, '[.-]', '_', 'g')}#init()
  endif
endfunction

" Support for a completefunc that gets snippets, files, and bufwords:
" Adapted from: https://gist.github.com/brenopacheco/0c1cb9b58d478716a8345a7cf8689ca8
function! dotfiles#autocomplete#snippets(findstart, base) abort
  if a:findstart
    let keywordregex = '\k\|\(\.\?\/\?\)\+'
    let regex = '\(' . keywordregex . '\)\+\%' . col('.') . 'c'
    return match(getline('.'), regex)
  endif
  let limit = 6
  " let buffers  = s:sort(s:buffer_source(a:base))[0:limit]
  let snippets = s:sort(s:snippets_source(a:base))[0:limit]
  " let lsp      = s:sort(s:lsp_source(a:base))[0:limit]
  " let files    = s:sort(s:file_source(a:base))[0:2*limit]
  let lists = [
      \   { 'list': snippets, 'priority': 3  },
      \ ]
  " \   { 'list': buffers,  'priority': 1  },
  " \   { 'list': files,    'priority': 2  },
  " \   { 'list': lsp,      'priority': 4  }
  return s:mix(lists)
endfunction

function! s:sort(list) abort
    return sort(a:list, { a,b -> len(a.word) > len(b.word) })
endfunction

function! s:mix(lists) abort
    let res = []
    for group in a:lists
        for item in group.list
            let item.priority = group.priority
            let res += [item]
        endfor
    endfor
    return sort(res,  { a,b -> len(a.word) > len(b.word) ? 1 : 
                \ len(a.word) < len(b.word) ? 0 : 
                \ a.priority < b.priority })
endfunction

" priorities -> [ 1, 5, 2, 3 ]
function! s:best(priorities) abort
  let best = 0
  for i in a:priorities
      let best = i > best ? i : best
  endfor
endfunction

function! s:snippets_source(word) abort
  try
      return 
          \   filter(
          \       vsnip#get_complete_items(bufnr()), 
          \   { _,s -> s.word =~ '^'.a:word.'.*' })
  catch /.*/
      return []
  endtry
endfunction

function! s:buffer_source(word) abort
  return  
  \ map(
  \     uniq(
  \       sort( 
  \           filter(
  \               split(
  \                   join(
  \                       getline(1,'$'), 
  \                   "\n"), 
  \               '\<\|\>'), 
  \           { _,s -> s =~# '^\(\k\+\)$' 
  \                 && s !~ '[#_\-1-9]' 
  \                 && len(s) > 1 
  \                 && s =~ '^'.a:word.'.*'}))),
  \ { _,s -> { 'word': s, 'kind': 'buffer' } })
endfunction

function! s:file_source(word) abort
  let files = filter(getcompletion(a:word, 'file'), 'match(v:val, "//") < 0')
  return map(files, { _,file -> { 'kind': 'file', 'word': file } })
endfunction

function! s:lsp_source(word) abort
    try
        return v:lua.omnifunc_sync(a:word)
    catch /.*/
        return []
    endtry
endfunction

lua << EOF
function _G.omnifunc_sync(base)
  local params = vim.lsp.util.make_position_params()
  local result = vim.lsp.buf_request_sync(bufnr, 'textDocument/completion', params, 2000)
  local items = {}
  if result then
    for _, item in ipairs(result) do
      if not item.err then
        local matches = vim.lsp.util.text_document_completion_list_to_complete_items(item.result, base)
        vim.list_extend(items, matches)
      end
    end
  end
  return items
end
EOF
