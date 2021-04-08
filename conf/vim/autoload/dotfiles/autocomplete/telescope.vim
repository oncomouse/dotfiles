function! dotfiles#autocomplete#telescope#init() abort
  " Old FZF Interface:
  nmap <Plug>(dotfiles-files) :<C-u>lua require("telescope.builtin").find_files({sorter=require("telescope.sorters").get_fzy_sorter()})<CR>
  nmap <Plug>(dotfiles-buffers) :<C-u>lua require("telescope.builtin").buffers({ sort_lastused = true, ignore_current_buffer = true, show_all_buffers = true, sorter=require("telescope.sorters").get_fzy_sorter() })<CR>
  nmap <Plug>(dotfiles-lines) :<C-u>Telescope current_buffer_fuzzy_find<CR>
  nmap <Plug>(dotfiles-commands) :<C-u>Telescope command_history<CR>
  nmap <Plug>(dotfiles-yanks) <nop>
  nmap <Plug>(dotfiles-windows) <nop>
  nmap <Plug>(dotfiles-home-files) <nop>
lua <<EOF
local actions = require('telescope.actions')
local action_state = require('telescope.actions.state')
local transform_mod = require('telescope.actions.mt').transform_mod

myactions = transform_mod({
  send_to_qf_or_open = function(prompt_bufnr)
    local picker = action_state.get_current_picker(prompt_bufnr)
    if #picker:get_multi_selection() <= 1 then
      actions.select_default(prompt_bufnr)
    else
      actions.send_selected_to_qflist(prompt_bufnr)
      vim.api.nvim_command("copen")
      vim.api.nvim_command("cfirst")
    end
  end,
})

require('telescope').setup{
  defaults = {
    mappings = {
      i = {
        ["<esc>"] = actions.close,
        ["<CR>"] = myactions.send_to_qf_or_open,
      },
    },
  }
}
EOF
endfunction
