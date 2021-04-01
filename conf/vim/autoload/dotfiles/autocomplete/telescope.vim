function! dotfiles#autocomplete#telescope#init() abort
  " Old FZF Interface:
  command! Files :exe 'lua require("telescope.builtin").find_files({sorter=require("telescope.sorters").get_fzy_sorter()})'
  command! Buffers :exe 'lua require("telescope.builtin").buffers({ sort_lastused = true, ignore_current_buffer = true, show_all_buffers = true, sorter=require("telescope.sorters").get_fzy_sorter() })'
  command! Windows :exe ''
  command! BLines :exe 'Telescope current_buffer_fuzzy_find'
  command! Commands :exe 'Telescope command_history'
  command! Yanks :exe ''
  command! -nargs=+ -complete=custom,dotfiles#rg_args Rg exe 'Telescope gre_string search='.<q-args>
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
