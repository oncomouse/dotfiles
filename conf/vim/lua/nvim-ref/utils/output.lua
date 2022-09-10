local M = {}
-- This is from fzf-lua:
local function _echo_multiline(msg)
  for _, s in ipairs(vim.fn.split(msg, "\n")) do
    vim.cmd("echom '" .. s:gsub("'", "''").."'")
  end
end

function M.info(msg)
  vim.cmd('echohl Directory')
  _echo_multiline("[Fzf-lua] " .. msg)
  vim.cmd('echohl None')
end

function M.warn(msg)
  vim.cmd('echohl WarningMsg')
  _echo_multiline("[Fzf-lua] " .. msg)
  vim.cmd('echohl None')
end

function M.err(msg)
  vim.cmd('echohl ErrorMsg')
  _echo_multiline("[Fzf-lua] " .. msg)
  vim.cmd('echohl None')
end
return M
