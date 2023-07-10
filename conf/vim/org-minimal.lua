-- Set Leader:
vim.g.mapleader = " "
local lazypath = os.getenv("HOME") .. "/.local/share/lazy-org/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)
require("lazy").setup({
  root = vim.fn.stdpath("data") .. "/lazy-org",
  dev = {
    path = vim.fn.expand("~/Projects"),
  },
  spec = {
    {
      "nvim-orgmode/orgmode",
      dev = true,
      opts = {},
      config = function(_, opts)
        require("orgmode").setup_ts_grammar()
        require("nvim-treesitter.configs").setup({
          highlight = {
            enable = true,
            additional_vim_regex_highlighting = { "org" },
          },
          ensure_installed = { "org" },
        })
        require("orgmode").setup(opts)
      end,
      dependencies = {
        {
          "nvim-treesitter/nvim-treesitter",
        },
      },
    },
  },
})
-- # vim:ts=2:sw=2:sts=2:expandtab
