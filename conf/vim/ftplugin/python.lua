vim.cmd([[compiler flake8]])
vim.opt_local.formatprg = "black --quiet -|reorder-python-imports -"
