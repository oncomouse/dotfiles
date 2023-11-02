-- Mf for directory creation:
vim.api.nvim_create_user_command("Mf", function(args)
	local file = vim.loop.fs_realpath(args.args) or args.args
	vim.fn.mkdir(vim.fn.fnamemodify(file, ":p:h"), "p")
	vim.cmd("e " .. file)
end, {
	complete = "dir",
	nargs = 1,
})

-- Auto create dir when saving a file, in case some intermediate directory does not exist
vim.api.nvim_create_autocmd({ "BufWritePre" }, {
  group = vim.api.nvim_create_augroup("auto_create_dir", { clear = true }),
  callback = function(event)
    if event.match:match("^%w%w+://") then
      return
    end
    local file = vim.loop.fs_realpath(event.match) or event.match
    vim.fn.mkdir(vim.fn.fnamemodify(file, ":p:h"), "p")
  end,
})
