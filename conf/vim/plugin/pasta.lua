local function t(str)
	-- Adjust boolean arguments as needed
	return vim.api.nvim_replace_termcodes(str, true, true, true)
end

local function normal_pasta(p, o)
	if vim.fn.getregtype() == "V" then
		vim.cmd("normal! " .. o .. t"<space><bs><esc>" .. vim.v.count1 .. '"' .. vim.v.register .. "]p")
		-- Save the `[ and `] marks (point to the last modification)
		local first = vim.fn.getpos("'[")
		local last = vim.fn.getpos("']")
		vim.cmd([[normal! k"_dd]])
		-- Compensate the line we have just deleted
		first[2] = first[2] - 1
		last[2] = first[2] - 1
		vim.fn.setpos("'[", first)
		vim.fn.setpos("']", last)
	else
		vim.cmd("normal! " .. vim.v.count1 .. '"' .. vim.v.register .. p)
	end
end

local function visual_pasta()
	if vim.fn.visualmode() == "V" then
		if vim.fn.getregtype() == "V" then
			vim.cmd([["normal! gv\"_c\<space>\<bs>\<esc>]] .. vim.v.count1 .. '"' .. vim.v.register .. ']pk"_dd')
		else
			vim.cmd([[normal! gv\"_c\<space>\<bs>\<esc>]] .. vim.v.count1 .. '"' .. vim.v.register .. "]p")
		end
	else
		-- workaround strange Vim behavior (""p is no-op in visual mode)
		local reg = vim.v.register == '"' and "" or '"' .. vim.v.register

		vim.cmd("normal! gv" .. vim.v.count1 .. reg .. "p")
	end
end

local function setup_pasta()
	if vim.fn.exists("g:pasta_enabled_filetypes") == 1 then
		if vim.fn.index(vim.g.pasta_enabled_filetypes, vim.bo.filetype) == -1 then
			return
		end
	elseif
		vim.fn.exists("g:pasta_disabled_filetypes")
		and vim.fn.index(vim.g.pasta_disabled_filetypes, vim.bo.filetype) ~= -1
	then
		return
	end
	vim.keymap.set("n", vim.g.pasta_paste_before_mapping, "<Plug>BeforePasta", { buffer = true })
	vim.keymap.set("x", vim.g.pasta_paste_before_mapping, "<Plug>VisualPasta", { buffer = true })
	vim.keymap.set("n", vim.g.pasta_paste_after_mapping, "<Plug>AfterPasta", { buffer = true })
	vim.keymap.set("x", vim.g.pasta_paste_after_mapping, "<Plug>VisualPasta", { buffer = true })
end

if vim.fn.exists("g:pasta_disabled_filetypes") == 0 then
	vim.g.pasta_disabled_filetypes = {
		"python",
		"coffee",
		"markdown",
		"yaml",
		"slim",
		"nerdtree",
		"netrw",
		"startify",
		"ctrlp",
	}
end

vim.g.pasta_paste_before_mapping = "P"
vim.g.pasta_paste_after_mapping = "p"

vim.keymap.set("n", "<Plug>BeforePasta", function()
	normal_pasta("P", "O")
end, { silent = true })
vim.keymap.set("n", "<Plug>AfterPasta", function()
	normal_pasta("p", "o")
end, { silent = true })
vim.keymap.set("x", "<Plug>VisualPasta", function()
	visual_pasta()
end, { silent = true })

vim.api.nvim_create_augroup("vim_pasta", {})
vim.api.nvim_create_autocmd("FileType", {
	group = "vim_pasta",
	pattern = { "*" },
	callback = setup_pasta,
})
