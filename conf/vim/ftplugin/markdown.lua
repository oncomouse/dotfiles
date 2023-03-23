-- Local settings for Markdown
vim.opt_local.wrap = true
vim.opt_local.linebreak = true
vim.opt_local.list = false
vim.opt_local.spell = true
vim.opt_local.showbreak = "NONE"

-- Turn conceal on and off in a buffer:
vim.keymap.set("n", "<leader>cc", function()
	vim.opt_local.conceallevel = vim.opt_local.conceallevel == 0 and 2 or 0
end, {
	buffer = true,
	silent = true,
})

vim.cmd([[compiler markdown_combo]])

-- vim.opt_local.iskeyword = vim.opt_local.iskeyword + "',-,@-@"

-- Pandoc <format> to compile documents quickly and easily:
vim.api.nvim_create_user_command("Pandoc", function(args)
	vim.cmd(
		"!pandoc -i "
			.. vim.fn.fnameescape(vim.fn.expand("%"))
			.. " -o "
			.. vim.fn.fnameescape(vim.fn.expand("%:r"))
			.. "."
			.. args.args
	)
end, {
	nargs = 1,
})

-- Markdown helper maps:

require("dotfiles.markdown").set_buf_maps()

-- The vim-markdown movement commands (source https://github.com/preservim/vim-markdown/blob/master/ftplugin/markdown.vim):
local headersRegexp = [[\v^(#|.+\n(\=+|-+)$)]]
local headersRegexpCompiled = vim.regex(headersRegexp)
local levelRegexpDict = {
	vim.regex([[\v^(#[^#]@=|.+\n\=+$)]]),
	vim.regex([[\v^(##[^#]@=|.+\n-+$)]]),
	vim.regex([[\v^###[^#]@=]]),
	vim.regex([[\v^####[^#]@=]]),
	vim.regex([[\v^#####[^#]@=]]),
	vim.regex([[\v^######[^#]@=]]),
}

local function move_to_next_header()
	if vim.fn.search(headersRegexp, "W") == 0 then
		vim.print("no next header")
	end
end

local function get_header_line_num(l)
	if l == nil then
		l = vim.fn.line(".")
	end
	while l > 0 do
		if headersRegexpCompiled:match_str(vim.fn.join(vim.fn.getline(l, l + 1), "\n")) then
			return l
		end
		l = l - 1
	end
	return 0
end

local function get_level_of_header_at_line(linenum)
	local lines = vim.fn.join(vim.fn.getline(linenum, linenum + 1), "\n")
	for i, regex in ipairs(levelRegexpDict) do
		if regex:match_str(lines) then
			return i
		end
	end
	return 0
end

local function get_header_level(line)
	if line == nil then
		line = vim.fn.line(".")
	end
	local linenum = get_header_line_num(line)
	if linenum ~= 0 then
		return get_level_of_header_at_line(linenum)
	else
		return 0
	end
end

local function get_previous_header_line_number_at_level(level, line)
	if line == nil then
		line = vim.fn.line(".")
	end
	local l = line
	while l > 0 do
		if levelRegexpDict[level]:match_str(vim.fn.join(vim.fn.getline(l, l + 1), "\n")) then
			return l
		end
		l = l - 1
	end
	return 0
end

local function get_next_header_line_number_at_level(level, line)
	if line == nil then
		line = vim.fn.line(".")
	end
	local l = line
	while l <= vim.fn.line("$") do
		if levelRegexpDict[level]:match_str(vim.fn.join(vim.fn.getline(l, l + 1), "\n")) then
			return l
		end
		l = l + 1
	end
	return 0
end

local function get_parent_header_line_number(line)
	if line == nil then
		line = vim.fn.line(".")
	end
	local level = get_header_level(line)
	local linenum
	if level > 1 then
		linenum = get_previous_header_line_number_at_level(level - 1, line)
		return linenum
	end
	return 0
end

local function move_to_previous_header()
	local curHeaderLineNumber = get_header_line_num()
	local noPreviousHeader = 0
	if curHeaderLineNumber <= 1 then
		noPreviousHeader = 1
	else
		local previousHeaderLineNumber = get_header_line_num(curHeaderLineNumber - 1)
		if previousHeaderLineNumber == 0 then
			noPreviousHeader = 1
		else
			vim.fn.cursor(previousHeaderLineNumber, 1)
		end
	end
	if noPreviousHeader then
		vim.print("no previous header")
	end
end

local function move_to_parent_header()
	local linenum = get_parent_header_line_number()
	if linenum ~= 0 then
		vim.fn.setpos("''", vim.fn.getpos("."))
		vim.fn.cursor(linenum, 1)
	else
		vim.print("no parent header")
	end
end

local function move_to_next_sibling_header()
	local curHeaderLineNumber = get_header_line_num()
	local curHeaderLevel = get_level_of_header_at_line(curHeaderLineNumber)
	local curHeaderParentLineNumber = get_parent_header_line_number()
	local nextHeaderSameLevelLineNumber = get_next_header_line_number_at_level(curHeaderLevel, curHeaderLineNumber + 1)
	local noNextSibling = 0
	if nextHeaderSameLevelLineNumber == 0 then
		noNextSibling = 1
	else
		local nextHeaderSameLevelParentLineNumber = get_parent_header_line_number(nextHeaderSameLevelLineNumber)
		if curHeaderParentLineNumber == nextHeaderSameLevelParentLineNumber then
			vim.fn.cursor(nextHeaderSameLevelLineNumber, 1)
		else
			noNextSibling = 1
		end
	end
	if noNextSibling then
		vim.print("no next sibling header")
	end
end

local function move_to_previous_sibling_header()
	local curHeaderLineNumber = get_header_line_num()
	local curHeaderLevel = get_level_of_header_at_line(curHeaderLineNumber)
	local curHeaderParentLineNumber = get_parent_header_line_number()
	local previousHeaderSameLevelLineNumber =
		get_previous_header_line_number_at_level(curHeaderLevel, curHeaderLineNumber - 1)
	local noPreviousSibling = 0
	if previousHeaderSameLevelLineNumber == 0 then
		noPreviousSibling = 1
	else
		local previousHeaderSameLevelParentLineNumber = get_parent_header_line_number(previousHeaderSameLevelLineNumber)
		if curHeaderParentLineNumber == previousHeaderSameLevelParentLineNumber then
			vim.fn.cursor(previousHeaderSameLevelLineNumber, 1)
		else
			noPreviousSibling = 1
		end
	end
	if noPreviousSibling then
		vim.print("no previous sibling header")
	end
end

local function move_to_cur_header()
	local lineNum = get_header_line_num()
	if lineNum ~= 0 then
		vim.fn.cursor(lineNum, 1)
	else
		vim.print("outside any header")
	end
	return lineNum
end

local function vis_move(f)
	return function()
		vim.cmd([[norm! gv]])
		f()
	end
end

local function map_norm_vis(rhs, lhs)
	vim.keymap.set("n", rhs, lhs, { buffer = true, silent = true })
	vim.keymap.set("v", rhs, vis_move(lhs), { buffer = true, silent = true })
end

map_norm_vis("<Plug>Markdown_MoveToNextHeader", move_to_next_header)
map_norm_vis("<Plug>Markdown_MoveToPreviousHeader", move_to_previous_header)
map_norm_vis("<Plug>Markdown_MoveToNextSiblingHeader", move_to_next_sibling_header)
map_norm_vis("<Plug>Markdown_MoveToPreviousSiblingHeader", move_to_previous_sibling_header)
map_norm_vis("<Plug>Markdown_MoveToParentHeader", move_to_parent_header)
map_norm_vis("<Plug>Markdown_MoveToCurHeader", move_to_cur_header)

if vim.fn.get(vim.g, "vim_markdown_no_default_key_mappings", 0) == 0 then
	vim.keymap.set({ "n", "v" }, "]]", "<Plug>Markdown_MoveToNextHeader", { buffer = true })
	vim.keymap.set({ "n", "v" }, "[[", "<Plug>Markdown_MoveToPreviousHeader", { buffer = true })
	vim.keymap.set({ "n", "v" }, "][", "<Plug>Markdown_MoveToNextSiblingHeader", { buffer = true })
	vim.keymap.set({ "n", "v" }, "[]", "<Plug>Markdown_MoveToPreviousSiblingHeader", { buffer = true })
	vim.keymap.set({ "n", "v" }, "]u", "<Plug>Markdown_MoveToParentHeader", { buffer = true })
	vim.keymap.set({ "n", "v" }, "]h", "<Plug>Markdown_MoveToCurHeader", { buffer = true })
end
