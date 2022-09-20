local ok, autolist = pcall(require, "autolist.nvim")
if ok then
	autolist.setup({
		colon = {
			preferred = "",
			indent = false,
			indent_raw = false,
		},
		invert = {
			ul_marker = "*",
		},
		insert_mappings = {
			invert = {
				"",
			},
			indent = {
				"",
			},
		},
	})
	-- <C-d> to delete list marker if that's all that's left
	vim.api.nvim_create_autocmd("FileType", {
		pattern = "markdown,text",
		group = "dotfiles-settings",
		callback = function(args)
			vim.keymap.set("i", "<C-d>", function()
				local line = vim.api.nvim_get_current_line()
				-- Check line for unordered list:
				local match = string.match(line, "^[*-] ")
				-- Check line for ordered list:
				if not match then
					match = string.match(line, "^%d+%. ")
				end
				if match then
					local savepos = vim.fn.winsaveview().col
					local jump = (savepos == #line) and "$a" or savepos - #match .. "li"
					return '<Esc>0"_2dl' .. jump
				end
				return "<C-d><Cmd>lua require('autolist').detab()<CR>"
			end, {
				expr = true,
				buffer = args.buf,
			})
		end,
	})
end
