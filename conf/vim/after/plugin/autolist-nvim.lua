require("autolist").setup({
	colon = {
		preferred = "",
		indent = false,
		indent_raw = false,
	},
	invert = {
		ul_marker = "*",
	},
	normal_mappings = {
		new = { "" },
		tab = { "" },
		detab = { "" },
		recal = { "" },
	},
	insert_mappings = {
		invert = { "" },
		new = { "" },
		detab = { "" },
		tab = { "" },
		recal = { "" },
		indent = { "" },
	},
})
vim.keymap.set("", "<Plug>(autolist-invert)", "<cmd>lua require('autolist').invert()<CR>")
vim.keymap.set("", "<Plug>(autolist-new)", "<cmd>lua require('autolist').new()<CR>")
vim.keymap.set("", "<Plug>(autolist-new-above)", "<cmd>lua require('autolist').new(true)<CR>")
vim.keymap.set("", "<Plug>(autolist-tab)", "<cmd>lua require('autolist').tab()<CR>")
vim.keymap.set("", "<Plug>(autolist-detab)", "<cmd>lua require('autolist').detab()<CR>")
vim.keymap.set("", "<Plug>(autolist-recal)", "<cmd>lua require('autolist').recal()<CR>")
vim.keymap.set("", "<Plug>(autolist-indent-increase)", "<cmd>lua require('autolist').indent('>>')<CR>")
vim.keymap.set("", "<Plug>(autolist-indent-decrease)", "<cmd>lua require('autolist').indent('<<')<CR>")
vim.api.nvim_create_autocmd("FileType", {
	pattern = "markdown,text",
	group = "dotfiles-settings",
	callback = function(args)
		-- Set the bindings we want:
		vim.keymap.set("i", "<C-z>", "<Plug>(autolist-invert)", { buffer = args.buf })
		vim.keymap.set("i", "<C-t>", "<C-t><Plug>(autolist-tab)", { buffer = args.buf })
		vim.keymap.set("i", "<CR>", "<CR><Plug>(autolist-new)", { buffer = args.buf })
		vim.keymap.set("n", ">>", ">><Plug>(autolist-tab)", { buffer = args.buf })
		vim.keymap.set("n", "<<", "<<<Plug>(autolist-detab)", { buffer = args.buf })
		vim.keymap.set("n", "o", "o<Plug>(autolist-new)", { buffer = args.buf })
		vim.keymap.set("n", "O", "O<Plug>(autolist-new-above)", { buffer = args.buf })
		vim.keymap.set("n", "<C-z>", "<Plug>(autolist-recal)", { buffer = args.buf })
		-- <C-d> to delete list marker if that's all that's left
		local function get_match(regex, line)
			local match = nil
			local start, ed = vim.regex(regex):match_str(line)
			if start ~= nil then
				match = string.sub(line, start, ed + 1)
			end
			return match
		end
		vim.keymap.set("i", "<C-d>", function()
			local line = vim.api.nvim_get_current_line()
			-- Check line for unordered list:
			local match = get_match([[^[*-] \(\[.\]\)\{0,1\}]], line)
			-- Check line for ordered list:
			if not match then
				match = get_match([[^\d\+\. \(\[.\]\)\{0,1\}]], line)
			end
			if match then
				local savepos = vim.fn.winsaveview().col
				local jump = (savepos == #line) and "$a" or savepos - #match .. "li"
				return '<Esc>0"_' .. #match .. "dl" .. jump
			end
			return "<C-d><Plug>(autolist-detab)"
		end, {
			expr = true,
			buffer = args.buf,
		})
	end,
})
