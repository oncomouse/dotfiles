local function config_neorg()
	require("neorg").setup({
		load = {
			["core.defaults"] = {
				config = {
					check_news = false,
				},
			},
			["core.norg.concealer"] = {
				config = {
					icons = {
						todo = {
							undone = { icon = " " },
						},
						heading = {
							level_5 = { icon = "     →"}
						}
					},
				},
			},
			["core.norg.completion"] = {
				config = {
					engine = "nvim-cmp",
				},
			},
			["core.integrations.nvim-cmp"] = {},
		},
	})
	require("nvim-treesitter.configs").setup({
		ensure_installed = {
			"norg", --[[ other parsers you would wish to have ]]
		},
		highlight = { -- Be sure to enable highlights if you haven't!
			enable = true,
		},
	})
	vim.api.nvim_create_autocmd("FileType", {
		pattern = "norg",
		callback = function()
			vim.keymap.set({ "i" }, "<C-t>", function()
				local line = vim.api.nvim_get_current_line()
				-- Don't add if we are at max nest level:
				if #line:match("[-~*>]+") == 6 then
					return ""
				end
				local insert_char = nil
				if line:match("^%s*[-]+") then
					insert_char = "-"
				elseif line:match("^%s*[~]+") then
					insert_char = "~"
				elseif line:match("^%s*[>]+") then
					insert_char = ">"
				elseif line:match("^%s*%*+") then
					insert_char = "*"
				end
				if insert_char == nil then
					return [[<C-t>]]
				end
				return [[<Esc><Cmd>keepjumps normal! F]] .. insert_char .. "a" .. insert_char .. [[<Cr>`^a]]
			end, {
				buffer = true,
				expr = true,
			})
			vim.keymap.set({ "i" }, "<C-d>", function()
				local line = vim.api.nvim_get_current_line()
				local delete_char = nil
				-- Prevent deleting leading whitespace if at a level one bullet
				if line:match("^%s*[-~*>][^-~*>]") then
					return ''
				end
				if line:match("^%s*-[-]+") then
					delete_char = "-"
				elseif line:match("^%s*~[~]+") then
					delete_char = "~"
				elseif line:match("^%s*>[>]+") then
					delete_char = ">"
				elseif line:match("^%s*%*%*+") then
					delete_char = "*"
				end
				if delete_char == nil then
					return [[<C-d>]]
				end

				local c = vim.fn.col(".")
				local pos = "`^hi"
				if c > #line then
					pos = "$a"
				elseif c == #line then
					pos = "$i"
				end
				return [[<Esc><Cmd>keepjumps normal! F]] .. delete_char .. [==[x<Cr>]==] .. pos

			end, {
				expr = true,
				buffer = true,
				desc = "Override <C-d> for neorg"
			})
		end,
		group = "dotfiles-settings",
	})
end

return config_neorg
