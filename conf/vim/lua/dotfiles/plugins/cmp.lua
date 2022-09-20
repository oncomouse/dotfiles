local function config_cmp()
	local cmp = require("cmp")

	cmp.setup({
		enabled = function()
			-- disable completion in comments
			local context = require("cmp.config.context")
			-- keep command mode completion enabled when cursor is in a comment
			if vim.api.nvim_get_mode().mode == "c" then
				return true
			else
				return not context.in_treesitter_capture("comment") and not context.in_syntax_group("Comment")
			end
		end,
		snippet = {
			expand = function(args)
				require("luasnip").lsp_expand(args.body)
			end,
		},
		mapping = {
			["<C-p>"] = cmp.mapping.select_prev_item(),
			["<C-n>"] = cmp.mapping.select_next_item(),
			["<C-x><C-o>"] = cmp.mapping.complete(),
			["<C-c>"] = cmp.mapping.abort(),
			["<C-e>"] = cmp.mapping(function(fallback)
				if cmp.visible() then
					cmp.mapping.close()
				elseif require("luasnip").choice_active() then
					require("luasnip").change_choice(1)
				else
					fallback()
				end
			end),
			["<C-y>"] = cmp.mapping.confirm({
				behavior = cmp.ConfirmBehavior.Replace,
				select = false,
			}),
		},
		sources = cmp.config.sources({
			{ name = "nvim_lsp" },
			{ name = "nvim_lua" },
			{ name = "luasnip" },
			{ name = "fish" },
			{ name = "neorg" },
			-- { name = "nvim_ref" },
		}),
		completion = {
			autocomplete = false,
		},
	})
	cmp.setup.filetype({ "markdown", "gina-commit" }, {
		completion = {
			autocomplete = false,
		},
	})
end

return config_cmp
