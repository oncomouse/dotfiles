local function config_cmp()
	local cmp = require("cmp")

	local feedkey = function(key, mode)
		vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes(key, true, true, true), mode or "", true)
	end
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
			["<C-u>"] = cmp.mapping.scroll_docs(-4),
			["<C-d>"] = cmp.mapping.scroll_docs(4),
			["<C-x><C-o>"] = cmp.mapping.complete(),
			["<C-c>"] = cmp.mapping.abort(),
			["<C-e>"] = cmp.mapping(function(fallback)
				if cmp.visible() then
					cmp.mapping.close()
				elseif require("luasnip").choice_active() then
					feedkey("<Plug>luasnip-next-choice")
				else
					fallback()
				end
			end),
			["<C-y>"] = cmp.mapping.confirm({
				behavior = cmp.ConfirmBehavior.Replace,
				select = false,
			}),
			["<Tab>"] = cmp.mapping(function(fallback)
				if require("luasnip").expand_or_locally_jumpable() then
					feedkey("<Plug>luasnip-expand-or-jump")
				else
					fallback()
				end
			end, { "i", "s" }),
			["<S-Tab>"] = cmp.mapping(function(fallback)
				if require("luasnip").jumpable(-1) then
					feedkey("<Plug>luasnip-jump-prev")
				else
					fallback()
				end
			end, { "i", "s" }),
		},
		sources = cmp.config.sources({
			{ name = "nvim_lsp" },
			{ name = "nvim_lua" },
			{ name = "luasnip" },
			{ name = "fish" },
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
