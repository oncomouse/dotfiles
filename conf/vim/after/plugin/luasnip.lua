local ok, ls = pcall(require, "luasnip")

if ok then
	local types = require("luasnip.util.types")
	local t = require("dotfiles.utils.termcode")

	-- Configuration:
	ls.config.set_config({
		history = true,
		updateevents = "TextChanged,TextChangedI",
		enable_autosnippets = true,
		ext_opts = {
			[types.choiceNode] = {
				active = {
					virt_text = { { "●", "AnsiColor3" } },
				},
			},
			[types.insertNode] = {
				active = {
					virt_text = { { "●", "AnsiColor4" } },
				},
			},
		},
		snip_env = {
			s = require("luasnip.nodes.snippet").S,
			sn = require("luasnip.nodes.snippet").SN,
			t = require("luasnip.nodes.textNode").T,
			f = require("luasnip.nodes.functionNode").F,
			i = require("luasnip.nodes.insertNode").I,
			c = require("luasnip.nodes.choiceNode").C,
			d = require("luasnip.nodes.dynamicNode").D,
			r = require("luasnip.nodes.restoreNode").R,
			l = require("luasnip.extras").lambda,
			rep = require("luasnip.extras").rep,
			p = require("luasnip.extras").partial,
			m = require("luasnip.extras").match,
			n = require("luasnip.extras").nonempty,
			dl = require("luasnip.extras").dynamic_lambda,
			fmt = require("luasnip.extras.fmt").fmt,
			fmta = require("luasnip.extras.fmt").fmta,
			conds = require("luasnip.extras.expand_conditions"),
			types = require("luasnip.util.types"),
			events = require("luasnip.util.events"),
			parse = require("luasnip.util.parser").parse_snippet,
			ai = require("luasnip.nodes.absolute_indexer"),
			postfix = require("luasnip.extras.postfix").postfix,
		},
	})

	-- Maps for LuaSnip:
	vim.keymap.set({ "i", "s" }, "<Tab>", function()
		if ls.expand_or_locally_jumpable() then
			return "<Plug>luasnip-expand-or-jump"
		end
		return t("<Tab>")
	end, {
		expr = true,
		remap = true,
	})
	vim.keymap.set({ "i", "s" }, "<S-Tab>", function()
		if ls.jumpable(-1) then
			return "<Plug>luasnip-jump-prev"
		end
		return t("<S-Tab>")
	end, {
		expr = true,
		remap = true,
	})
	vim.keymap.set({ "i", "s" }, "<C-E>", function()
		if ls.choice_active() then
			return "<Plug>luasnip-next-choice"
		end
		return t("<C-E>")
	end, {
		expr = true,
		remap = true,
	})
	vim.api.nvim_create_autocmd("CompleteDone", {
		group = "dotfiles-settings",
		callback = function()
			if ls.available(1) then
				ls.expand()
			end
		end,
	})

	-- Loaders:
	require("luasnip.loaders.from_vscode").lazy_load()
	require("luasnip.loaders.from_lua").load({ paths = "~/dotfiles/conf/vim/snippets" })
end
