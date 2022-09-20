local function config_luasnips()
	local ls = require("luasnip")
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
	})

	-- Maps for LuaSnip:
	local ok = pcall(require, "cmp") -- Do we have completion?
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
	if not ok then
		vim.keymap.set({ "i", "s" }, "<C-E>", function()
			if ls.choice_active() then
				return "<Plug>luasnip-next-choice"
			end
			return t("<C-E>")
		end, {
			expr = true,
			remap = true,
		})
	end

	-- Loaders:
	require("luasnip.loaders.from_vscode").lazy_load()
	require("luasnip.loaders.from_lua").lazy_load({ paths = "~/dotfiles/conf/vim/snippets" })

	local augroup = vim.api.nvim_create_augroup("dotfiles-settings-luasnips", { clear = true })
	vim.api.nvim_create_autocmd("CompleteDone", {
		group = augroup,
		callback = function()
			if ls.available(1) then
				ls.expand()
			end
		end,
	})
	vim.api.nvim_create_autocmd("InsertLeave", {
		group = augroup,
		callback = function()
			if ls.session.current_nodes[vim.api.nvim_get_current_buf()] and not ls.session.jump_active then
				ls.unlink_current()
			end
		end,
	})
end

return config_luasnips
