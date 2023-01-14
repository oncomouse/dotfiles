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
	local has_cmp = pcall(require, "cmp") -- Do we have completion?
	if not has_cmp then
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
	require("luasnip.loaders.from_vscode").lazy_load({
		default_priority = 1000,
	})
	require("luasnip.loaders.from_lua").lazy_load({
		paths = "~/dotfiles/conf/vim/snippets",
		default_priotity = 2000,
	})

	local augroup = vim.api.nvim_create_augroup("dotfiles-settings-luasnips", { clear = true })
	-- Portions of this code (related to clearing the region) have been adapted from cmp_luasnip
	if not has_cmp then
		local function make_clear_region(cursor, word)
			return {
				clear_region = {
					from = {
						cursor[1],
						cursor[2] - #word,
					},
					to = cursor,
				},
			}
		end
		local function get_lsp_snippet(body)
			return ls.parser.parse_snippet(
				"",
				body,
				{ trim_empty = false, dedent = false }
			)
		end
		vim.api.nvim_create_autocmd("CompleteDone", {
			group = augroup,
			callback = function()
				if type(vim.v.completed_item.user_data) == "table" then
					local completion_item = vim.v.completed_item.user_data.nvim.lsp.completion_item
					local cursor = vim.api.nvim_win_get_cursor(0)
					cursor[1] = cursor[1] - 1
					local word = vim.v.completed_item.word
					local snippet = nil
					-- null-ls luasnip provider:
					if completion_item.data then
						snippet = ls.get_id_snippet(completion_item.data.snip_id)
						if snippet.regTrig then
							snippet = snippet:get_pattern_expand_helper()
						end
					-- VSCode LSPs:
					elseif completion_item.textEdit then
						snippet = get_lsp_snippet(completion_item.textEdit.newText)
					-- Sumneko-lua:
					elseif completion_item.insertTextFormat and completion_item.insertTextFormat == 2 then
						snippet = get_lsp_snippet(completion_item.insertText)
					end
					if snippet ~= nil then
						ls.snip_expand(snippet, make_clear_region(cursor, word))
					end
				end
			end,
		})
	end
	vim.api.nvim_create_autocmd("InsertLeave", {
		group = augroup,
		callback = function()
			if ls.session.current_nodes[vim.api.nvim_get_current_buf()] and not ls.session.jump_active then
				ls.unlink_current()
			end
		end,
	})
end

