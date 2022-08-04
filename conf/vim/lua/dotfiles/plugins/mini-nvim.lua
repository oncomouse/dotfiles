local function config_mini()
	-- local augroup = vim.api.nvim_create_augroup("dotfiles-mini.nvim", { clear = true })
	local spec_pair = require("mini.ai").gen_spec.pair
	require("mini.ai").setup({
		custom_textobjects = {
			e = function()
				local from = { line = 1, col = 1 }
				local to = {
					line = vim.fn.line("$"),
					col = vim.fn.getline("$"):len(),
				}
				return { from = from, to = to }
			end,
			s = function(type)
				local punc_matcher = "[.?!]"
				local _, lnum, col, _ = unpack(vim.fn.getpos("."))
				local line = vim.api.nvim_get_current_line()
				local end_of_sentence = string.find(string.sub(line, col, #line), punc_matcher)
				if not end_of_sentence then
					return nil
				end
				end_of_sentence = end_of_sentence + col - 1
				local _, start_of_sentence = string.find(string.sub(line, 1, col), punc_matcher .. "%s+[A-Z]")
				if start_of_sentence == nil and string.match(string.sub(line, 1, 1), "[A-Z]") then
					start_of_sentence = 1
				end
				if not start_of_sentence then
					return nil
				end
				end_of_sentence = type == "i" and end_of_sentence - 1 or end_of_sentence
				return {
					from = {
						line = lnum,
						col = start_of_sentence,
					},
					to = {
						line = lnum,
						col = end_of_sentence,
					},
				}
			end,
			["*"] = spec_pair("*", "*", { type = "greedy" }),
			["_"] = spec_pair("_", "_", { type = "greedy" }),
		},
		mappings = {
			around_last = "aN",
			inside_last = "iN",
		},
	})
	-- vim.api.nvim_create_autocmd("FileType", {
	-- 	pattern = "markdown",
	-- 	group = augroup,
	-- 	callback = function()
	-- 		-- local spec_pair = require("mini.ai").gen_spec.pair
	-- 		-- vim.b.miniai_config = {
	-- 		-- 	custom_textobjects = {
	-- 		-- 		["*"] = spec_pair("*", "*", { type = "greedy" }),
	-- 		-- 		["_"] = spec_pair("_", "_", { type = "greedy" }),
	-- 		-- 	},
	-- 		-- }
	-- 	end,
	-- })
	require("mini.comment").setup({})
	require("mini.indentscope").setup({
		options = {
			indent_at_cursor = false,
		},
	})
	vim.g.miniindentscope_disable = true
	require("mini.jump").setup({})
end

return config_mini
