local h = require("null-ls.helpers")
local methods = require("null-ls.methods")

local HOVER = methods.internal.HOVER

return h.make_builtin({
	method = HOVER,
	filetypes = vim.tbl_keys(require("nvim-ref").filetypes),
	name = "bibtex",
	generator = {
		fn = function(_, done)
			local cword = vim.fn.expand("<cword>")

			-- Strip off pandoc @ from citekey, if present:
			if string.match(cword, "^@") then
				cword = string.sub(cword, 2)
			end

			local results = require("nvim-ref.bibliography").query(cword)
			if #results > 0 and results[1].key == cword then
				done({ require("nvim-ref.format").get_markdown_documentation(results[1]) })
			else
				done()
			end
		end,
		async = true,
	},
})
