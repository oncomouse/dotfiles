local Lsp = require("lspize")

local handlers = {
	[Lsp.methods.HOVER] = function(_, done)
		local cword = vim.fn.expand("<cword>")
		local send_definition = function(def)
			done(nil, { contents = cword .. ": " .. def })
		end

		require("plenary.curl").request({
			url = "https://api.dictionaryapi.dev/api/v2/entries/en/" .. cword,
			method = "get",
			callback = vim.schedule_wrap(function(data)
				if not (data and data.body) then
					send_definition("no definition available")
					return
				end

				local ok, decoded = pcall(vim.json.decode, data.body)
				if not ok or not (decoded and decoded[1]) then
					send_definition("no definition available")
					return
				end

				send_definition(decoded[1].meanings[1].definitions[1].definition)
			end),
		})
	end,
}

Lsp.create(handlers, {
	filetype = { "org", "markdown", "text" },
	name = "hover_definition",
	on_attach = require("dotfiles.plugins.lsp.on_attach"),
})
