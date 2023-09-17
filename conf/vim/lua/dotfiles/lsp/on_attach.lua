local features = {
	codeLensProvider = {
		function(_, bufnr)
			vim.api.nvim_create_autocmd({ "CursorHold", "CursorHoldI", "InsertLeave" }, {
				buffer = bufnr,
				group = vim.api.nvim_create_augroup("dotfiles-lsp-codelens", {}),
				callback = vim.lsp.codelens.refresh,
				desc = "lua vim.lsp.codelens.refresh()",
			})
		end,
		keys = {
			{ "<leader>gl", vim.lsp.codelens.run, desc = "lua vim.lsp.codelens.run()" },
		},
	},
	-- Use C+x C+o for completion:
	completionProvider = {
		settings = {
			omnifunc = "v:lua.vim.lsp.omnifunc",
		},
	},
	-- Use C+x C+] for tags:
	definitionProvider = {
		settings = {
			tagfunc = "v:lua.vim.lsp.tagfunc",
		},
		keys = {
			{ "<leader>gd", vim.lsp.buf.definition, desc = "lua vim.lsp.buf.definition()" },
		},
	},
	hoverProvider = {
		function(_, bufnr)
			vim.api.nvim_buf_create_user_command(bufnr, "Hover", vim.lsp.buf.hover, {
				desc = "lua vim.lsp.buf.hover()",
				force = true,
				nargs = "+",
			})
		end,
		settings = {
			keywordprg = ":Hover",
		},
	},
	documentSymbolProvider = {
		keys = {
			{ "<leader>gs", vim.lsp.buf.document_symbol, desc = "lua vim.lsp.buf.document_symbol()" },
		},
	},
	renameProvider = {
		keys = {
			{ "<F2>", vim.lsp.buf.rename, desc = "lua vim.lsp.buf.rename()" },
		},
	},
	declarationProvider = {
		keys = {
			{ "<leader>gD", vim.lsp.buf.declaration, desc = "lua vim.lsp.buf.declaration()" },
		},
	},
	typeDefinitionProvider = {
		keys = {
			{ "<leader>gy", vim.lsp.buf.type_definition, desc = "lua vim.lsp.buf.type_definition()" },
		},
	},
	implementationProvider = {
		keys = {
			{ "<leader>gi", vim.lsp.buf.implementation, desc = "lua vim.lsp.buf.implementation()" },
		},
	},
	referencesProvider = {
		keys = {
			{ "<leader>gr", vim.lsp.buf.references, desc = "lua vim.lsp.buf.references()" },
		},
	},
	codeActionProvider = {
		keys = {
			{ "<leader>ga", vim.lsp.buf.code_action, desc = "lua vim.lsp.buf.code_action()" },
			{
				"v",
				"<leader>ga",
				function()
					local _, s_row, s_col, _ = unpack(vim.fn.getpos("v"))
					local _, e_row, e_col, _ = unpack(vim.fn.getcurpos())
					if vim.fn.mode() == "V" then
						s_col = 1
						e_col = #(vim.fn.getline(e_row))
					end
					vim.lsp.buf.code_action({
						range = {
							start = { s_row, s_col },
							["end"] = { e_row, e_col },
						},
					})
				end,
				desc = "'<,'>lua vim.lsp.buf.code_action()",
			},
		},
	},
	signatureHelpProvider = {
		keys = {
			{ "<leader>k", vim.lsp.buf.signature_help, desc = "lua vim.lsp.buf.signature_help()" },
		},
	},
	documentFormattingProvider = {
		function()
			vim.b.dotfiles_lsp_can_format = true
		end,
	},
}

local function on_attach(client, bufnr)
	for feature, _ in pairs(client.server_capabilities) do
		if type(features[feature]) == "function" then
			features[feature](client, bufnr)
		elseif type(features[feature]) == "table" then
			if type(features[feature][1]) == "function" then
				features[feature][1](client, bufnr)
			end
			for _, map in pairs(features[feature].keys or {}) do
				local mode, lhs, rhs
				if #map == 3 then
					mode = map[1]
					lhs = map[2]
					rhs = map[3]
				else
					mode = map.mode or "n"
					lhs = map[1]
					rhs = map[2]
				end
				local desc = map.desc or nil
				vim.keymap.set(mode, lhs, rhs, {
					buffer = bufnr,
					desc = desc,
				})
			end
			for setting, value in pairs(features[feature].settings or {}) do
				vim.opt_local[setting] = value
			end
		end
	end
	-- Turn off LSP semantic highlighting:
	client.server_capabilities.semanticTokensProvider = nil
end

return on_attach
