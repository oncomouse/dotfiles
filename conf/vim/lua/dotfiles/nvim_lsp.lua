-- luacheck: globals vim
local lspconfig = require('lspconfig')
-- vim.lsp.set_log_level('debug')

-- Disable diagnostics
-- vim.lsp.handlers["textDocument/publishDiagnostics"] = function() end

local no_diagnostics = {
	["textDocument/publishDiagnostics"] = function() end
}

local on_attach = function(_, bufnr)
	-- Once codelens is setup:
	-- vim.api.nvim_command [[autocmd CursorHold,CursorHoldI,InsertLeave <buffer> lua vim.lsp.codelens.refresh()]]
	vim.api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")
	vim.api.nvim_buf_set_option(bufnr, "formatexpr", "v:lua.lsp_formatexpr")
end

local servers = {
	'tsserver',
	'vimls',
	'pyls',
	'solargraph' ,
	'bashls',
}
for _, lsp in ipairs(servers) do
	lspconfig[lsp].setup {
		on_attach = on_attach,
		handlers = no_diagnostics,
	}
end

lspconfig.sumneko_lua.setup{
	on_attach = on_attach,
	cmd = {'sumneko-lua-language-server'},
	handlers = no_diagnostics,
	settings = {
		Lua = {
			-- diagnostics = {
			-- 	enable = true,
			-- 	globals = {'vim'},
			-- 	disable = { 'lowercase-global' },
			-- },
			runtime = {
				version = "LuaJIT",
				path = vim.split(package.path, ";")
			},
			workspace = {
				library = {
					[vim.fn.expand("$VIMRUNTIME/lua")] = true,
					[vim.fn.expand("$VIMRUNTIME/lua/vim/lsp")] = true
				}
			},
		}
	}
}

lspconfig.citation_lsp.setup{
	on_attach = on_attach,
	handlers = no_diagnostics,
	settings = {
		citation = {
			bibliographies = {
			'~/Seadrive/My Libraries/My Library/Documents/Academic Stuff/library.bib'
			}
		}
	}
}
-- VSCode LSPs need some fake settings to work:
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities.textDocument.completion.completionItem.snippetSupport = true
local vscode_lsps = {
	'cssls',
	'html',
}
for _, lsp in ipairs(vscode_lsps) do
	lspconfig[lsp].setup {
		on_attach = on_attach,
		handlers = no_diagnostics,
		capabilities = capabilities,
	}
end
lspconfig.jsonls.setup {
	on_attach = on_attach,
	capabilities = capabilities,
	handlers = no_diagnostics,
	cmd = {'json-languageserver', '--stdio'},
}

-- setlocal formatexpr=v:lua.lsp_formatexpr
-- vim.api.nvim_buf_set_option("formatexpr", _G.lsp_formatexpr)
_G.lsp_formatexpr = function(start_line, end_line, timeout_ms)
	timeout_ms = timeout_ms or 500

	if not start_line or not end_line then
	  if vim.fn.mode() == 'i' or vim.fn.mode() == 'R' then
		-- `formatexpr` is also called when exceeding `textwidth` in insert mode
		-- fall back to internal formatting
		return 1
	  end
	  start_line = vim.v.lnum
	  end_line = start_line + vim.v.count - 1
	end

	if start_line > 0 and end_line > 0 then
	  local params = {
		textDocument = vim.lsp.util.make_text_document_params();
		range = {
		  start = { line = start_line - 1; character = 0; };
		  ["end"] = { line = end_line - 1; character = 0; };
		};
	  };
	  local client_results = vim.lsp.buf_request_sync(0, "textDocument/rangeFormatting", params, timeout_ms)

	  -- Apply the text edits from one and only one of the clients.
	  for _, response in pairs(client_results) do
		if response.result then
		  vim.lsp.util.apply_text_edits(response.result, 0)
		  return 0
		end
	  end
	end

	-- do not run builtin formatter.
	return 0
end
