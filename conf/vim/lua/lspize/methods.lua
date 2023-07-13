local M = {}

local lsp_methods = {
	INITIALIZE = "initialize",
	SHUTDOWN = "shutdown",
	EXIT = "exit",
	CODE_ACTION = "textDocument/codeAction",
	EXECUTE_COMMAND = "workspace/executeCommand",
	PUBLISH_DIAGNOSTICS = "textDocument/publishDiagnostics",
	FORMATTING = "textDocument/formatting",
	RANGE_FORMATTING = "textDocument/rangeFormatting",
	DID_CHANGE = "textDocument/didChange",
	DID_OPEN = "textDocument/didOpen",
	DID_CLOSE = "textDocument/didClose",
	DID_SAVE = "textDocument/didSave",
	HOVER = "textDocument/hover",
	COMPLETION = "textDocument/completion",
	DEFINITION = "textDocument/definition",
	REFERENCES = "textDocument/references",
}
vim.tbl_add_reverse_lookup(lsp_methods)

-- extracted from Neovim's lsp.lua
local request_name_to_capability = {
	["textDocument/hover"] = { "hoverProvider" },
	["textDocument/signatureHelp"] = { "signatureHelpProvider" },
	["textDocument/definition"] = { "definitionProvider" },
	["textDocument/implementation"] = { "implementationProvider" },
	["textDocument/declaration"] = { "declarationProvider" },
	["textDocument/typeDefinition"] = { "typeDefinitionProvider" },
	["textDocument/documentSymbol"] = { "documentSymbolProvider" },
	["textDocument/prepareCallHierarchy"] = { "callHierarchyProvider" },
	["callHierarchy/incomingCalls"] = { "callHierarchyProvider" },
	["callHierarchy/outgoingCalls"] = { "callHierarchyProvider" },
	["textDocument/rename"] = { "renameProvider" },
	["textDocument/prepareRename"] = { "renameProvider", "prepareProvider" },
	["textDocument/codeAction"] = { "codeActionProvider" },
	["textDocument/codeLens"] = { "codeLensProvider" },
	["codeLens/resolve"] = { "codeLensProvider", "resolveProvider" },
	["codeAction/resolve"] = { "codeActionProvider", "resolveProvider" },
	["workspace/executeCommand"] = { "executeCommandProvider" },
	["workspace/symbol"] = { "workspaceSymbolProvider" },
	["textDocument/references"] = { "referencesProvider" },
	["textDocument/rangeFormatting"] = { "documentRangeFormattingProvider" },
	["textDocument/formatting"] = { "documentFormattingProvider" },
	["textDocument/completion"] = { "completionProvider" },
	["textDocument/documentHighlight"] = { "documentHighlightProvider" },
	["textDocument/semanticTokens/full"] = { "semanticTokensProvider" },
	["textDocument/semanticTokens/full/delta"] = { "semanticTokensProvider" },
	["textDocument/inlayHint"] = { "inlayHintProvider" },
	["inlayHint/resolve"] = { "inlayHintProvider", "resolveProvider" },
}

M.lsp = lsp_methods
M.request_name_to_capability = request_name_to_capability

return M
