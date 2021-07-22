local ts_types = {
	"bash",
	"beancount",
	"bibtex",
	"c",
	"c_sharp",
	"clojure",
	"comment",
	"commonlisp",
	"cpp",
	"css",
	"dart",
	"devicetree",
	"dockerfile",
	"elixir",
	"erlang",
	"fennel",
	"fish",
	"gdscript",
	"glimmer",
	"go",
	"gomod",
	"graphql",
	"html",
	"java",
	"javascript",
	"javascriptreact",
	"jsdoc",
	"json",
	"jsonc",
	"julia",
	"kotlin",
	"latex",
	"ledger",
	"lua",
	"nix",
	"ocaml",
	"ocaml_interface",
	"ocamllex",
	"php",
	"python",
	"ql",
	"query",
	"r",
	"regex",
	"rst",
	"ruby",
	"rust",
	"scss",
	"sparql",
	"supercollider",
	"svelte",
	"teal",
	"toml",
	"tsx",
	"turtle",
	"typescript",
	"verilog",
	"vue",
	"yaml",
	"zig",
}

-- For reasons I don't understand, if this is in the packer config, it breaks FastFold
function ts_type_autocmds()
	vim.cmd[[
	set foldexpr=nvim_treesitter#foldexpr()
	]]
	for _,v in pairs(ts_types) do
		vim.cmd("autocmd FileType " .. v .. " setlocal foldmethod=expr")
	end
end

return { ts_types = ts_types, ts_type_autocmds = ts_type_autocmds }
