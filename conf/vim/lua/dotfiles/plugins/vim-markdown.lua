-- luacheck: globals vim
return {
	"plasticboy/vim-markdown",
	ft = "markdown",
	setup = function()
		vim.g.vim_markdown_frontmatter = 1 -- Format YAML
		vim.g.vim_markdown_strikethrough = 0 -- Don't format strikethrough
		vim.g.vim_markdown_conceal = 0 -- Don't conceal
		vim.g.vim_markdown_conceal_code_blocks = 0 -- Don't conceal code blocks
		vim.g.vim_markdown_math = 1 -- Do process MathJaX and LaTeX math
	end,
	requires = {
		{ -- Required for TableFormat in vim-markdown but also useful elsewhere
			"godlygeek/tabular",
			cmd = { "Tabularize" },
		},
	},
}
