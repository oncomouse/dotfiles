return {
	"oncomouse/vim-markdown",
	ft = "markdown",
	init = function()
		vim.g.vim_markdown_frontmatter = 1 -- Format YAML
		vim.g.vim_markdown_strikethrough = 1 -- Don't format strikethrough
		vim.g.vim_markdown_conceal = 0 -- Don't conceal
		vim.g.vim_markdown_conceal_code_blocks = 0 -- Don't conceal code blocks
		vim.g.vim_markdown_math = 1 -- Do process MathJaX and LaTeX math
		vim.g.vim_markdown_auto_insert_bullets = 0 -- autoList handles bullet insertion now
		vim.g.vim_markdown_new_list_item_indent = 0 -- autoList handles spacing for lists
	end,
} -- Markdown Syntax
