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
		vim.g.vim_markdown_no_default_key_mappings = 1 -- Disable keymaps
	end,
	config = function()
		vim.api.nvim_create_autocmd("FileType", {
			pattern = "markdown",
			callback = function()
				vim.keymap.set({ "n", "v" }, "]]", "<Plug>Markdown_MoveToNextHeader", { buffer = true })
				vim.keymap.set({ "n", "v" }, "[[", "<Plug>Markdown_MoveToPreviousHeader", { buffer = true })
				vim.keymap.set({ "n", "v" }, "][", "<Plug>Markdown_MoveToNextSiblingHeader", { buffer = true })
				vim.keymap.set({ "n", "v" }, "[]", "<Plug>Markdown_MoveToPreviousSiblingHeader", { buffer = true })
				vim.keymap.set({ "n", "v" }, "]u", "<Plug>Markdown_MoveToParentHeader", { buffer = true })
				vim.keymap.set({ "n", "v" }, "]h", "<Plug>Markdown_MoveToCurHeader", { buffer = true })
				vim.keymap.set({ "n", "v" }, "ge", "<Plug>Markdown_EditUrlUnderCursor", { buffer = true })
			end,
		})
	end,
} -- Markdown Syntax
