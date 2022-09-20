local ok, indent_blankline = pcall(require, "indent_blankline")
if ok then
	indent_blankline.setup({
		show_end_of_line = true,
		space_char_blankline = " ",
		show_current_context = true,
	})
end
