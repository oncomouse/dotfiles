-- luacheck: globals vim
return {
	"norcalli/nvim-colorizer.lua",
	config = function()
		if vim.opt.termguicolors:get() then
			require("colorizer").setup({
				["*"] = { names = false, RRGGBBAA = true },
				packer = { RGB = false },
				html = { names = true, RRGGBBAA = false },
				css = { css = true, RRGGBBAA = false },
				scss = { css = true, RRGGBBAA = false },
			})
		end
	end,
}
