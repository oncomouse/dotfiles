return {
	"creativenull/efmls-configs-nvim",
	opts = function()
		local flake8 = require("efmls-configs.linters.flake8")
		local htmlhint = {
			prefix = "htmlhint",
			lintSource = require("efmls-configs.utils").sourceText("htmlhint"),
			lintCommand = "htmlhint -f unix stdin",
			lintStdin = true,
			lintFormat = "stdin:%l:%c: %m",
		}
		local selene = require("efmls-configs.linters.selene")
		local shellcheck = require("efmls-configs.linters.shellcheck")
		local vint = require("efmls-configs.linters.vint")
		local languages = {
			html = { htmlhint },
			lua = { selene },
			python = { flake8 },
			sh = { shellcheck },
			vim = { vint },
		}

		return {
			filetypes = vim.tbl_keys(languages),
			settings = {
				rootMarkers = {
					".git/",
				},
				languages = languages,
			},
			init_options = {
				documentFormatting = true,
				documentRangeFormatting = true,
			},
		}
	end,
	config = function(_, opts)
		local augroup = vim.api.nvim_create_augroup("dotfiles-efm-settings", { clear = true })
		vim.api.nvim_create_autocmd("FileType", {
			group = augroup,
			pattern = vim.fn.join(opts.filetypes, ","),
			callback = function()
				require("dotfiles.lsp").start_server("efm", opts)
			end,
		})
	end,
}
