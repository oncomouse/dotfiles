--luacheck: globals vim dotfiles
dotfiles = _G.dotfiles or {}
return require("packer").startup({
	function(use)
		use {
			{ "wbthomason/packer.nvim", opt = true },
			{ "dstein64/vim-startuptime", cmd = "StartupTime" },
			"tpope/vim-sensible",
			"xero/securemodelines",
			"sickill/vim-pasta", -- fix block paste for Neovim
			"tpope/vim-commentary", -- gc<motion> to (un)comment
			"tpope/vim-repeat",
			"oncomouse/vim-surround", -- ys to add, cs to change, ds to delete. f, F for function, t, T for tag
			require("dotfiles.plugins.vim-rooter"), -- Set CWD for projects
			"wellle/targets.vim", -- add next block n]) targets, plus words in commas (a,), asterisks (a*), etc
			require("dotfiles.plugins.lexima-vim"), -- Autopairs + Endwise
			require("dotfiles.plugins.nvim-colorizer-lua"),
			require("dotfiles.plugins.fzf-vim"), -- Add shorcuts for FZF
			require("dotfiles.plugins.gina-vim"), -- Git support
			require("dotfiles.plugins.vim-vsnip"), -- Snippets
			require("dotfiles.plugins.lspconfig"), -- LSP
			require("dotfiles.plugins.nvim-treesitter"), -- Treesitter-based Syntax
			-- Non-Treesitter Syntax:
			require("dotfiles.plugins.vim-markdown"), -- Markdown Syntax
		}
	end,
})
