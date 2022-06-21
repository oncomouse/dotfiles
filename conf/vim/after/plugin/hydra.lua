local Hydra = require("hydra")

Hydra({
	name = "Side scroll",
	mode = "n",
	body = "z",
	heads = {
		{ "h", "5zh" },
		{ "l", "5zl", { desc = "←/→" } },
		{ "H", "zH" },
		{ "L", "zL", { desc = "half screen ←/→" } },
	},
})
Hydra({
	mode = "n",
	body = "<C-w>",
	heads = {
		-- Move focus
		{ "h", "<C-w>h" },
		{ "j", "<C-w>j" },
		{ "k", "<C-w>k" },
		{ "l", "<C-w>l", { desc = "focus ←/↓/↑/→" } },
		-- Split
		{ "s", "<C-w>s" },
		{ "S", "<C-w>v", { desc = "split —/|" } },
		{ "q", "<Cmd>try | close | catch | endtry<CR>", { desc = "close" } },
		-- Size
		{ "+", "<C-w>+" },
		{ "-", "<C-w>-", { desc = "height +/-" } },
		{ ">", "2<C-w>>" },
		{ "<", "2<C-w><", { desc = "width +/-" } },
		{ "=", "<C-w>=", { desc = "equalize" } },
		--
		{ "b", "<Cmd>Buffers<CR>", { exit = true, desc = "choose buffer" } },
		{ "<Esc>", nil, { exit = true } },
	},
})
