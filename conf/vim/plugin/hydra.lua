local ok, Hydra = pcall(require, "hydra")
if ok then
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
		name = "Resize windows",
		mode = "n",
		body = "<C-w>",
		heads = {
			{ "+", "<C-w>+" },
			{ "-", "<C-w>-", { desc = "height +/-" } },
			{ ">", "2<C-w>>" },
			{ "<", "2<C-w><", { desc = "width +/-" } },
			{ "=", "<C-w>=", { desc = "equalize" } },
		},
	})
end
