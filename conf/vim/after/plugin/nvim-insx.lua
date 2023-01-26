local has_insx, insx = pcall(require, "insx")
if has_insx then
	require("insx.preset.standard").setup()
	local esc = require("insx.helper.regex").esc
	local endwise = require("insx.recipe.endwise")
	insx.add(
		"<CR>",
		endwise.recipe({
			["lua"] = {

				endwise.simple([[^\s*if\>.*then\%(.*[^.:@$]\<end\>\)\@!.*]], "end"),
				endwise.simple([[^\s*\%(for\|while\)\>.*do\%(.*[^.:@$]\<end\>\)\@!.*]], "end"),
				endwise.simple([[^\s*\%(local\)\=.*function\>\%(.*[^.:@$]\<end\>\)\@!.*]], "end"),
			},

			["ruby"] = {
				endwise.simple(
					[[^\s*\%(module\|def\|class\|if\|unless\|for\|while\|until\|case\)\>\%(.*[^.:@$]\<end\>\)\@!.*]],
					"end"
				),
				endwise.simple([[^\s*\%(begin\)\s*]], "end"),
				endwise.simple([[\%(^\s*#.*\)\@<!do\%(\s*|.*|\)\?\s*]], "end"),
			},

			["elixir"] = {
				endwise.simple([[\%(^\s*#.*\)\@<!do\s*]], "end"),
			},

			["sh"] = {
				endwise.simple([[^\s*if\>.*]], "fi"),
				endwise.simple([[^\s*case\>.*]], "esac"),
				endwise.simple([[\%(^\s*#.*\)\@<!do\>.*]], "done"),
			},
			["zsh"] = {
				endwise.simple([[^\s*if\>.*]], "fi"),
				endwise.simple([[^\s*case\>.*]], "esac"),
				endwise.simple([[\%(^\s*#.*\)\@<!do\>.*]], "done"),
			},

			["julia"] = {
				endwise.simple(
					[[\%(^\s*#.*\)\@<!\<\%(module\|struct\|function\|if\|for\|while\|do\|let\|macro\)\>\%(.*\<end\>\)\@!.*]],
					"end"
				),
				endwise.simple([[\%(^\s*#.*\)\@<!\s*\<\%(begin\|try\|quote\)\s*]], "end"),
			},
		})
	)
	local function for_ft(ft, recipe)
		return {
			action = recipe.action,
			enabled = function(ctx)
				return ctx.ft == ft and (recipe.enabled and ctx.enabled(ctx) or true)
			end,
		}
	end
	insx.add(
		">",
		for_ft("markdown", {
			action = function(ctx)
				local col = ctx.col()
				ctx.send(">")
				if col == 0 then
					ctx.send(" ")
				end
			end,
		})
	)
	insx.add(
		"<BS>",
		for_ft("markdown", {
			action = function(ctx)
				local text = ctx.text()
				local col = ctx.col()
				if text:match("^> ") and col == 2 then
					ctx.send("<BS>")
				end
				ctx.send("<BS>")
			end,
		})
	)
	insx.add(
		"<CR>",
		for_ft("markdown", {
			action = function(ctx)
				local text = ctx.text()
				if #text == 2 then
					ctx.send("<BS><BS>")
				else
					ctx.send("<CR>")
				end
				if text:match("^> ") and #text > 2 then
					ctx.send("> ")
				end
			end,
		})
	)
	for _, char in pairs({
		"*",
		"_",
	}) do
		insx.add(
			char,
			for_ft(
				"markdown",
				require("insx.recipe.jump_next")({
					jump_pat = {
						[[\%#]] .. esc(char) .. [[\zs]],
					},
				})
			)
		)
		insx.add(
			char,
			for_ft(
				"markdown",
				require("insx.recipe.auto_pair")({
					open = char,
					close = char,
				})
			)
		)
		insx.add(
			"<BS>",
			for_ft(
				"markdown",
				require("insx.recipe.delete_pair")({
					open_pat = esc(char),
					close_pat = esc(char),
				})
			)
		)
	end
end
