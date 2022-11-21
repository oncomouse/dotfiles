local ok, leap = pcall(require, "leap")
if ok then
	require("leap").add_default_mappings()
	require("leap-spooky").setup({
		affixes = {},
	})
	-- require("flit").setup() -- Flit isn't quite ready for primetime

	-- leap-spooky working with arbitrary textobjects!
	local H = {}
	H.cache = {}

	-- Copied from leap-spooky.nvim
	function H.v_exit()
		local mode = vim.fn.mode(1)
		if mode:match("o") then
			return ""
		end
		-- v/V/<C-v> exits the corresponding Visual mode if already in it.
		return mode:sub(1, 1)
	end
	function H.get_motion_force()
		local force = ""
		local mode = vim.fn.mode(1)
		if mode:sub(2) == "oV" then
			force = "V"
		elseif mode:sub(2) == "o" then
			force = ""
		end
		return force
	end
	-- End copied form leap-spooky.nvim

	-- Copied from mini.nvim/ai
	function H.echo(msg, is_important)
		-- Construct message chunks
		msg = type(msg) == "string" and { { msg } } or msg
		table.insert(msg, 1, { "(mini.ai) ", "WarningMsg" })

		-- Avoid hit-enter-prompt
		local max_width = vim.o.columns * math.max(vim.o.cmdheight - 1, 0) + vim.v.echospace
		local chunks, tot_width = {}, 0
		for _, ch in ipairs(msg) do
			local new_ch = { vim.fn.strcharpart(ch[1], 0, max_width - tot_width), ch[2] }
			table.insert(chunks, new_ch)
			tot_width = tot_width + vim.fn.strdisplaywidth(new_ch[1])
			if tot_width >= max_width then
				break
			end
		end

		-- Echo. Force redraw to ensure that it is effective (`:h echo-redraw`)
		vim.cmd([[echo '' | redraw]])
		vim.api.nvim_echo(chunks, is_important, {})
	end
	function H.unecho()
		if H.cache.msg_shown then
			vim.cmd([[echo '' | redraw]])
		end
	end
	function H.user_textobject_id(ai_type)
		-- Get from user single character textobject identifier
		local needs_help_msg = true
		vim.defer_fn(function()
			if not needs_help_msg then
				return
			end

			local msg =
				string.format("Enter `%s` textobject identifier (single character) ", ai_type)
			H.echo(msg)
			H.cache.msg_shown = true
		end, 1000)
		local char_ok, char = pcall(vim.fn.getcharstr)
		needs_help_msg = false
		H.unecho()

		-- Terminate if couldn't get input (like with <C-c>) or it is `<Esc>`
		if not char_ok or char == "\27" then
			return nil
		end

		if char:find("^[%w%p%s]$") == nil then
			H.message("Input must be single character: alphanumeric, punctuation, or space.")
			return nil
		end

		return char
	end
	-- End copied from mini.nvim/ai

	local ai_types = { "a", "i" }
	local leap_types = { "r", "m", "R", "M" }
	local modes = { "x", "o" }

	for _, ai_type in pairs(ai_types) do
		for _, mode in pairs(modes) do
			for _, leap_type in pairs(leap_types) do
				vim.keymap.set(mode, string.format("%s%s", ai_type, leap_type), function()
					local target_windows = nil
					local keeppos = leap_type == "r" or leap_type == "R"
					local target_curwin = leap_type == "r" or leap_type == "m"
					if target_curwin then
						target_windows = { vim.fn.win_getid() }
					else
						target_windows = require("leap.util").get_enterable_windows()
					end
					local yank_paste = (
						false
						and keeppos
						and vim.v.operator == "y"
						and vim.v.register == '"'
					)
					local to = H.user_textobject_id(ai_type)
					if to == nil then
						return
					end
					require("leap").leap({
						action = require("leap-spooky").spooky_action(function()
							return H.v_exit()
								.. "v"
								.. vim.v.count1
								.. string.format("%s%s", ai_type, to)
								.. H.get_motion_force()
						end, {
							keeppos = keeppos,
							on_return = yank_paste and "p",
						}),
						target_windows = target_windows,
					})
				end)
			end
		end
	end
end
