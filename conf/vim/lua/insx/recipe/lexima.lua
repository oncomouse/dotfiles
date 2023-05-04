local function get_syntax_link_chain(l, c)
	local synname = vim.fn.synIDattr(vim.fn.synID(l, c, 1), "name")
	local result_stack = {}
	if synname == "" and c > 1 then
		synname = vim.fn.synIDattr(vim.fn.synID(l, c - 1, 1), "name")
	end
	while true do
		if synname == "" then
			break
		end
		table.insert(result_stack, synname)
		local hl_group = vim.api.nvim_get_hl(0, { name = synname })
		synname = hl_group.link or ""
	end
	return result_stack
end
local function calc_indent_depth(lnum)
	local indent_depth = 0
	if vim.opt.indentexpr:get() == "" then
		if vim.opt.smartindent:get() or vim.opt.cindent:get() then
			indent_depth = vim.fn.cindent(lnum)
		elseif vim.opt.autoindent:get() then
			indent_depth = vim.fn.indent(lnum - 1)
		end
	else
		vim.v.lnum = lnum
		indent_depth = vim.api.nvim_cmd(vim.api.nvim_parse_cmd("echo " .. vim.opt.indentexpr:get(), {}), {
			output = true,
		})
	end
	return indent_depth
end
local function get_indent_chars(indent_depth)
	if vim.opt.expandtab:get() then
		return vim.fn["repeat"](" ", indent_depth)
	else
		return vim.fn["repeat"]("\t", math.floor(indent_depth / vim.opt.tabstop:get()))
			.. vim.fn["repeat"](" ", indent_depth % vim.opt.tabstop:get())
	end
end

local function calculate_input_length(input)
	return #string.gsub(input, "<[^>]+>", ".")
end

local priority = 0
return function(rule)
	priority = priority + 1
	return {
		priority = priority,
		action = function(ctx)
			local output = ""
			local move = {
				right = 0,
				left = 0,
				up = 0,
				down = 0,
			}

			local base_string, pattern
			if rule.with_submatch then
				local at = type(rule.at) == "function" and rule.at(ctx) or rule.at
				local at_start_pos = ctx.search(at)
				at_start_pos[1] = at_start_pos[1] + 1
				at_start_pos[2] = at_start_pos[2] + 1
				local search_limit = vim.fn.max({ 0, ctx:row() - 20 })
				local at_end_pos = vim.fn.searchpos(at, "bcWne", search_limit)
				if at_end_pos[1] == 0 and at_end_pos[2] == 0 then
					at_end_pos = vim.fn.searchpos(at, "cWne", search_limit)
				end
				local context = vim.fn
					.join(vim.fn.getline(at_start_pos[1], at_end_pos[1]), "\n")
					:sub(at_start_pos[2] - 1, at_end_pos[2])
				pattern = vim.fn.substitute(at, "\\\\%#", "", "")
				base_string = vim.fn.matchstr(context, pattern)
			end
			if rule.input then
				local input = type(rule.input) == "function" and rule.input(ctx) or rule.input
				if base_string then
					input = vim.fn.substitute(base_string, pattern, input, "")
				end

				output = output .. input
			elseif not rule.leave then
				output = rule.char
			end

			if rule.delete then
				local delete = type(rule.delete) == "string" and string.find(ctx.after(), rule.delete) or rule.delete
				output = output .. vim.fn["repeat"]("<Del>", delete)
			end

			local after_lines = {}
			if rule.input_after then
				local input_after = type(rule.input_after) == "function" and rule.input_after(ctx) or rule.input_after
				if base_string then
					input_after = vim.fn.substitute(base_string, pattern, rule.input_after, "")
				end
				output = output .. input_after
				local newlines = vim.fn.count(input_after, "<cr>", true)
				if newlines > 0 then
					after_lines = vim.fn.split(input_after, [[<cr>\c]], true)
					move.left = move.left + calculate_input_length(after_lines[#after_lines])
					move.up = move.up + newlines
				else
					move.left = move.left + calculate_input_length(input_after)
				end
			end

			-- Leave needs to happen right away:
			if rule.leave then
				local leave = type(rule.leave) == "string" and string.find(ctx.after(), rule.leave) or rule.leave
				local row, col = ctx.row(), ctx.col()
				ctx.move(row, col + leave)
			end

			if #output > 0 then
				ctx.send(output)
			end
			local row, col = ctx.row(), ctx.col()
			ctx.move(row + move.down - move.up, col + move.right - move.left)
			-- Handle indent for endwise and multiply lines:
			if #after_lines > 0 then
				-- Save post-move position:
				row, col = ctx.row(), ctx.col()
				-- Calculate indentation:
				for i = 0, #after_lines - 1, 1 do
					local line = vim.api.nvim_buf_get_lines(0, row + i - 1, row + i, false)[1]
					local indent_depth = calc_indent_depth(row + i)
					local indent = get_indent_chars(indent_depth)
					vim.api.nvim_buf_set_lines(0, row + i - 1, row + i, false, { indent .. line })
				end
				-- The indent function sometimes mess withthe outcome of our move:
				ctx.move(row, col)
				ctx.send("<End>")
			end
		end,
		enabled = function(ctx)
			if rule.filetype then
				local ft = type(rule.filetype) == "table" and rule.filetype or { rule.filetype }
				if not vim.tbl_contains(ft, ctx.filetype) then
					return false
				end
			end

			if rule.syntax then
				local syntax = type(rule.syntax) == "table" and rule.syntax or { rule.syntax }
				if #rule.syntax > 0 then
					local syntax_chain = get_syntax_link_chain(ctx.row(), ctx.col())
					local found = false
					for _, syn in pairs(syntax) do
						if vim.tbl_contains(syntax_chain, syn) then
							found = true
						end
					end
					if not found then
						return false
					end
				end
			end

			if rule.at then
				local at = type(rule.at) == "function" and rule.at(ctx) or rule.at
				if not ctx.match(at) then
					return false
				end
			end

			if rule.except then
				local except = type(rule.except) == "function" and rule.except(ctx) or rule.except
				if ctx.match(except) then
					return false
				end
			end

			return true
		end,
	}
end
