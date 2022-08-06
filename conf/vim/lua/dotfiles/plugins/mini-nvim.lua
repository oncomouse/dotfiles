---@class Pair
---@public field line number
---@public field col number
local pair = {}

local function config_mini()
	-- local augroup = vim.api.nvim_create_augroup("dotfiles-mini.nvim", { clear = true })
	local spec_pair = require("mini.ai").gen_spec.pair

	---@return Pair
	local function make_point()
		local _, l, c, _ = unpack(vim.fn.getpos("."))
		return {
			line = l,
			col = c,
		}
	end


	---@param inside boolean
	---@return Pair top, Pair bottom
	local function extract_sentence(inside)
		-- Get the end of the sentence:
		vim.cmd([[normal! )]])
		local p_bottom = make_point()

		-- Get the beginning of the sentence:
		vim.cmd([[normal! (]])
		local p_top = make_point()

		local punc_matcher = "[.?!]"
		-- Try to find any punctuation at the end of our sentence:
		local target_line = p_bottom.line
		while target_line >= p_top.line and target_line > 0 do
			-- From where should we start searching?
			local start_pos = target_line == p_top.line and p_top.col or 0
			-- Get a match, if one exists:
			local match_start, match_stop = vim.regex(punc_matcher):match_line(0, target_line - 1, start_pos)
			if match_start ~= nil then
				p_bottom.col = start_pos + (inside and match_start or match_stop)
				p_bottom.line = target_line
				break
			end
			target_line = target_line - 1
		end

		return p_top, p_bottom
	end

	require("mini.ai").setup({
		custom_textobjects = {
			e = function()
				local from = { line = 1, col = 1 }
				local to = {
					line = vim.fn.line("$"),
					col = vim.fn.getline("$"):len(),
				}
				return {
					from = from,
					to = to,
				}
			end,
			s = function(type) -- Use the built-in sentence-wise motions (`(` and `)`) to capture sentence textobjects
				local from, to = extract_sentence(type == "i")

				return {
					from = from,
					to = to,
				}
			end,
			[","] = function(type)
				---@param sentence table<number, Pair> Beginning and end of sentence in col,line pairs
				---@param cursor Pair position of the cursor
				---@param forward boolean Are we searching forward or backward within the sentence?
				---@param inside boolean Are we searching inside (with an i- mapping) or around (with an a- mapping)?
				---@return Pair match A comma was found | nil A comma was not found
				local function hunt_comma(sentence, cursor, forward, inside)

					---@param line number The line currently being checked
					---@return boolean continue If searching can continue, if moving forward, keep line lower than end of sentence; if moving backward keep it higher than beginning of sentence
					local function not_done(line)
						if forward then
							return line <= sentence[2].line
						end
						return line >= sentence[1].line
					end

					---@param line number The buffer line to get from Neovim
					---@return string bufline The buffer line from Neovim
					local function get_line_contents(line)
						return vim.api.nvim_buf_get_lines(0, line - 1, line, false)[1] or ""
					end

					---@param line The current line number being scanned
					---@param contents The content of the buffer line being scanned
					---@return start_col number The column to start scanning from, depending on direction
					---@return stop_col number The column to stop scanning at, depending on direction
					local function set_start_stop(line, contents)
						local start_col, stop_col

						-- Moving forward: start at the beginning of the line end at the end, unless we're on the last line
						if forward then
							start_col = 1
							stop_col = #contents
							if line == sentence[2].line then
								stop_col = sentence[2].col
							end
						-- Moving backward: start at the end of the line and end at the beginning, unless we're on the first line
						else
							start_col = #contents
							stop_col = 1
							if line == sentence[1].line then
								stop_col = sentence[1].col
							end
						end

						-- If we're on the line with the cursor, start there
						if line == cursor.line then
							start_col = cursor.col
						end

						return start_col, stop_col
					end

					-- Start scanning at the cursor line
					local line = cursor.line

					while not_done(line) do
						local contents = get_line_contents(line)
						local start_col, stop_col = set_start_stop(line, contents)
						local col
						if forward then
							-- Find the first comma in the available search area
							col = contents:sub(start_col, stop_col):find([[,]])
							if col ~= nil then
								if inside then
									-- If the comma is at the beginning (*sad grammarian noises*), move to the end of the previous line
									if col == 1 then
										line = line - 1
										col = #get_line_contents(line)
									else
										col = col - 1
									end
								else
									if col ~= #contents and contents:sub(start_col, stop_col):sub(col + 1, col + 1):match([[ ]]) then
										col = col + 1
									end
								end
								return {
									col = (start_col - 1) + col,
									line = line,
								}
							end
							line = line + 1
						else
							local found = false
							start_col, stop_col = set_start_stop(line, contents)
							col = start_col
							while col >= stop_col do
								if contents:sub(col, col):match([[,]]) then
									found = true
									if inside then
										if col == #contents then
											line = line + 1
											col = 1
										elseif contents:sub(col + 1, col + 1):match([[ ]]) then
											col = col + 2
										else
											col = col + 1
										end
									end
									break
								end
								col = col - 1
							end
							if found then
								return {
									col = col,
									line = line,
								}
							end
							line = line - 1
						end
					end

					return nil
				end

				local cursor = make_point()
				local s_start, s_end = extract_sentence(type == "i")
				local sentence = { s_start, s_end }
				local comma_in_front = hunt_comma(sentence, cursor, false, type == "i")
				local comma_behind = hunt_comma(sentence, cursor, true, type == "i")
				return {
					to = comma_in_front == nil and s_start or comma_in_front,
					from = comma_behind == nil and s_end or comma_behind,
				}
			end,
			["*"] = spec_pair("*", "*", { type = "greedy" }),
			["_"] = spec_pair("_", "_", { type = "greedy" }),
		},
		mappings = {
			around_last = "aN",
			inside_last = "iN",
		},
	})
	-- vim.api.nvim_create_autocmd("FileType", {
	-- 	pattern = "markdown",
	-- 	group = augroup,
	-- 	callback = function()
	-- 		-- local spec_pair = require("mini.ai").gen_spec.pair
	-- 		-- vim.b.miniai_config = {
	-- 		-- 	custom_textobjects = {
	-- 		-- 		["*"] = spec_pair("*", "*", { type = "greedy" }),
	-- 		-- 		["_"] = spec_pair("_", "_", { type = "greedy" }),
	-- 		-- 	},
	-- 		-- }
	-- 	end,
	-- })
	require("mini.comment").setup({})
	require("mini.indentscope").setup({
		options = {
			indent_at_cursor = false,
		},
	})
	vim.g.miniindentscope_disable = true
	require("mini.jump").setup({})
end

return config_mini
