local get_node_text = vim.treesitter.get_node_text

local function replace_node_text(node, replacement)
	local srow, scol, erow, ecol = vim.treesitter.get_node_range(node)
	vim.api.nvim_buf_set_text(0, srow, scol, erow, ecol, { replacement })
end

local function get_image_html(row, root)
	local query = vim.treesitter.query.get("html", "images")
	if not query then
		return
	end
	local node
	local alt
	local src
	for id, _node in query:iter_captures(root, 0, row, row + 1) do
		local cap = query.captures[id]
		if cap == "image.html" then
			node = _node
		elseif cap == "image.html.attr" then
			local name, value
			for child in _node:iter_children() do
				local text = get_node_text(child, 0)
				local type = child:type()
				if type == "attribute_name" then
					name = text
				elseif type == "attribute_value" then
					value = text
				elseif type == "quoted_attribute_value" then
					value = text:gsub("^['|\"](.*)['|\"]$", "%1")
				end
			end
			if name == "alt" then
				alt = value
			elseif name == "src" then
				src = value
			end
		end
	end
	return node, alt, src
end

local function get_image_md(row, root)
	local query = vim.treesitter.query.get("markdown_inline", "images")
	if not query then
		return
	end
	local node
	local alt
	local src
	for id, _node in query:iter_captures(root, 0, row, row + 1) do
		local cap = query.captures[id]
		if cap == "image.markdown" then
			node = _node
		elseif cap == "image.markdown.alt" then
			alt = get_node_text(_node, 0)
		elseif cap == "image.markdown.src" then
			src = get_node_text(_node, 0)
		end
	end
	return node, alt, src
end

local function html_to_md(row, root)
	local node, alt, src = get_image_html(row, root)
	if node then
		replace_node_text(node, ("![%s](%s)"):format(alt, src))
		return true
	end
	return false
end

local function md_to_html(row, root)
	local node, alt, src = get_image_md(row, root)
	if node then
		replace_node_text(node, ('<img alt="%s" src="%s">'):format(alt, src))
		return true
	end
	return false
end

local function toggle_markdown_image()
	local ts_utils = require("nvim-treesitter.ts_utils")
	local row, col = unpack(vim.api.nvim_win_get_cursor(0))
	local root, _, langtree = ts_utils.get_root_for_position(row - 1, col)
	if not langtree then return nil end
	local lang = langtree:lang()
	if lang == "html" then
		return html_to_md(row - 1, root)
	elseif lang == "markdown_inline" then
		return md_to_html(row - 1, root)
	end
end

return {
	"monaqa/dial.nvim",
	keys = {
		{ "<C-a>", "<Plug>(dial-increment)", mode = "n" },
		{ "<C-x>", "<Plug>(dial-decrement)", mode = "n" },
		{ "<C-a>", "<Plug>(dial-increment)", mode = "v" },
		{ "<C-x>", "<Plug>(dial-decrement)", mode = "v" },
		{ "g<C-a>", "g<Plug>(dial-increment)", mode = "v" },
		{ "g<C-x>", "g<Plug>(dial-decrement)", mode = "v" },
	},
	config = function()
		local augend = require("dial.augend")
		require("dial.config").augends:register_group({
			-- default augends used when no group name is specified
			default = {
				augend.integer.alias.decimal, -- nonnegative decimal number (0, 1, 2, 3, ...)
				augend.integer.alias.hex, -- nonnegative hex number  (0x01, 0x1a1f, etc.)
				augend.constant.alias.bool, -- boolean value (true <-> false)
				augend.date.alias["%Y/%m/%d"], -- date (2022/02/18, etc.)
				augend.date.alias["%m/%d/%Y"], -- date (02/19/2022)
				-- augend.date.alias["%m-%d-%Y"], -- date (02-19-2022)
				-- augend.date.alias["%Y-%m-%d"], -- date (02-19-2022)
				augend.date.new({
					pattern = "%m.%d.%Y",
					default_kind = "day",
					only_valid = true,
					word = false,
				}),
				augend.misc.alias.markdown_header,
				augend.user.new({
					find = function()
						return toggle_markdown_image() and { from = 0, to = 0} or nil
					end,
					add = function(text, _, cursor)
						return { text = text, cursor = cursor }
					end
				})
			},
		})
	end,
}
