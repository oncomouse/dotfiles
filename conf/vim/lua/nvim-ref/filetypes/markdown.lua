local M = {}

function M.ref(citation)
	return "@" .. citation.key
end

function M.citation(citation)
	return {
		before = "[@" .. citation.key .. ", ",
		after = "]",
	}
end

local yaml = {
}

-- This is based on vim-pandoc and it works for getting bibliography keys that are strings, but not much else:
function yaml.parse(block)
	local yaml_dict = {}
	for _, line in pairs(block) do
		local _, key, val = unpack(vim.fn.matchlist(line, [==[\s*\([[:graph:]]\+\)\s*:\s*\(.*\)]==]))
		key = vim.fn.substitute(key, '[ -]', '_', 'g')
		val = vim.fn.substitute(val, [[\(^"\|"$\)]], '', 'g')
		yaml_dict[key] = val
	end

	return yaml_dict
end

function M.find_bibliography(bufnum)
	bufnum = bufnum or 0
	local lines = require("nvim-ref.utils.file").get_buffer_lines(bufnum)
	local yaml_blocks = {}
	local in_block = false
	for _, line in pairs(lines) do
		if string.match(line, "^---") then
			if not in_block then
				table.insert(yaml_blocks, {})
			end
			in_block = not in_block
		elseif in_block and string.match(line, "^%.%.%.") then
			in_block = not in_block
		else
			if in_block then
				table.insert(yaml_blocks[#yaml_blocks], line)
			end
		end
	end
	local yaml_objs = {}
	for _, block in pairs(yaml_blocks) do
		table.insert(yaml_objs, yaml.parse(block))
	end
	local bibliographies = {}
	for _,yaml_obj in pairs(yaml_objs) do
		if yaml_obj.bibliography then
			if type(yaml_obj.bibliography) ~= "table" then
				yaml_obj.bibliography = { yaml_obj.bibliography }
			end
			for _,bib in pairs(yaml_obj.bibliography) do
				table.insert(bibliographies, bib)
			end
		end
	end
	return bibliographies
end

function M.find_start()
	return require("nvim-ref.filetypes").find_start([[@\k*\%#]])
end

function M.setup()
	require("nvim-ref.hooks").trigger("add_filetype", {
		type = "markdown",
	})
end

return require("nvim-ref.filetypes.utils").setmetatable(M)
