return {
	s("dc", {
		f(function()
			return string.format(string.gsub(vim.bo.commentstring, "%%s", " %%s"), os.date())
		end, {}),
	}),
	postfix(".br", {
		c(1, {
			f(function(_, parent)
				return "[" .. parent.env.POSTFIX_MATCH .. "]"
			end),
			f(function(_, parent)
				return "{" .. parent.env.POSTFIX_MATCH .. "}"
			end),
		}),
	}),
	postfix(".qt", {
		c(1, {
			f(function(_, parent)
				return '"' .. parent.env.POSTFIX_MATCH .. '"'
			end),
			f(function(_, parent)
				return "'" .. parent.env.POSTFIX_MATCH .. "'"
			end),
		}),
	}),
	postfix(".pr", {
		f(function(_, parent)
			return "(" .. parent.env.POSTFIX_MATCH .. ")"
		end),
	}),
}
