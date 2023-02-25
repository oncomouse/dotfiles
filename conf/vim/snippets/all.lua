local postfix = require("luasnip.extras.postfix").postfix
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
		l("(" .. l.POST_FIX_MATCH .. ")"),
	}),
	postfix(".fn", {
		d(1, function(_, parent)
			return sn(nil, {
				i(1),
				t("(" .. parent.env.POSTFIX_MATCH .. ")"),
			})
		end),
	}),
}
