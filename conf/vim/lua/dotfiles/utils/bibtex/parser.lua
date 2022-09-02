local lpeg = require("lpeg")

local C = lpeg.C
local Ct = lpeg.Ct
local Cg = lpeg.Cg
local Cc = lpeg.Cc
local R = lpeg.R
local P = lpeg.P
local S = lpeg.S
local V = lpeg.V

local ident = -R("09") * (-S(" \t\"#%'(),={}") * R("\32\127"))^1
local ws = S(" \t\n\r")^0
local not_nl = 1 - S("\n\r")
local not_brace = 1 - S("{}")
local bib_parser = P({
	"bib_db",
	bib_db = ws * Ct(V("node")^0) * ws,
	-- This way of handling node and comment is based on astrocite: https://github.com/dsifford/astrocite/blob/9010483f0d265bc9d5ca701b0c5724d58907b182/packages/astrocite-bibtex/src/grammar.pegjs#L88-L116
	node = Ct(V("comment") + V("string") + V("preamble") + V("entry")),
	comment = Cg(Cc("comment"), "type") * ((Cg(Cc("braced_comment"), "comment_type") * P("@comment") * ws * Cg(C(V("braced_comment")) / function(x) return x:sub(2, -2) end, "contents")) +
			(Cg(Cc("line_comment"), "comment_type") * P("@comment") * ws * Cg(C(not_nl^0), "contents") * S("\r\n")^0) +
			Cg(Cc("non_entry_text"), "comment_type") * Cg(C((1-S("@")) * not_nl^0), "contents") * S("\r\n")^0),
	braced_comment = P("{") * (not_brace + V("braced_comment"))^0 * P("}"),
	preamble = Cg(Cc("preamble"), "type") * P("@preamble") * ws * ( P("{") * ws * V("preamble_body") * ws * P("}") + P("(") * ws * V("preamble_body") * ws * P(")") ) * ws,
	preamble_body = V("value"),
	string = Cg(Cc("string"), "type") * P("@string") * ws * S("{(") * ws * Cg(Ct(V("string_body")), "contents") * ws * S(")}") * ws,
	string_body = Cg(C(ident), "key") * ws * P("=") * ws * Cg(C(V("value")), "value"),
	entry = Cg(Cc("entry"), "type") * P("@") * Cg(C(ident), "kind") * ws * (P("{") * ws * Cg(C(V("key")), "key") * ws * Cg(Ct(V("entry_body")^-1), "contents") * ws * P("}") +
			P("(") * ws * C(V("key_paren")) * ws * C(V("entry_body")^-1) * ws * P(")")) * ws,
	key = (1-S(", \t}\n"))^0,
	key_paren = (1-S(", \t\n"))^0,
	entry_body = (P(",") * ws * Ct(Cg(C(ident), "key") * ws * P("=") * ws * Cg(C(V("value")), "value")) * ws)^0 * P(",")^-1,
	value = V("piece") * (ws * P("#") * ws * V("piece"))^0,
	piece
	 = R("09")^1 +
		P("{") * V("balanced")^0 * P("}") +
		P('"') * (-P('"') * V("balanced"))^0 * P('"') +
		ident,
	balanced
	 = P("{") * V("balanced")^0 * P("}") +
		not_brace,
})

return bib_parser
