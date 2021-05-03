--luacheck: globals vim
local configs = require 'lspconfig/configs'
local util = require 'lspconfig/util'

local name = 'citation_lsp'

configs[name] = {
  default_config = {
	filetypes = {'markdown', 'pandoc'};
	root_dir = function(fname)
	  return util.root_pattern("Rakefile") or util.find_git_ancestor(fname) or util.path.dirname(fname)
	end;
	log_level = vim.lsp.protocol.MessageType.Warning;
	cmd = {'/usr/bin/env', 'citation-langserver'};
	-- cmd = {'env', 'PYTHONPATH=~/Projects/citation-langserver', 'python3', '-m', 'citation_langserver'}
  }
}
