--luacheck: globals vim
local configs = require 'lspconfig/configs'
local util = require 'lspconfig/util'

local name = "emmy_lua"

configs[name] = {
  default_config = {
    filetypes = {'lua'};
    root_dir = function(fname)
      return util.find_git_ancestor(fname) or util.path.dirname(fname)
    end;
    log_level = vim.lsp.protocol.MessageType.Warning;
	cmd = {'/usr/bin/java', '-cp', os.getenv('HOME')..'/dotfiles/scripts/lsp/EmmyLua-LS-all.jar', 'com.tang.vscode.MainKt'};
  }
}
