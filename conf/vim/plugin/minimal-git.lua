-- luacheck: globals vim
_G.minimal_git = {}
function _G.minimal_git.git_commit()
	vim.ui.input({
		prompt = "Commit message: ",
	}, function(input)
		if not input == nil then
			vim.fn.jobstart({'git', 'commit', '-m', input}, {
				on_stdout = function(stdout) print(stdout) end,
				on_stderr = function(stderr) error(stderr) end,
			})
		end
	end)
end

vim.cmd("command GitCommit lua minimal_git.git_commit()")
