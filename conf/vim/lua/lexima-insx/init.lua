local M = {}

function M.add_rule(rule)
    require("insx").add(rule.char, require("insx.recipe.lexima")(rule))
end

function M.setup()
    vim.g.lexima_no_default_rules = vim.fn.get(vim.g, "lexima_no_default_rules", false)
    vim.g.lexima_no_map_to_escape = vim.fn.get(vim.g, "lexima_no_map_to_escape", false)
    vim.g.lexima_enable_basic_rules = vim.fn.get(vim.g, "lexima_enable_basic_rules", true)
    vim.g.lexima_enable_newline_rules = vim.fn.get(vim.g, "lexima_enable_newline_rules", true)
    vim.g.lexima_enable_space_rules = vim.fn.get(vim.g, "lexima_enable_space_rules", true)
    vim.g.lexima_enable_endwise_rules = vim.fn.get(vim.g, "lexima_enable_endwise_rules", true)
    vim.g.lexima_accept_pum_with_enter = vim.fn.get(vim.g, "lexima_accept_pum_with_enter", vim.fn.has("nvim"))
    vim.g.lexima_ctrlh_as_backspace = vim.fn.get(vim.g, "lexima_ctrlh_as_backspace", false)
    vim.g.lexima_disable_on_nofile = vim.fn.get(vim.g, "lexima_disable_on_nofile", false)
    vim.g.lexima_disable_abbrev_trigger = vim.fn.get(vim.g, "lexima_disable_abbrev_trigger", false)

    vim.g["lexima#default_rules"] = {
        { char = "(", input_after = ")" },
        { char = "(", at = [[\\\%#]] },
        { char = ")", at = [[\%#)]], leave = 1 },
        { char = "<BS>", at = [[(\%#)]], delete = 1 },
        { char = "{", input_after = "}" },
        { char = "}", at = [[\%#}]], leave = 1 },
        { char = "<BS>", at = [[{\%#}]], delete = 1 },
        { char = "[", input_after = "]" },
        { char = "[", at = [[\\\%#]] },
        { char = "]", at = [==[\%#]]==], leave = 1 },
        { char = "<BS>", at = [==[\[\%#\]]==], delete = 1 },
        { char = '"', input_after = '"' },
        { char = '"', at = [[\%#"]], leave = 1 },
        { char = '"', at = [[\\\%#]] },
        { char = '"', at = [[^\s*\%#]], filetype = "vim" },
        { char = '"', at = [[\%#\s*$]], filetype = "vim" },
        { char = "<BS>", at = [["\%#"]], delete = 1 },
        { char = '"', at = [[""\%#]], input_after = '"""' },
        { char = '"', at = [[\%#"""]], leave = 3 },
        { char = "<BS>", at = [["""\%#"""]], input = "<BS><BS><BS>", delete = 3 },
        { char = "'", input_after = "'" },
        { char = "'", at = [[\%#'']], leave = 1 },
        { char = "'", at = [[\w\%#''\@!]] },
        { char = "'", at = [[\\\%#]] },
        { char = "'", at = [[\\\%#]], leave = 1, filetype = { "vim", "sh", "csh", "ruby", "tcsh", "zsh" } },
        { char = "'", filetype = { "haskell", "lisp", "clojure", "ocaml", "reason", "scala", "rust" } },
        { char = "<BS>", at = "'\\%#'", delete = 1 },
        { char = "'", at = "''\\%#", input_after = "'''" },
        { char = "'", at = "\\%#'''", leave = 3 },
        { char = "<BS>", at = "'''\\%#'''", input = "<BS><BS><BS>", delete = 3 },
        { char = "`", input_after = "`" },
        { char = "`", at = [[\%#`]], leave = 1 },
        { char = "<BS>", at = [[`\%#`]], delete = 1 },
        { char = "`", filetype = { "ocaml", "reason" } },
        { char = "`", at = [[``\%#]], input_after = "```" },
        { char = "`", at = [[\%#```]], leave = 3 },
        { char = "<BS>", at = [[```\%#```]], input = "<BS><BS><BS>", delete = 3 },
    }

    vim.g["lexima#newline_rules"] = {
        { char = "<CR>", at = [[(\%#)]], input_after = "<CR>" },
        {
            char = "<CR>",
            at = [[(\%#$]],
            input_after = "<CR>)",
            except = [[\C\v^(\s*)\S.*%#\n%(%(\s*|\1\s.+)\n)*\1\)]],
        },
        { char = "<CR>", at = [[{\%#}]], input_after = "<CR>" },
        {
            char = "<CR>",
            at = [[{\%#$]],
            input_after = "<CR>}",
            except = [[\C\v^(\s*)\S.*%#\n%(%(\s*|\1\s.+)\n)*\1\}]],
        },
        { char = "<CR>", at = [==[\[\%#]]==], input_after = "<CR>" },
        {
            char = "<CR>",
            at = [==[\[\%#$]==],
            input_after = "<CR>]",
            except = [==[\C\v^(\s*)\S.*%#\n%(%(\s*|\1\s.+)\n)*\1\]]==],
        },
        { char = "<CR>", at = [[^```\(\S*\)\%#```]], input = "<CR>", input_after = "<CR>" },
    }

    vim.g["lexima#space_rules"] = {
        { char = "<Space>", at = [[(\%#)]], input_after = "<Space>" },
        { char = ")", at = [[\%# )]], leave = 2 },
        { char = "<BS>", at = [[( \%# )]], delete = 1 },
        { char = "<Space>", at = [[{\%#}]], input_after = "<Space>" },
        { char = "}", at = [[\%# }]], leave = 2 },
        { char = "<BS>", at = [[{ \%# }]], delete = 1 },
        { char = "<Space>", at = [==[\[\%#]]==], input_after = "<Space>" },
        { char = "]", at = [==[\%# ]]==], leave = 2 },
        { char = "<BS>", at = [==[\[ \%# ]]==], delete = 1 },
    }

    if vim.g.lexima_enable_basic_rules then
        for _, rule in ipairs(vim.g["lexima#default_rules"]) do
            M.add_rule(rule)
        end
    end
    if vim.g.lexima_enable_newline_rules then
        for _, rule in ipairs(vim.g["lexima#newline_rules"]) do
            M.add_rule(rule)
        end
    end
    if vim.g.lexima_enable_space_rules then
        for _, rule in ipairs(vim.g["lexima#space_rules"]) do
            M.add_rule(rule)
        end
    end

    if vim.g.lexima_enable_endwise_rules then
        require("lexima-insx.endwise").setup()
    end
end

return M
