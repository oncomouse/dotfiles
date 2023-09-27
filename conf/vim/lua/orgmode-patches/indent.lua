local config = require('orgmode.config')
local ts_utils = require('nvim-treesitter.ts_utils')
local query = nil

local get_matches = ts_utils.memoize_by_buf_tick(function(bufnr)
  local tree = vim.treesitter.get_parser(bufnr, 'org', {}):parse()
  if not tree or not #tree then
    return {}
  end
  local matches = {}
  local root = tree[1]:root()
  for _, match, _ in query:iter_matches(root, bufnr, 0, -1) do
    for id, node in pairs(match) do
      local range = ts_utils.node_to_lsp_range(node)
      local type = node:type()

      local opts = {
        type = type,
        node = node,
        parent = node:parent(),
        line_nr = range.start.line + 1,
        line_end_nr = range['end'].line,
        name = query.captures[id],
        indent = vim.fn.indent(range.start.line + 1),
      }

      if type == 'headline' then
        opts.stars = vim.treesitter.get_node_text(node:field('stars')[1], bufnr):len()
        opts.indent = opts.indent + opts.stars + 1
        matches[range.start.line + 1] = opts
      end

      if type == 'listitem' then
        local content = node:named_child(1)
        if content then
          local content_linenr, content_indent = content:start()
          if content_linenr == range.start.line then
            opts.overhang = content_indent - opts.indent
          end
        end
        if not opts.overhang then
          local bullet = node:named_child(0)
          opts.overhang = vim.treesitter.get_node_text(bullet, bufnr):len() + 1
        end

        local parent = node:parent()
        while parent and parent:type() ~= 'section' and parent:type() ~= 'listitem' do
          parent = parent:parent()
        end
        opts.nesting_parent_linenr = parent and (parent:start() + 1)

        for i = range.start.line, range['end'].line - 1 do
          matches[i + 1] = opts
        end
      end

      if type == 'paragraph' or type == 'drawer' or type == 'property_drawer' or type == 'block' then
        opts.indent_type = 'other'
        local parent = node:parent()
        while parent and parent:type() ~= 'section' do
          parent = parent:parent()
        end
        if parent then
          local headline = parent:named_child('headline')
          local stars = vim.treesitter.get_node_text(headline:field('stars')[1], bufnr):len()
          opts.indent = stars + 1
          for i = range.start.line, range['end'].line - 1 do
            matches[i + 1] = opts
          end
        end
      end
    end
  end

  return matches
end)

local prev_section = nil
local function foldexpr()
  query = query or vim.treesitter.query.get('org', 'org_indent')
  local matches = get_matches(0)
  local match = matches[vim.v.lnum]
  local next_match = matches[vim.v.lnum + 1]
  if not match and not next_match then
    return '='
  end
  match = match or {}

  if match.type == 'headline' then
    prev_section = match
    if
      match.parent:parent():type() ~= 'section'
      and match.stars > 1
      and match.parent:named_child_count('section') == 0
    then
      return 0
    end
    return '>' .. match.stars
  end

  if match.type == 'drawer' or match.type == 'property_drawer' or match.type == 'block' then
    if match.line_nr == vim.v.lnum then
      return 'a1'
    end
    if match.line_end_nr == vim.v.lnum then
      return 's1'
    end
  end

  if next_match and next_match.type == 'headline' and prev_section then
    if next_match.stars <= prev_section.stars then
      return '<' .. prev_section.stars
    end
  end

  return '='
end

local function indentexpr(linenr, mode)
  linenr = linenr or vim.v.lnum
  mode = mode or vim.fn.mode()
  local noindent_mode = config.org_indent_mode == 'noindent'
  query = query or vim.treesitter.query.get('org', 'org_indent')

  local prev_linenr = vim.fn.prevnonblank(linenr - 1)

  local matches = get_matches(0)
  local match = matches[linenr]
  local prev_line_match = matches[prev_linenr]

  if not match and not prev_line_match then
    return -1
  end

  match = match or {}
  prev_line_match = prev_line_match or {}

  if prev_line_match.type == 'headline' then
    if noindent_mode or (match.type == 'headline' and match.stars > 0) then
      return 0
    end
    return prev_line_match.indent
  end

  if match.type == 'headline' then
    return 0
  end

  if match.type == 'listitem' then
    -- We first figure out the indent of the first line of a listitem. Then we
    -- check if we're on the first line or a "hanging" line. In the latter
    -- case, we add the overhang.
    local first_line_indent
    local parent_linenr = match.nesting_parent_linenr
    if parent_linenr then
      local parent_match = matches[parent_linenr]
      if parent_match.type == 'listitem' then
        -- Nested listitem. Because two listitems cannot start on the same line,
        -- we simply fetch the parent's indentation and add its overhang.
        -- Don't use parent_match.indent, it might be stale if the parent
        -- already got reindented.
        first_line_indent = vim.fn.indent(parent_linenr) + parent_match.overhang
      elseif parent_match.type == 'headline' and not noindent_mode then
        -- Un-nested list inside a section, indent according to section.
        first_line_indent = parent_match.indent
      else
        -- Noindent mode.
        first_line_indent = 0
      end
    else
      -- Top-level list before the first headline.
      first_line_indent = 0
    end
    -- Add overhang if this is a hanging line.
    if linenr ~= match.line_nr then
      return first_line_indent + match.overhang
    end
    return first_line_indent
  end

  -- In indent mode, we also count the non-listem line *after* a listitem as
  -- part of the listitem. Keep in mind that double empty lines end a list as
  -- per Orgmode syntax.
  if mode:match('^[iR]') and prev_line_match.type == 'listitem' and linenr - prev_linenr < 3 then
    -- After the first line of a listitem, we have to add the overhang to the
    -- listitem's own base indent. After all further lines, we can simply copy
    -- the indentation.
    if prev_linenr == prev_line_match.line_nr then
      return vim.fn.indent(prev_linenr) + prev_line_match.overhang
    end
    return vim.fn.indent(prev_linenr)
  end

  if noindent_mode then
    return 0
  end

  if match.indent_type == 'other' then
    return match.indent
  end

  return vim.fn.indent(prev_linenr)
end

local function foldtext()
  local line = vim.fn.getline(vim.v.foldstart)

  if config.org_hide_leading_stars then
    line = vim.fn.substitute(line, '\\(^\\*\\+\\)', '\\=repeat(" ", len(submatch(0))-1) . "*"', '')
  end

  if vim.opt.conceallevel:get() > 0 and string.find(line, '[[', 1, true) then
    line = string.gsub(line, '%[%[(.-)%]%[?(.-)%]?%]', function(link, text)
      if text == '' then
        return link
      else
        return text
      end
    end)
  end

  return line .. config.org_ellipsis
end

local indent = require("orgmode.org.indent")
indent.foldexpr = foldexpr
indent.indentexpr = indentexpr
indent.foldtext = foldtext
