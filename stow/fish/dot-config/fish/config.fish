# Colors:
set fish_color_normal normal
set fish_color_command green
set fish_color_param blue
set fish_color_redirection $fish_color_param
set fish_color_comment white
set fish_color_escape brpurple
set fish_color_operator $fish_color_escape
set fish_color_end blue
set fish_color_quote cyan
set fish_color_autosuggestion 555 brblack
set fish_color_user brgreen
set fish_color_host $fish_color_normal
set fish_color_valid_path --underline
set fish_color_cwd cyan
set fish_color_cwd_root red
set fish_color_match --background=brblue
set fish_color_search_match bryellow --background=brblack
set fish_color_selection white --bold --background=brblack
set fish_color_cancel -r
set fish_pager_color_prefix white --bold --underline
set fish_pager_color_completion
set fish_pager_color_description $fish_color_quote yellow
set fish_pager_color_progress brwhite --background=cyan
set fish_color_history_current --bold

if status is-login

    # EDITOR
    set -gx EDITOR (which nvim)

    # Tell sxhkd to not use fish:
    set -gx SXHKD_SHELL sh

    # NNN Theme
    set -gx NNN_FCOLORS 0603040200050E070D09abc4

    # Cache Openweather credentials (if they exist)
    if test -e $HOME/.ow_credentials.json
        set -gx OW_KEY (cat ~/.ow_credentials.json | jq -r .key)
        set -gx OW_LAT (cat ~/.ow_credentials.json | jq .coordinates[0])
        set -gx OW_LONG (cat ~/.ow_credentials.json | jq .coordinates[1])
    end

    # DOTFILES_TARGET
    if test -e $HOME/.local/share/dotfiles/target
        set -gx DOTFILES_TARGET (/bin/cat $HOME/.local/share/dotfiles/target)
    end

    # Setup FZF themes:
    set -gx FZF_DEFAULT_OPTS "--ansi --bind='ctrl-o:execute(open {})+abort'"
    set -gx FZF_DEFAULT_COMMAND "fd -t f --follow --hidden"
    set -gx FZF_CTRL_T_COMMAND $FZF_DEFAULT_COMMAND
    set -gx FZF_ALT_C_COMMAND "fd --type d --follow"
    if test -d "$HOME/.cache/wal"
        source ~/.cache/wal/colors.fish
        set -gx FZF_DEFAULT_OPTS "$FZF_DEFAULT_OPTS --color=bg+:$color8,bg:$background,spinner:$color14,hl:$color12" \
            "--color=fg:$foreground,header:$color12,info:$background,pointer:$color14" \
            "--color=marker:$color14,fg+:$foreground,prompt:$color3,hl+:$color12"
    end
    set -gx FZF_CTRL_T_OPTS "--preview-window 'right:60%' --preview 'bat --theme=wal --color=always --style=header,grid --line-range :300 {}'"
    set -gx FZF_ALT_C_OPTS "--preview 'ls --color=always {} | head -200'"

    # XDG Stuff:
    set -gx XDG_DATA_DIRS ~/.local/share/flatpak/exports/share:/var/lib/flatpak/exports/share:/usr/local/share:/usr/share
end
