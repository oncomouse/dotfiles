if status --is-login
  # Custom paths:
  add_to_user_paths ~/bin
  add_to_user_paths ~/.local/bin
  # Set up Cargo:
  add_to_user_paths ~/.cargo/bin
  # Set up FZF (if local):
  add_to_user_paths ~/.fzf/bin
  # Set up Poetry:
  add_to_user_paths ~/.poetry/bin
  # Set up ASDF:
  add_to_user_paths ~/.asdf/shims
  add_to_user_paths /usr/local/opt/asdf/bin
  add_to_user_paths ~/.asdf/bin
  # Sbin:
  add_to_user_paths /usr/local/sbin
  add_to_user_paths ~/.ghcup/bin
  # NPM Local:
  add_to_user_paths ~/.npm-packages/bin

  # NPM Local manpath:
  set -q MANPATH || set MANPATH ''
  set -gx MANPATH $MANPATH ~/.npm-packages/share/man

  # Set Python stub location:
  set -gx MYPYPATH ~/dotfiles/conf/python-stubs
  # Set Luarocks PATH
  if which lua5.3 > /dev/null
    for i in (luarocks --lua-version 5.3 path | awk '{sub(/PATH=/, "PATH ", $2); print "set -gx "$2}'); eval $i; end
  end
end

