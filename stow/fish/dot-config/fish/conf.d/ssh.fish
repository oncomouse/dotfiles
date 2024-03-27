# set -gx SSH_AUTH_SOCK "$XDG_RUNTIME_DIR/ssh-agent.socket"
# set -gx SSH_AUTH_SOCK "$XDG_RUNTIME_DIR/keyring/ssh"
if test -e "$XDG_RUNTIME_DIR/gcr/ssh"
  set -gx SSH_AUTH_SOCK "$XDG_RUNTIME_DIR/gcr/ssh"
end
