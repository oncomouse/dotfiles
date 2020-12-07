# oncomouse Dotfiles

These dotfiles work for Mac OS, Arch, and Ubuntu. Ubuntu can also run for server or desktop configurations. Arch server configuration is on the todo list.

## Usage

Run `git clone https://github.com/oncomouse/dotfiles ~/dotfiles` on your computer. Then run either `make desktop` or `make server` to install. `make` can also be re-run to update software.

## Structure

The organization is as follows:

* `~/dotfiles/stow` -- Configuration files that should be stowed. This is the majority of the actual "dot" file in this dotfiles repository
* `~/dotfiles/conf` -- Configuration files that do not need to be stowed. This is primarily used by files that need to be included into other configurations and having a stable location, inside the dotfiles repository, is preferable. It was basically included to manage my (extremely complicated) Vim/Neovim configuration.
* `~/dotfiles/scripts` -- Executables used in these configurations. This includes my implementation of GNU stow, but also things like my panel script for BSPWM/lemonbar and things like color demo scripts for screenshots.
* `~/dotfiles/bootstrap` -- Scripts used to install and configure the software used in these 
* `~/dotfiles/notes` -- Things I'm always forgetting when installing systems (mostly Arch).

## Stow

Packages are managed with a custom re-write of GNU stow. It's located in `scripts/stow.py`. It implements all features of stow except: `--adopt`, `--ignore`, `--defer`, `--override`, `--compat`, and the operations listed in the [Mixing Operations section](https://www.gnu.org/software/stow/manual/stow.html#Mixing-Operations).
