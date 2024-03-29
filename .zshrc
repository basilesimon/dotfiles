#Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
ZSH_THEME="ys"

#keybindings
bindkey '\e[1;5C' forward-word # ctrl right
bindkey '\e[1;5D' backward-word # ctrl left
bindkey '\e[1;5A' beginning-of-line # ctrl top
bindkey '\e[1;5B' end-of-line # ctrl down

# disable command auto-correction.
DISABLE_CORRECTION="true"

# display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

# change the command execution time
# stamp shown in the history command output.
HIST_STAMPS="dd.mm.yyyy"

ZSH_DISABLE_COMPFIX="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# plugins=(git github sublime)
source $ZSH/oh-my-zsh.sh
source ~/.aliases

export LANG=en_US.UTF-8
export EDITOR='vim'

case `uname` in
  Darwin)
    # commands for OS X go here
      # Tmux scheme
      [[ $TMUX = "" ]] && export TERM="xterm-24bit"
      TERM="xterm-24bit"
  ;;
  Linux)
    # commands for Linux go here
      # Tmux scheme
      [[ $TMUX = "" ]] && export TERM="xterm-256color"
      TERM="xterm-256color"
  ;;
esac


# NVM slows down zsh startup A LOT
# so we lazy load it and still make global binaries available
# https://www.reddit.com/r/node/comments/4tg5jg/lazy_load_nvm_for_faster_shell_start/d5ib9fs/?context=3
# https://medium.com/@dannysmith/little-thing-2-speeding-up-zsh-f1860390f92
# Add every binary that requires nvm, npm or node to run to an array of node globals
NODE_GLOBALS=(`find ${nvm_path}/versions/node -maxdepth 3 -type l -wholename '*/bin/*' | xargs -n1 basename | sort | uniq`)
NODE_GLOBALS+=("node")
NODE_GLOBALS+=("nvm")

local brew_path="/opt/homebrew/bin"
local brew_opt_path="/opt/homebrew/opt"
local nvm_path="$HOME/.nvm"
export PATH="${brew_path}:${PATH}"

# Lazy-loading nvm + npm on node globals call
load_nvm () {
  export NVM_DIR="${HOME}/.nvm"
  case `uname` in
      Darwin)
	  # commands for OS X go here
	[ -s "/usr/local/opt/nvm/nvm.sh" ] && \. "/usr/local/opt/nvm/nvm.sh"  # This loads nvm
	[ -s "/usr/local/opt/nvm/etc/bash_completion.d/nvm" ] && \. "/usr/local/opt/nvm/etc/bash_completion.d/nvm"  # This loads nvm bash_completion
	  ;;
      Linux)
	  # commands for Linux go here
	  [ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"
	  ;;
  esac
}

# Making node global trigger the lazy loading
for cmd in "${NODE_GLOBALS[@]}"; do
  eval "${cmd}(){ unset -f ${NODE_GLOBALS}; load_nvm; ${cmd} \$@ }"
done


# export PYENV_ROOT="$HOME/.pyenv"
# eval "$(pyenv init -)"
# eval "$(pyenv init --path)"
export PATH=$PATH"/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/Library/TeX/texbin:/usr/local/opt/imagemagick@6/bin:/usr/local/opt/ruby/bin:/usr/local/sbin:/Users/silverie/gems/bin:$PYENV_ROOT/bin:$PATH"
export PYTHONPATH=/usr/local/opt/lib/python3.7/site-packages:/usr/local/opt/osgeo-qgis/lib/python3.7/site-packages:/usr/local/opt/osgeo-qgis/QGIS.app/Contents/Resources/python:/usr/local/opt/osgeo-gdal-python/lib/python3.7/site-packages:$PYTHONPATH
export GEM_HOME=$HOME/gems
export NODE_PATH=`which node`

