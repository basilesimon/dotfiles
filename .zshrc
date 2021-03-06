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

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to disable command auto-correction.
DISABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
HIST_STAMPS="dd.mm.yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

ZSH_DISABLE_COMPFIX="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# plugins=(git github sublime)
source $ZSH/oh-my-zsh.sh
source ~/.aliases

# User configuration

export PATH=$PATH"/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/Library/TeX/texbin:"
export PATH=$PATH:/usr/local/bin/prettier

# You may need to manually set your language environment
export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
 if [[ -n $SSH_CONNECTION ]]; then
   export EDITOR='vim'
 else
   export EDITOR='vim'
 fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"

# Dircolors
# eval `dircolors ~/.dir_colors/dircolors.ansi-dark`
# eval `dircolors ~/.colors`

# Plugins
# plugins=(sudo)

# Tmux scheme
[[ $TMUX = "" ]] && export TERM="xterm-256color"

# Disable completion
# zstyle ':completion:*' hosts off


# if [ "$(uname 2> /dev/null)" != "Linux" ]; then
#     source $(brew --prefix nvm)/nvm.sh
#     export NVM_DIR="$HOME/.nvm"
#     . "$(brew --prefix nvm)/nvm.sh"
#     eval "$(rbenv init -)"
# fi

# NVM slows down zsh startup A LOT
# so we lazy load it and still make global binaries available
# https://www.reddit.com/r/node/comments/4tg5jg/lazy_load_nvm_for_faster_shell_start/d5ib9fs/?context=3
# https://medium.com/@dannysmith/little-thing-2-speeding-up-zsh-f1860390f92
# Add every binary that requires nvm, npm or node to run to an array of node globals
NODE_GLOBALS=(`find ~/.nvm/versions/node -maxdepth 3 -type l -wholename '*/bin/*' | xargs -n1 basename | sort | uniq`)
NODE_GLOBALS+=("node")
NODE_GLOBALS+=("nvm")

# Lazy-loading nvm + npm on node globals call
load_nvm () {
  export NVM_DIR=~/.nvm
  [ -s "$(brew --prefix nvm)/nvm.sh" ] && . "$(brew --prefix nvm)/nvm.sh"
}

# Making node global trigger the lazy loading
for cmd in "${NODE_GLOBALS[@]}"; do
  eval "${cmd}(){ unset -f ${NODE_GLOBALS}; load_nvm; ${cmd} \$@ }"
done


# compinit/zcompdump once a day
# autoload -Uz compinit
# for dump in ~/.zcompdump(N.mh+24); do
#     compinit
# done
# compinit -C


eval "$(pyenv init -)"
export PATH="/usr/local/opt/imagemagick@6/bin:$PATH"
export PYTHONPATH=/usr/local/opt/lib/python3.7/site-packages:/usr/local/opt/osgeo-qgis/lib/python3.7/site-packages:/usr/local/opt/osgeo-qgis/QGIS.app/Contents/Resources/python:/usr/local/opt/osgeo-gdal-python/lib/python3.7/site-packages:$PYTHONPATH
export PATH=/usr/local/opt/ruby/bin:$PATH
export PATH="/usr/local/sbin:$PATH"
export GEM_HOME=$HOME/gems
export PATH=$HOME/gems/bin:$PATH
export NODE_PATH=`which node`
