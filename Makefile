homebrew:
	/usr/bin/ruby -e "$$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
	brew update --force && brew upgrade

zshMac:
	brew install zsh wget tmux
	sudo sh -c "echo '/usr/local/bin/zsh' >> /etc/shells"
	chsh -s /usr/local/bin/zsh
	curl -L https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh | sh
	mkdir ${HOME}/colorschemes/
	ln -f -s ${HOME}/dotfiles/colorschemes/solarized.py ${HOME}/colorschemes/solarized.py
	rm ${HOME}/.zshrc
	ln -f -s ${HOME}/dotfiles/.zshrc ${HOME}/.zshrc
	ln -f -s ${HOME}/dotfiles/.terminfo-24bit.src ${HOME}/.terminfo-24bit.src
	ln -f -s ${HOME}/dotfiles/.tmux.conf ${HOME}/.tmux.conf
	source ${HOME}/.zshrc
	tic -x -o ~/.terminfo ~/dotfiles/terminfo-24bit.src

emacsMac:
	brew tap d12frosted/emacs-plus
	brew install emacs-plus@27
	ln -s /usr/local/opt/emacs-plus@27/Emacs.app /Applications
	mkdir ${HOME}/.emacs.d/
	mkdir ${HOME}/.emacs.d/private
	ln -f -s ${HOME}/dotfiles/init.el ${HOME}/.emacs.d/init.el
	ln -f -s ${HOME}/dotfiles/snippets ${HOME}/.emacs.d/private/snippets

linkDotfiles:
	ln -f -s ${HOME}/dotfiles/.config ${HOME}/.config
	ln -f -s ${HOME}/dotfiles/.aliases ${HOME}/.aliases 
	ln -f -s ${HOME}/dotfiles/.colors ${HOME}/.colors
	ln -f -s ${HOME}/dotfiles/.slate ${HOME}/.slate

nodeMac:
	brew install nvm yarn
	nvm install 8
	nvm use 8
	yarn install -g prettier csvtojson

pythonMac:
	brew install pyenv 	
	pyenv install 3.7.2
	pyenv global 3.7.2

rubyMac:
	brew install rbenv
	rbenv init
	rbenv install 2.7.1
	rbenv global 2.7.1

slateMac: 
	cd /Applications && curl http://www.ninjamonkeysoftware.com/slate/versions/slate-latest.tar.gz | tar -xz
	echo "logout of the session and start Slate"

gitMac:
	brew install git
	git config --global user.name "Basile Simon"
	git config --global user.email "basile@basilesimon.fr"

macinstall: homebrew gitMac zshMac linkDotfiles nodeMac pythonMac rubyMac slateMac
