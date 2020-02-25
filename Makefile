homebrew:
	/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

zshMac:
	brew install zsh wget tmux
	chsh -s /bin/zsh
	sh -c "$(wget https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh -O -)"
	mkdir ${HOME}/colorschemes/
	ln -f -s ${HOME}/dotfiles/colorschemes/solarized.py ${HOME}/colorschemes/solarized.py
	rm ${HOME}/.zshrc
	rm ${HOME}/.zshrc.pre-oh-my-zsh
	ln -f -s ${HOME}/dotfiles/.zshrc ${HOME}/.zshrc
	ln -f -s ${HOME}/dotfiles/.terminfo-24bit.src ${HOME}/.terminfo-24bit.src
	ln -f -s ${HOME}/dotfiles/.tmux.conf ${HOME}/.tmux.conf
	source ${HOME}/.zshrc

emacsMac:
	brew tap d12frosted/emacs-plus
	brew install emacs-plus
	brew linkapps emacs-plus
	mkdir ${HOME}/emacs.d/
	mkdir ${HOME}/emacs.d/private
	ln -f -s ${HOME}/dotfiles/init.el ${HOME}/.emacs.d/init.el
	ln -f -s ${HOME}/dotfiles/snippets ${HOME}/.emacs.d/private/snippets

linkDotfiles:
	ln -f -s ${HOME}/dotfiles/.config ${HOME}/.config
	ln -f -s ${HOME}/dotfiles/.aliases {HOME}/.aliases 
	ln -f -s ${HOME}/dotfiles/.colors {HOME}/.colors
	ln -f -s ${HOME}/dotfiles/.slate {HOME}/.slate

nodeMac:
	brew install nvm yarn
	nvm install 8
	nvm use 8
	npm install -g prettier

pythonMac:
	brew install pyenv 	
	pyenv install 3.7.2
	pyenv global 3.7.2

macinstall:
	homebrew
	zshMac
	linkDotfiles
	cd /Applications && curl http://www.ninjamonkeysoftware.com/slate/versions/slate-latest.tar.gz | tar -xz
	nodeMac
	pythonMac
	tic -x -o ~/.terminfo ~/dotfiles/terminfo-24bit.src
