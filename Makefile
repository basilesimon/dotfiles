homebrew:
	/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

linkZSH:
	chsh -s /bin/zsh
	sh -c "$(wget https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh -O -)"
	mkdir ${HOME}/colorschemes/
	ln -f -s ${HOME}/dotfiles/colorschemes/solarized.py ${HOME}/colorschemes/solarized.py
	ln -f -s ${HOME}/dotfiles/.zshrc ${HOME}/.zshrc
	ln -f -s ${HOME}/dotfiles/.terminfo-24bit.src ${HOME}/.terminfo-24bit.src
	ln -f -s ${HOME}/dotfiles/.tmux.conf ${HOME}/.tmux.conf
	echo "source ${HOME}/.zshrc"

zshMac:
	brew install zsh wget tmux
zshLinux:
	sudo apt-get install zsh wget tmux htop

linkEmacs:
	mkdir ${HOME}/.emacs.d/
	mkdir ${HOME}/.emacs.d/private
	ln -f -s ${HOME}/dotfiles/init.el ${HOME}/.emacs.d/init.el
	ln -f -s ${HOME}/dotfiles/snippets ${HOME}/.emacs.d/private/snippets

emacsMac:
	brew tap d12frosted/emacs-plus
	brew install emacs-plus
	brew linkapps emacs-plus
emacsLinux:
	sudo add-apt-repository ppa:kelleyk/emacs
	sudo apt-get install emacs26

linkDotfiles:
	ln -f -s ${HOME}/dotfiles/.slate ${HOME}/.slate
	ln -f -s ${HOME}/dotfiles/.config ${HOME}/.config
	ln -f -s ${HOME}/dotfiles/.colors ${HOME}/.colors
	ln -f -s ${HOME}/dotfiles/.aliases ${HOME}/.aliases 
	ln -f -s ${HOME}/dotfiles/.profile ${HOME}/.profile

nodeMac:
	brew install nvm yarn
	nvm install 8
	nvm use 8
	npm install -g prettier

nodeLinux:
    	wget -qO- https://raw.githubusercontent.com/nvm-sh/nvm/v0.35.2/install.sh | bash
	nvm install 8
	nvm use 8
	sudo apt-get install yarn
	yarn global add prettier

pythonMac:
	brew install pyenv 	
	pyenv install 3.7.2
	pyenv global 3.7.2

macinstall:
	homebrew zshMac linkZSH linkDotfiles emacsMac linkEmacs
	cd /Applications && curl http://www.ninjamonkeysoftware.com/slate/versions/slate-latest.tar.gz | tar -xz
	nodeMac
	pythonMac
	tic -x -o ~/.terminfo ~/dotfiles/terminfo-24bit.src

regolith:
	mkdir -p ${HOME}/.config/regolith/i3
	ln -f -s ${HOME}/dotfiles/i3/regolith_root_style ${HOME}/.Xresources-regolith
	ln -f -s ${HOME}/dotfiles/i3/i3xrocks ${HOME}/.Xresources-i3xrocks
	ln -f -s ${HOME}/dotfiles/i3/config ${HOME}/.config/regolith/i3/config
	ln -f -s ${HOME}/dotfiles/i3/typeface-input ${HOME}/.config/regolith/typeface-input

linuxclean:
	rm -r ${HOME}/.emacs.d/
	rm -r ${HOME}/colorschemes/
	rm -r ${HOME}/.config/regolith
	rm ${HOME}/.Xresources-regolith
	rm ${HOME}/.Xresources-i3xrocks
	rm ${HOME}/.tmux.conf
	rm ${HOME}/.profile
	rm ${HOME}/.zshrc
	sudo apt-get autoremove yarn

linuxinstall: zshLinux linkZSH emacsLinux linkEmacs regolith linkDotfiles
