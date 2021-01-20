homebrew:
	/usr/bin/ruby -e "$$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
	brew update --force && brew upgrade

linkZSH:
	chsh -s /bin/zsh
	sh -c "$(wget https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh -O -)"
	mkdir ${HOME}/colorschemes/
	ln -f -s ${HOME}/dotfiles/colorschemes/solarized.py ${HOME}/colorschemes/solarized.py
	ln -f -s ${HOME}/dotfiles/.zshrc ${HOME}/.zshrc
	ln -f -s ${HOME}/dotfiles/.terminfo-24bit.src ${HOME}/.terminfo-24bit.src
	ln -f -s ${HOME}/dotfiles/.tmux.conf ${HOME}/.tmux.conf
	echo "source ${HOME}/.zshrc"

zshLinux:
	sudo apt-get install zsh wget tmux htop

linkEmacs:
	mkdir ${HOME}/.emacs.d/
	mkdir ${HOME}/.emacs.d/private
	ln -f -s ${HOME}/dotfiles/init.el ${HOME}/.emacs.d/init.el
	ln -f -s ${HOME}/dotfiles/snippets ${HOME}/.emacs.d/private/snippets

emacsLinux:
	sudo add-apt-repository ppa:kelleyk/emacs
	sudo apt-get install emacs26

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
	ln -f -s ${HOME}/dotfiles/.colors ${HOME}/.colors
	ln -f -s ${HOME}/dotfiles/.aliases ${HOME}/.aliases 
	ln -f -s ${HOME}/dotfiles/.profile ${HOME}/.profile

nodeMac:
	brew install nvm yarn
	nvm install 10 
	nvm use 10
	yarn global add prettier csvtojson

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

linuxclean:
	rm -r ${HOME}/.emacs.d/
	rm -r ${HOME}/colorschemes/
	rm ${HOME}/.tmux.conf
	rm ${HOME}/.profile
	rm ${HOME}/.zshrc
	sudo apt-get autoremove yarn

rubyMac:
	brew install rbenv
	rbenv init
	rbenv install 2.7.1
	rbenv global 2.7.1

slateMac: 
	cd /Applications && curl http://www.ninjamonkeysoftware.com/slate/versions/slate-latest.tar.gz | tar -xz
	echo "logout of the session and start Slate"

rMac:
	brew cask install r
	brew cask install rstudio

gitMac:
	brew install git
	git config --global user.name "Basile Simon"
	git config --global user.email "basile@basilesimon.fr"

gitLinux:
	sudo apt-get install git
	git config --global user.name "Basile Simon"
	git config --global user.email "basile@basilesimon.fr"

dockerLinux:
	sudo apt-get install apt-transport-https ca-certificates curl gnupg-agent software-properties-common
	cat "deb [arch=amd64 trusted=yes] https://download.docker.com/linux/ubuntu $(lsb_release -cs) stable" >> /etc/apt/sources.list
	sudo apt-get update
	sudo apt-get install docker-ce docker-ce-cli containerd.io
	sudo usermod -aG docker ${USER}
	su - ${USER}


linuxinstall: zshLinux gitLinux linkZSH emacsLinux linkEmacs linkDotfiles dockerLinux
macinstall: homebrew gitMac zshMac linkDotfiles nodeMac pythonMac rubyMac slateMac rMac
