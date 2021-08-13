homebrew:
	/usr/bin/ruby -e "$$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
	brew update --force && brew upgrade

iterm:
	brew cask install iterm2
	ln -f -s ${HOME}/dotfiles/src/change-theme-with-os.py ${HOME}/Library/ApplicationSupport/iTerm2/Scripts/change-theme-with-os.py

linkZSH:
	chsh -s /usr/bin/zsh
	sh -c "$(wget https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh -O -)"
	sudo apt-get install dconf-cli
	ln -f -s ${HOME}/dotfiles/.zshrc ${HOME}/.zshrc
	echo "source ${HOME}/.zshrc"

tmux:
	mkdir ${HOME}/.tmux
	mkdir ${HOME}/.tmux/plugins
	git clone https://github.com/tmux-plugins/tpm ${HOME}/.tmux/plugins/tpm
	ln -f -s ${HOME}/dotfiles/.tmux.conf ${HOME}/.tmux.conf

zshLinux:
	sudo apt-get install zsh wget tmux htop curl

linkEmacs:
	mkdir ${HOME}/.emacs.d/
	mkdir ${HOME}/.emacs.d/private
	ln -f -s ${HOME}/dotfiles/init.el ${HOME}/.emacs.d/init.el
	ln -f -s ${HOME}/dotfiles/snippets ${HOME}/.emacs.d/private/snippets

emacsLinux:
	sudo echo "deb http://ppa.launchpad.net/kelleyk/emacs/ubuntu focal main" >> /etc/apt/sources.list
	sudo echo "deb http://ppa.launchpad.net/kelleyk/emacs/ubuntu focal main/debug" >> /etc/apt/sources.list
	sudo apt-key adv --keyserver hkp://pool.sks-keyservers.net:80 --recv-keys 0x873503A090750CDAEB0754D93FF0E01EEAAFC9
	sudo apt-get update
	sudo apt-get install emacs27
	mkdir ${HOME}/.emacs.d/
	mkdir ${HOME}/.emacs.d/private
	ln -f -s ${HOME}/dotfiles/init.el ${HOME}/.emacs.d/init.el
	ln -f -s ${HOME}/dotfiles/snippets ${HOME}/.emacs.d/private/snippets

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
    	curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.37.2/install.sh | bash
	nvm install --lts
	nvm use --lts
	sudo apt remove yarn
	sudo apt-get install yarn
	yarn global add prettier

pythonMac:
	brew install pyenv 	
	pyenv install 3.7.2
	pyenv global 3.7.2

pythonLinux:
	sudo apt install -y make build-essential libssl-dev zlib1g-dev $\
		libbz2-dev libreadline-dev libsqlite3-dev wget curl llvm libncurses5-dev $\
		libncursesw5-dev xz-utils tk-dev libffi-dev liblzma-dev python-openssl git
	git clone https://github.com/pyenv/pyenv.git ~/.pyenv
	exec ${SHELL}
	pyenv install 3.7.11

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
	curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo apt-key add -
	sudo add-apt-repository "deb [arch=amd64] https://download.docker.com/linux/ubuntu focal stable"
	sudo apt-get update
	apt-cache policy docker-ce
	sudo apt-get install docker-ce docker-ce-cli containerd.io
	sudo usermod -aG docker ${USER}
	su - ${USER}

albert:
	curl https://build.opensuse.org/projects/home:manuelschneid3r/public_key | sudo apt-key add -
	echo 'deb http://download.opensuse.org/repositories/home:/manuelschneid3r/xUbuntu_20.04/ /' | sudo tee /etc/apt/sources.list.d/home:manuelschneid3r.list
	sudo wget -nv https://download.opensuse.org/repositories/home:manuelschneid3r/xUbuntu_20.04/Release.key -O "/etc/apt/trusted.gpg.d/home:manuelschneid3r.asc"
	sudo apt update
	sudo apt install albert

dockerMac:
	brew install --cask docker

linuxinstall: zshLinux gitLinux linkZSH tmux emacsLinux linkEmacs linkDotfiles dockerLinux pythonLinuxgit clone https://github.com/pyenv/pyenv.git ~/.pyenv
macinstall: homebrew iterm gitMac zshMac linkDotfiles tmux nodeMac pythonMac rubyMac slateMac rMac dockerMac
vps: zshLinux gitLinux linkZSH tmux linkDotfiles dockerLinux
