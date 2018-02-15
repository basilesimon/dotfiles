#/bin/sh

# Install homebrew
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

# Install zsh and tmux
echo "Install zsh and tmux..."
brew install zsh tmux
echo "Changing default shell to zsh..."
chsh -s /bin/zsh

# Install ohmyzsh
echo "Installing wget oh-my-zsh and themes..."
brew install wget
sh -c "$(wget https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh -O -)"

# Spacemacs
echo "Installing emacs-plus..."
brew tap d12frosted/emacs-plus
brew install emacs-plus
brew linkapps emacs-plus

echo "Cloning syl20bnr/spacemacs..."
git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d
# ln -f -s ~/.spacemacs ~/.spacemacs

echo "Importing snippets..."
rm -r ~/.emacs.d/private/snippets
# ln -f -s ~/dotfiles/.emacs.d/private/snippets ~/.emacs.d/private/snippets

mkdir ~/colorschemes/
echo "Link all dotfiles in /home..."
for i in $(ls -A)
do
    ln -f -s ~/dotfiles/$i ~/$i
done
ln -f -s ~/dotfiles/colorschemes/solarized.py ~/colorschemes/solarized.py
rm ~/.zshrc ~/.zshrc.pre-oh-my-zsh
ln -f -s ~/dotfiles/.zshrc ~/.zshrc

# Install Vundle
# Launch vim and run :PluginInstall
echo "Installing Vundle..."
rm -rf ~/dotfiles/.vim/bundle/Vundle.vim
git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
echo "Launch vim and run :PluginInstall"

# Install slate
cd /Applications && curl http://www.ninjamonkeysoftware.com/slate/versions/slate-latest.tar.gz | tar -xz

# dev
brew install nvm yarn
nvm install 8
nvm use 8
npm install -g yo prettier http-server

# more installs
brew install r
wget https://bootstrap.pypa.io/get-pip.py
sudo python get-pip.py

echo "Cleaning up..."
rm -rf ~/.git

echo "done."
