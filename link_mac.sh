#/bin/sh

mkdir ~/colorschemes
echo "Link all dotfiles in /home..."
for i in $(ls -A)
do
  ln -f -s ~/dotfiles/$i ~/$i
done
  ln -f -s ~/dotfiles/colorschemes/solarized.py ~/colorschemes/solarized.py


# Install Vundle
# Launch vim and run :PluginInstall
echo "Installing Vundle..."
rm -rf ~/dotfiles/.vim/bundle/Vundle.vim
git clone https://github.com/VundleVim/Vundle.vim.git ~/dotfiles/.vim/bundle
echo "Launch vim and run :PluginInstall"

# Install zsh
echo "Install zsh..."
brew install zsh
chsh -s /bin/zsh

# Install ohmyzsh
echo "Installing oh-my-zsh and themes..."
brew install wget
sh -c "$(wget https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh -O -)"
rm ~/.zshrc ~/.zshrc.pre-oh-my-zsh
ln -f -s ~/dotfiles/.zshrc ~/.zshrc

echo "Cleaning up..."
rm -rf ~/.git

echo "done."
