#/bin/sh

echo "Link all dotfiles in /home..."
for i in $(ls -A -I link.sh)
do
  ln -f -s $(pwd)/$i ~/$i
done

# Install ohmyzsh
echo "Installing oh-my-zsh and themes..."
sh -c "$(wget https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh -O -)"

# Install Vundle
# Launch vim and run :PluginInstall
echo "Installing Vundle..."
rm -rf ~/dotfiles/.vim/bundle/Vundle.vim
git clone https://github.com/VundleVim/Vundle.vim.git ~/dotfiles/.vim/bundle
echo "Launch vim and run :PluginInstall"

echo "done."
