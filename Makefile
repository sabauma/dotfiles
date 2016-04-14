
top = $(shell pwd)

all:
	# Config files first
	ln -s $(top)/vim ~/.vim || true
	ln -s $(top)/vimrc ~/.vimrc || true
	ln -s $(top)/nvim ~/.config/nvim || true
	# This symlink is just for convenience. Neovim looks in the above directory
	ln -s $(top)/nvimrc ~/.nvimrc || true
	ln -s $(top)/pentadactylrc ~/.pentadactylrc || true
	ln -s $(top)/xmobarrc ~/.xmobarrc || true
	ln -s $(top)/xsessionrc ~/.xsessionrc || true
	ln -s $(top)/haskeline ~/.haskeline || true
	ln -s $(top)/ghci ~/.ghci || true
	# Backup things which might exists
	mv ~/.bashrc ~/.bashrc.bak
	ln -s $(top)/bashrc ~/.bashrc || true
	ln -s $(top)/Xresources ~/.Xresources || true
	# Make the user land bin folder for wallpapers
	mkdir ~/bin || true
	ln -s $(top)/wallpaper.sh ~/bin/wallpaper.sh || true
	# Link pictures into the user's Pictures directory
	mkdir -p ~/Pictures/
	ln -s $(top)/high-res ~/Pictures/high-res || true
	# Download vim-plug for vim and neovim
	curl -fLo ~/.vim/autoload/plug.vim 		   --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
	curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

