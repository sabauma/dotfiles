
top = $(shell pwd)

all:
	ln -s $(top)/vim ~/.vim
	ln -s $(top)/vimrc ~/.vimrc
	ln -s $(top)/nvim ~/.nvim
	ln -s $(top)/nvimrc ~/.nvimrc
	ln -s $(top)/pentadactylrc ~/.pentadactylrc
	ln -s $(top)/xmobarrc ~/.xmobarrc
	ln -s $(top)/xsessionrc ~/.xsessionrc
