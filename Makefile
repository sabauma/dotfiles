
top = $(shell pwd)

all:
	ln -s $(top)/vim ~/.vim || true
	ln -s $(top)/vimrc ~/.vimrc || true
	ln -s $(top)/nvim ~/.nvim || true
	ln -s $(top)/nvimrc ~/.nvimrc || true
	ln -s $(top)/pentadactylrc ~/.pentadactylrc || true
	ln -s $(top)/xmobarrc ~/.xmobarrc || true
	ln -s $(top)/xsessionrc ~/.xsessionrc || true
	mv ~/.bashrc ~/.bashrc.bak
	ln -s $(top)/bashrc ~/.bashrc || true
