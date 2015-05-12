
call plug#begin('~/.nvim/plugged')

Plug 'gmarik/vundle'
Plug 'FuzzyFinder'
Plug 'L9'
Plug 'SearchComplete'
Plug 'a.vim'
Plug 'buffergrep'
Plug 'cecutil'
Plug 'compview'
Plug 'git://github.com/nathanaelkane/vim-indent-guides.git'
Plug 'https://github.com/Shougo/vimproc.git'
Plug 'https://github.com/eagletmt/ghcmod-vim.git'
Plug 'https://github.com/godlygeek/tabular.git'
Plug 'https://github.com/scrooloose/nerdcommenter.git'
Plug 'https://github.com/scrooloose/nerdtree.git'
Plug 'https://github.com/tpope/vim-repeat.git'
Plug 'https://github.com/tpope/vim-surround.git'
Plug 'https://github.com/vim-latex/vim-latex.git'
Plug 'python.vim'
Plug 'taglist.vim'
Plug 'vim-indent-object'
Plug 'https://github.com/coderifous/textobj-word-column.vim.git'
Plug 'https://github.com/idris-hackers/idris-vim.git'
Plug 'https://github.com/guns/vim-sexp.git'
Plug 'https://github.com/kien/rainbow_parentheses.vim.git'
Plug 'https://github.com/derekelkins/agda-vim.git'
Plug 'https://github.com/wlangstroth/vim-racket.git'
Plug 'https://github.com/ehamberg/vim-cute-python.git'
Plug 'https://github.com/rust-lang/rust.vim.git'

" Colorscheme bundles
Plug 'molokai'
Plug 'tir_black'
Plug 'darkspectrum'
Plug 'Sorcerer'
Plug 'https://github.com/altercation/vim-colors-solarized.git'
Plug 'https://github.com/nanotech/jellybeans.vim.git'

call plug#end()

" Tab settings
" set cindent
set autoindent
set smartindent
set expandtab
set tabstop=4
set shiftwidth=4

set t_Co=256

syntax on
filetype plugin on
filetype indent on

colorscheme losh_molokai

set incsearch
set ignorecase
set smartcase

set encoding=utf-8
set scrolloff=3
set showmode
set showcmd
set hidden
set wildmenu
set wildmode=list:longest
set visualbell
set cursorline
set ttyfast
set ruler
set backspace=indent,eol,start
set laststatus=2
set number
set virtualedit=all

set nocompatible
set modelines=0
set wrap
set textwidth=80
set formatoptions=qrn1
set colorcolumn=85
set title
set lazyredraw
" set breakindent

" Don't move the cursor to the first column during row traversals
set nostartofline

" set autochdir

" Backup & Undo settings
set undodir=~/.nvim/undodir
set backupdir=~/.nvim/backup
set undofile
set undolevels=1000
set undoreload=10000

set ssop-=options   " do not store global and local values in session
set ssop-=folds     " do not store folds in the session

" Navigation of folded lines
map j gj
map k gk

" Quick navigation of tabs
map <leader>th :tabprev<CR>
map <leader>tl :tabnext<CR>
map <leader>tn :tabnew<CR>
map <leader>td :tabclose<CR>

" Fold based on the syntax of the file, but only fold the outer level
set foldnestmax=1
set foldmethod=syntax

" Hilight search
set hlsearch

" Press Space to turn off highlighting and clear any message already displayed.
nnoremap <silent> <Space> :nohlsearch<Bar>:echo<CR>
nnoremap ' `
nnoremap ` '

" Remove menu
set go=c

" Format using astyle
map <leader>f :!astyle --style=allman --indent=spaces=4 -N -S -w --add-one-line-brackets --convert-tabs --indent-col1-comments -m0 %<CR>

" Faster way to switch between splits
map <leader>w <C-w>w
map <leader>h <C-w>h
map <leader>j <C-w>j
map <leader>k <C-w>k
map <leader>l <C-w>l

" Some options for word processing
autocmd BufRead *\.txt setlocal formatoptions+=l
autocmd BufRead *\.txt setlocal lbr
autocmd BufRead *\.txt setlocal spell spelllang=en_us

" some options for pentadactyl text areas
autocmd BufRead *\.tmp setlocal formatoptions+=l
autocmd BufRead *\.tmp setlocal lbr
autocmd BufRead *\.tmp setlocal spell spelllang=en_us

" Spell checking for latex files
autocmd BufRead *\.tex setlocal spell spelllang=en_us

" Load latex even for empty files
let g:tex_flavor='latex'
autocmd BufRead *\.tex set iskeyword+=:
map ,p :!xelatex -shell-escape %<CR>
map ,b :!bibtex %<CR>

" Remove trailing whitespace
function! StripTrailingWhitespaces()
    let l = line(".")
    let c = col(".")
    %s/\s\+$//e
    call cursor(l, c)
endfun

map <silent> <leader>s :call StripTrailingWhitespaces() <CR>

" Remove trailing whitespace before saving

let g:proj_flags="imstvcg"

" Complete options (disable preview scratch window)
set completeopt=menu,menuone,longest

" Limit popup menu height
set pumheight=15

" Show clang errors in the quickfix window
let python_highlight_all = 1
let python_slow_sync = 1

" Key Maps
" --------
" Toggle NERDTree with F2
map <F2> :NERDTreeToggle<CR>
" Use Y to copy until the end of the line. Use yy to copy the whole line.
nnoremap Y y$

set statusline=
set statusline +=%1*\ %n\ %*            "buffer number
set statusline +=%5*%{&ff}%*            "file format
set statusline +=%3*%y%*                "file type
set statusline +=%4*\ %<%F%*            "full path
set statusline +=%2*%m%*                "modified flag
set statusline +=%1*%=%5l%*             "current line
set statusline +=%2*/%L%*               "total lines
set statusline +=%1*%4v\ %*             "virtual column number
set statusline +=%2*0x%04B\ %*          "character under cursor

highlight ExtraWhitespace ctermbg=red guibg=red
match ExtraWhitespace /\s\+$/
autocmd BufWinEnter * match ExtraWhitespace /\s\+$/
autocmd InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
autocmd InsertLeave * match ExtraWhitespace /\s\+$/
autocmd BufWinLeave * call clearmatches()

" Prefer C++11 over the standard C++ syntax file
autocmd BufNewFile,BufRead *.h,*.cpp set syntax=cpp11

let g:sexp_enable_insert_mode_mappings = 0
au BufNewFile,BufRead *.agda setf agda

" Interrobangs...
digraph !? 8253
digraph ?! 8253
