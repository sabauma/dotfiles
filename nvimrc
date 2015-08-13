
call plug#begin('~/.nvim/plugged')

Plug 'L9'
Plug 'a.vim'
Plug 'buffergrep'
Plug 'git://github.com/nathanaelkane/vim-indent-guides.git'
Plug 'https://github.com/Shougo/vimproc.git', { 'do': 'make' }
Plug 'https://github.com/eagletmt/ghcmod-vim.git'
Plug 'https://github.com/godlygeek/tabular.git'
Plug 'https://github.com/scrooloose/nerdcommenter.git'
Plug 'https://github.com/scrooloose/nerdtree.git'
Plug 'https://github.com/tpope/vim-repeat.git'
Plug 'https://github.com/tpope/vim-surround.git'
Plug 'https://github.com/vim-latex/vim-latex.git'
Plug 'https://github.com/michaeljsmith/vim-indent-object.git'
Plug 'https://github.com/wlangstroth/vim-racket.git'
Plug 'https://github.com/ehamberg/vim-cute-python.git'
Plug 'https://github.com/rust-lang/rust.vim.git'
Plug 'https://github.com/vim-scripts/Efficient-python-folding.git'
Plug 'https://github.com/Konfekt/FastFold.git'
Plug 'https://github.com/octol/vim-cpp-enhanced-highlight.git'
Plug 'https://github.com/raichoo/haskell-vim.git'
Plug 'https://github.com/bitc/vim-hdevtools.git'

" Colorscheme bundles
Plug 'molokai'
Plug 'tir_black'
Plug 'darkspectrum'
Plug 'Sorcerer'
Plug 'https://github.com/altercation/vim-colors-solarized.git'
Plug 'https://github.com/nanotech/jellybeans.vim.git'
Plug 'https://github.com/flazz/vim-colorschemes.git'
Plug 'https://github.com/chriskempson/base16-vim.git'

call plug#end()

" Tab settings
" set cindent
set autoindent
set smartindent
set expandtab
set tabstop=4
set shiftwidth=4

syntax on
filetype plugin on
filetype indent on
set synmaxcol=120

colorscheme badwolf

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
set ruler
set backspace=indent,eol,start
set laststatus=2
set number
set virtualedit=all

set modelines=0
set wrap
set textwidth=80
set formatoptions=qrn1
set colorcolumn=85
set title
set lazyredraw

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

let g:fastfold_fold_command_suffixes = []

" Hilight search
set hlsearch

" Press Space to turn off highlighting and clear any message already displayed.
nnoremap <silent> <Space> :nohlsearch<Bar>:echo<CR>
nnoremap ' `
nnoremap ` '

" Remove menu
set go=c

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

" Interrobangs...
digraph !? 8253
digraph ?! 8253

set tags=./tags;

" Haskell configuration
let g:haskell_enable_quantification = 1
let g:haskell_enable_recursivedo = 1
let g:haskell_infent_if = 0
let g:haskell_indent_case = 2
let g:haskell_indent_where = 2
let g:haskell_indent_do = 2
let g:haskell_indent_in = 0

