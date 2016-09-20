
call plug#begin('~/.vim/plugged')

Plug 'https://github.com/vim-scripts/a.vim.git'
Plug 'https://github.com/eparreno/vim-l9.git'
Plug 'https://github.com/nathanaelkane/vim-indent-guides.git'
Plug 'https://github.com/Shougo/vimproc.git', { 'do': 'make' }
Plug 'https://github.com/eagletmt/ghcmod-vim.git'
Plug 'https://github.com/godlygeek/tabular.git'
Plug 'https://github.com/scrooloose/nerdcommenter.git'
Plug 'https://github.com/tpope/vim-repeat.git'
Plug 'https://github.com/tpope/vim-surround.git'
Plug 'https://github.com/vim-latex/vim-latex.git'
Plug 'https://github.com/michaeljsmith/vim-indent-object.git'
Plug 'https://github.com/wlangstroth/vim-racket.git'
Plug 'https://github.com/rust-lang/rust.vim.git'
Plug 'https://github.com/vim-scripts/Efficient-python-folding.git'
Plug 'https://github.com/Konfekt/FastFold.git'
Plug 'https://github.com/octol/vim-cpp-enhanced-highlight.git'
Plug 'https://github.com/neovimhaskell/haskell-vim.git'
Plug 'https://github.com/bitc/vim-hdevtools.git'
Plug 'https://github.com/tpope/vim-vinegar.git'
"Plug 'https://github.com/Shougo/deoplete.nvim.git', { 'do': function('DoRemote') }
Plug 'https://github.com/mbbill/undotree.git'
Plug 'https://github.com/tpope/vim-obsession.git'

" Colorscheme bundles
Plug 'https://github.com/flazz/vim-colorschemes.git'
Plug 'https://github.com/chriskempson/base16-vim.git'
Plug 'https://github.com/morhetz/gruvbox.git'

call plug#end()

set termguicolors

" Tab settings
" set cindent
set autoindent
set smartindent
set expandtab
set tabstop=4
set shiftwidth=4 softtabstop=4

set t_Co=256

syntax on
filetype plugin on
filetype indent on

"colorscheme losh_molokai
let g:gruvbox_italics=1
let g:gruvbox_invert_selection=0
let g:gruvbox_contrast_dark='hard'
let g:gruvbox_contrast_light='soft'
colorscheme gruvbox
set background=dark

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

" No delay for escape key
set timeoutlen=1000 ttimeoutlen=0
" set autochdir

" Backup & Undo settings
set undodir=~/.vim/undodir
set backupdir=~/.vim/backup
set undofile
set undolevels=1000
set undoreload=10000

set ssop-=options   " do not store global and local values in session
set ssop-=folds     " do not store folds in the session

" Navigation of folded lines
map j gj
map k gk

" Quick navigation of tabs
map <Leader>th :tabprev<CR>
map <Leader>tl :tabnext<CR>
map <Leader>tn :tabnew<CR>
map <Leader>td :tabclose<CR>

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
map <Leader>f :!astyle --style=allman --indent=spaces=4 -N -S -w --add-one-line-brackets --convert-tabs --indent-col1-comments -m0 %<CR>

" Faster way to switch between splits
map <Leader>w <C-w>w
map <Leader>h <C-w>h
map <Leader>j <C-w>j
map <Leader>k <C-w>k
map <Leader>l <C-w>l

" Use extra tag files for python
autocmd FileType python set tags+=$HOME/.vim/tags/python.ctags

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

" Complete options (disable preview scratch window)
set completeopt=menu,menuone,longest

" Limit popup menu height
set pumheight=15

" Key Maps
" --------
" Toggle NERDTree with F2
map <F2> :NERDTreeToggle<CR>
" Use Y to copy until the end of the line. Use yy to copy the whole line.
nnoremap Y y$

" Some tricks for mutt
" F1 through F3 re-wraps paragraphs in useful ways
augroup MUTT
    au BufRead ~/.mutt/temp/mutt* set spell " <-- vim 7 required
    au BufRead ~/.mutt/temp/mutt* nmap  <F1>  gqap
    au BufRead ~/.mutt/temp/mutt* nmap  <F2>  gqqj
    au BufRead ~/.mutt/temp/mutt* nmap  <F3>  kgqj
    au BufRead ~/.mutt/temp/mutt* map!  <F1>  <ESC>gqapi
    au BufRead ~/.mutt/temp/mutt* map!  <F2>  <ESC>gqqji
    au BufRead ~/.mutt/temp/mutt* map!  <F3>  <ESC>kgqji
augroup END

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

let g:netrw_liststyle=3
let g:netrw_winsize=10
let g:netrw_alto=1

let g:deoplete#enable_at_startup = 1

nnoremap gb :ls<CR>:buffer<Space>
nnoremap gB :ls<CR>:sbuffer<Space>
nnoremap ,b :buffer *
nnoremap ,B :sbuffer *

let g:haskell_indent_where = 2
let g:haskell_indent_if = 0
let g:haskell_indent_in = 0
let g:haskell_indent_guard = 2
let g:haskell_enable_quantification = 1
let g:haskell_enable_recursivedo = 1
let g:haskell_enable_arrowsyntax = 1
let g:haskell_enable_pattern_synonyms = 1
let g:haskell_enable_typeroles = 1
let g:haskell_enable_static_pointers = 1
let g:haskell_classic_highlighting = 1

