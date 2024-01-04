call plug#begin()
Plug 'sheerun/vim-polyglot'
Plug 'tpope/vim-fugitive'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
call plug#end()

set nocompatible

syntax enable
colorscheme lunaperche

filetype plugin indent on

set laststatus=2
set t_Co=256
set encoding=utf-8
set autoindent
set magic
set number
set scrolloff=3
set sidescroll=3
set ruler
set cc=80
set nowrap
set ignorecase
set smartcase
set splitbelow
set hidden
set notimeout
set incsearch
set showmatch
set hlsearch
set mouse=a
set noswapfile
set nofoldenable
set termguicolors
set lazyredraw

imap jj <Esc>
imap jk <Esc>
