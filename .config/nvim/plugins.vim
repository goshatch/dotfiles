" Plugin management with Plug
" https://github.com/junegunn/vim-plug

call plug#begin('~/.local/share/nvim/plugged')
" Those Gruvbox colours™
Plug 'morhetz/gruvbox'

" Tagbar, a bar for showing tags
Plug 'preservim/tagbar'

" === Languages & Frameworks ===
Plug 'sheerun/vim-polyglot'

" Rust
Plug 'rust-lang/rust.vim'

" Clojure
Plug 'Olical/conjure', { 'tag': 'v4.3.1' }

" Elixir, Phoenix
Plug 'elixir-editors/vim-elixir'

" Ruby & Rails
Plug 'rlue/vim-fold-rspec'
Plug 'tpope/vim-rails'
Plug 'tpope/vim-endwise'

" StyledComponents
Plug 'styled-components/vim-styled-components', { 'branch': 'main' }

" Linting
Plug 'w0rp/ale'

" Fuzzy finder
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'

" Ferret
Plug 'wincent/ferret'

" Autocomplete
Plug 'neoclide/coc.nvim', {'do': { -> coc#util#install()}}

" Git
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-rhubarb'
Plug 'airblade/vim-gitgutter'

" Various UNIX commands
Plug 'tpope/vim-eunuch'

" Airline
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" NerdTree
Plug 'preservim/nerdtree'
Plug 'Xuyuanp/nerdtree-git-plugin'

" CSS colours preview
Plug 'ap/vim-css-color'

" http://editorconfig.org
Plug 'editorconfig/editorconfig-vim'

" Dash support
Plug 'rizzatti/dash.vim'

" (Un)commenting
Plug 'tpope/vim-commentary'

" Advanced project management
Plug 'tpope/vim-projectionist'

" Complementary pairs of mappings
Plug 'tpope/vim-unimpaired'

" Help remembering key mappings
Plug 'liuchengxu/vim-which-key'

" Markdown preview
Plug 'iamcco/markdown-preview.nvim', { 'do': 'cd app && yarn install'  }
call plug#end()

