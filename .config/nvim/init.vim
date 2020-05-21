" Plugin management with Plug
" https://github.com/junegunn/vim-plug
call plug#begin('~/.local/share/nvim/plugged')
" The only themes that matter
Plug 'nanotech/jellybeans.vim'
Plug 'lifepillar/vim-solarized8'
Plug 'ajh17/Spacegray.vim'
Plug 'morhetz/gruvbox'
" Languages support
Plug 'sheerun/vim-polyglot'
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
" Eunuch
Plug 'tpope/vim-eunuch'
" Airline
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
" NerdCommenter
Plug 'scrooloose/nerdcommenter'
" GitGutter
Plug 'airblade/vim-gitgutter'
" CSS colours preview
Plug 'ap/vim-css-color'
" http://editorconfig.org
Plug 'editorconfig/editorconfig-vim'
call plug#end()

" True colours in term
set termguicolors
" Show line numbers
set number
" Theme
colorscheme gruvbox

" Leader
let mapleader = ','

" Esc with double j
imap jj <Esc>

nmap <leader>ew :e <C-R>=expand('%:h').'/'<cr>
nmap <leader>es :sp <C-R>=expand('%:h').'/'<cr>
nmap <leader>ev :vsp <C-R>=expand('%:h').'/'<cr>
nmap <leader>et :tabe <C-R>=expand('%:h').'/'<cr>

" Auto indent
filetype plugin indent on
" show existing tab with 2 spaces width
set tabstop=2
" when indenting with '>', use 2 spaces width
set shiftwidth=2
" On pressing tab, insert 4 spaces
set expandtab
" Set folding to manual
set foldmethod=manual

" Highlight cursor line
set cursorline

" ================= Speed stuff =====================
set ttyfast
set lazyredraw

" This is for faster syntax highlight in ruby files:
" https://stackoverflow.com/a/16920294
set re=1

" Another tweak for Ruby
let g:ruby_path = system('echo $HOME/.rbenv/shims')
" ==================================================

" GitGutter
let g:gitgutter_realtime = 1
let g:gitgutter_eager = 1
let g:gitgutter_sign_removed = '-'
highlight link GitGutterAdd DiffAdd
highlight link GitGutterChange DiffChange
highlight link GitGutterDelete DiffDelete
highlight link GitGutterChangeDelete GitGutterChange


" Strip trailing whitespaces on save
function! StripTrailingWhitespaces()
  let l = line(".")
  let c = col(".")
  %s/\s\+$//e
  call cursor(l, c)
endfunction
autocmd BufWritePre * :call StripTrailingWhitespaces()

" ALE (Linting)
" https://github.com/w0rp/ale
nmap <silent> [c <Plug>(ale_previous_wrap)
nmap <silent> ]c <Plug>(ale_next_wrap)
let g:ale_sign_error = 'X'
let g:ale_sign_warning = '!'
let g:ale_fixers = {
     \ 'javascript': ['eslint', 'prettier'],
     \ 'sass': ['prettier', 'stylelint'],
     \ 'scss': ['prettier', 'stylelint'],
     \ 'css': ['prettier', 'stylelint'],
     \ 'ruby': ['rubocop'],
     \}

" Fix files automatically on save
"let g:ale_fix_on_save = 1

let g:ale_lint_on_text_changed = 'never'
let g:ale_sign_column_always = 1

" Airline extension
let g:airline#extensions#ale#enabled = 1

"nmap <F6> <Plug>(ale_fix)

" Fuzzy finder
" Install `fzf` with brew first
" https://github.com/junegunn/fzf.vim
nnoremap <C-p> :Files<CR>
nnoremap <Leader>b :Buffers<CR>
nnoremap <Leader>h :History<CR>
nnoremap <Leader>t :BTags<CR>
nnoremap <Leader>T :Tags<CR>

" Coc (completion)
" https://github.com/neoclide/coc.nvim/wiki/Completion-with-sources
" use <tab> for trigger completion and navigate to the next complete item
function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~ '\s'
endfunction

inoremap <silent><expr> <Tab>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<Tab>" :
      \ coc#refresh()

" For copying to system clipboard
set clipboard=unnamed

" Editorconfig
" Exclude Fugitive and SCP editing
let g:EditorConfig_exclude_patterns = ['fugitive://.\*', 'scp://.\*']
let g:EditorConfig_disable_rules = ['trim_trailing_whitespace']

" Nerd Commenter
" Align line-wise comment delimiters flush left instead of following code indentation
let g:NERDDefaultAlign = 'left'

