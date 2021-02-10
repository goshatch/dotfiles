" Plugin management with Plug
" https://github.com/junegunn/vim-plug
call plug#begin('~/.local/share/nvim/plugged')
" The only themes that matter
Plug 'nanotech/jellybeans.vim'
Plug 'lifepillar/vim-solarized8'
Plug 'ajh17/Spacegray.vim'
Plug 'cormacrelf/vim-colors-github'
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
" NerdTree
Plug 'preservim/nerdtree'
Plug 'Xuyuanp/nerdtree-git-plugin'
" GitGutter
Plug 'airblade/vim-gitgutter'
" CSS colours preview
Plug 'ap/vim-css-color'
" http://editorconfig.org
Plug 'editorconfig/editorconfig-vim'
" VimWiki + Zettelkasten + TaskWarrior
Plug 'vimwiki/vimwiki'
Plug 'michal-h21/vim-zettel'
" StyledComponents
Plug 'styled-components/vim-styled-components', { 'branch': 'main' }
" Dash support
Plug 'rizzatti/dash.vim'
" Auto fold RSpec examples
Plug 'rlue/vim-fold-rspec'
call plug#end()

" True colours in term
set termguicolors
" Show line numbers
set number
" Theme
colorscheme gruvbox

set nocompatible
syntax on

" Leader
let mapleader = ' '

" Esc with double j
imap jj <Esc>

" REMINDER:
" This is how mapping works in Vim https://stackoverflow.com/a/3776182
" map = map key sequence to command
" noremap = map but not recursively
" prefix (n = normal, v = visual, etc) indicates mode

" Clear search pattern highlight by hitting ESC
nnoremap <ESC> :noh<RETURN><ESC>

" Open in the current directgory
nmap <leader>ew :e <C-R>=expand('%:h').'/'<cr>
nmap <leader>es :sp <C-R>=expand('%:h').'/'<cr>
nmap <leader>ev :vsp <C-R>=expand('%:h').'/'<cr>
nmap <leader>et :tabe <C-R>=expand('%:h').'/'<cr>

" Fuzzy finder
" Install `fzf` first
" https://github.com/junegunn/fzf.vim
nnoremap <Leader><Space> :Files<CR>
nnoremap <Leader>p :Rg<CR>
nnoremap <Leader>b :Buffers<CR>
nnoremap <Leader>h :History<CR>
nnoremap <Leader>t :BTags<CR>
nnoremap <Leader>T :Tags<CR>

nnoremap <Leader>n :NERDTreeToggle<CR>
nnoremap <Leader>nn :NERDTreeFind<CR>

nnoremap <Leader>tt :TagbarToggle<CR>

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
" set re=1
" Commented this out for better performance in TypeScript 2020/12/03

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

" Automatically create directories on save if needed
augroup Mkdir
  autocmd!
  autocmd BufWritePre * call mkdir(expand("<afile>:p:h"), "p")
augroup END

" ALE (Linting)
" https://github.com/w0rp/ale
nmap <silent> <C-k> <Plug>(ale_previous_wrap)
nmap <silent> <C-j> <Plug>(ale_next_wrap)
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
" Add spaces after comment delimiters by default
let g:NERDSpaceDelims = 1
" Allow commenting and inverting empty lines (useful when commenting a region)
let g:NERDCommentEmptyLines = 1
" Enable trimming of trailing whitespace when uncommenting
let g:NERDTrimTrailingWhitespace = 1

" VimWiki (+ VimZettel, + TaskWiki)
" Use Markdown instead of VimWiki syntax
let g:vimwiki_list = [{'path': '~/vimwiki/', 'syntax': 'markdown', 'ext': '.md'}]
let g:zettelkasten = '~/vimwiki/zettel/'
nnoremap <Leader><Enter> :VimwikiToggleListItem<CR>

" Change format of default Zettel file name
let g:zettel_format = '%y%m%d-%H%M-%title'

" Keymaps
nnoremap <Leader>zn :ZettelNew<space>
nnoremap <Leader>zo :ZettelOpen<CR>
nnoremap <Leader>zb :ZettelBackLinks<CR>
nnoremap <Leader>zi :ZettelInbox<CR>
nnoremap <Leader>zgl :ZettelGenerateLinks<CR>
nnoremap <Leader>zgt :ZettelGenerateTags<CR>

" RSpec folding
let g:fold_rspec_foldenable = 0     " disables folding (toggle with `zi`)
let g:fold_rspec_foldlevel = 2      " sets initial open/closed state of all folds (open unless nested more than two levels deep)
let g:fold_rspec_foldclose = 'all'  " closes folds automatically when the cursor is moved out of them (only applies to folds deeper than 'foldlevel')
let g:fold_rspec_foldminlines = 3   " disables closing of folds containing two lines or fewer

" Bring back old fzf popup position
let g:fzf_layout = { 'down': '40%' }
