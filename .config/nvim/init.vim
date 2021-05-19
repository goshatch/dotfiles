set nocompatible

source $HOME/.config/nvim/plugins.vim
source $HOME/.config/nvim/keymaps.vim

" True colours in term
set termguicolors
" Show line numbers
set number
" Theme
colorscheme gruvbox
set background=dark
" Use syntax coloring
syntax enable

" Auto indent
filetype plugin indent on
" Show existing tab with 2 spaces width
set tabstop=2
" When indenting with '>', use 2 spaces width
set shiftwidth=2
" On pressing tab, insert 4 spaces
set expandtab
" Set folding to manual
set foldmethod=manual
" Highlight cursor line
set cursorline
" For copying to system clipboard
set clipboard=unnamed

" ================= Speed stuff =====================
set ttyfast
set lazyredraw

" Improve fugitive-vim performance
" https://github.com/tpope/vim-fugitive/issues/1176
if has('mac')
  set shell=/bin/bash
endif

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
let g:ale_sign_error = 'X'
let g:ale_sign_warning = '!'
let g:ale_fixers = {
     \ 'javascript': ['eslint', 'prettier'],
     \ 'sass': ['prettier', 'stylelint'],
     \ 'scss': ['prettier', 'stylelint'],
     \ 'css': ['prettier', 'stylelint'],
     \ 'ruby': ['rubocop'],
     \ 'clojure': ['clj-kondo', 'joker'],
     \}

" Fix files automatically on save
"let g:ale_fix_on_save = 1

let g:ale_lint_on_text_changed = 'never'
let g:ale_sign_column_always = 1

" Airline extension
let g:airline#extensions#ale#enabled = 1

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

" Editorconfig
" Exclude Fugitive and SCP editing
let g:EditorConfig_exclude_patterns = ['fugitive://.\*', 'scp://.\*']
let g:EditorConfig_disable_rules = ['trim_trailing_whitespace']

" RSpec folding
let g:fold_rspec_foldenable = 0     " disables folding (toggle with `zi`)
let g:fold_rspec_foldlevel = 2      " sets initial open/closed state of all folds (open unless nested more than two levels deep)
let g:fold_rspec_foldclose = 'all'  " closes folds automatically when the cursor is moved out of them (only applies to folds deeper than 'foldlevel')
let g:fold_rspec_foldminlines = 3   " disables closing of folds containing two lines or fewer

" Rails projections using RSpec instead
let g:rails_projections = {
      \  'app/*.rb': {
      \     'alternate': 'spec/{}_spec.rb',
      \     'type': 'source'
      \   },
      \  'spec/*_spec.rb': {
      \     'alternate': 'app/{}.rb',
      \     'type': 'test'
      \   }
      \}

" Bring back old fzf popup position
let g:fzf_layout = { 'down': '40%' }
