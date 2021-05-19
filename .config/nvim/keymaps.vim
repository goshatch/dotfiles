" REMINDER:
" This is how mapping works in Vim https://stackoverflow.com/a/3776182
" map = map key sequence to command
" noremap = map but not recursively
" prefix (n = normal, v = visual, etc) indicates mode

" Leader
let mapleader = ' '
nnoremap <silent> <leader> :WhichKey '<Space>'<CR>

" Esc with double j
imap jj <Esc>
imap jk <Esc>

" Clear search pattern highlight by hitting ESC
nnoremap <ESC> :noh<RETURN><ESC>

" Open in the current directgory
nmap <leader>ew :e <C-R>=expand('%:h').'/'<cr>
nmap <leader>es :sp <C-R>=expand('%:h').'/'<cr>
nmap <leader>ev :vsp <C-R>=expand('%:h').'/'<cr>
nmap <leader>et :tabe <C-R>=expand('%:h').'/'<cr>

nnoremap <Leader><Space> :Files<CR>
nnoremap <Leader>p :Rg<CR>
nnoremap <Leader>b :Buffers<CR>

nnoremap <Leader>HH :History<CR>
nnoremap <Leader>HS :History/<CR>
nnoremap <Leader>HC :History:<CR>

nnoremap <Leader>tb :BTags<CR>
nnoremap <Leader>tT :Tags<CR>
nnoremap <Leader>tt :TagbarToggle<CR>

nnoremap <Leader>w :Windows<CR>

nnoremap <Leader>m :Marks<CR>

nnoremap <Leader>n :NERDTreeToggle<CR>
nnoremap <Leader>nn :NERDTreeFind<CR>

nnoremap <Leader>gg :vertical Git<CR>
nnoremap <Leader>gd :Gdiff<CR>
nnoremap <Leader>gb :Git blame<CR>
nnoremap <Leader>gf :GFiles<CR>
nnoremap <Leader>gc :Commits<CR>
nnoremap <Leader>gC :BCommits<CR>

nnoremap <Leader>cc :Commentary<CR>

nnoremap <Leader>cf <Plug>(ale_fix)

nnoremap <Leader>Pm :MarkdownPreviewToggle<CR>

" ALE (Linting)
" https://github.com/w0rp/ale
nmap <silent> <C-k> <Plug>(ale_previous_wrap)
nmap <silent> <C-j> <Plug>(ale_next_wrap)
