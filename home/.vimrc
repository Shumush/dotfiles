execute pathogen#infect()
syntax on
let g:solarized_termtrans=1
set background=dark
colorscheme solarized

filetype plugin indent on

set ttyfast
set nocursorline
set nocursorcolumn

set tabstop=2
set softtabstop=2
set shiftwidth=2
set expandtab

map <C-Up> :CoqUndo<CR>
map <C-Down> :CoqNext<CR>
map <C-Right> :CoqToCursor<CR>
map <C-Left> :CoqLaunch<CR>
map <A-Left> :CoqKill<CR>

autocmd BufNewFile,BufRead *.shaun set ft=hjson
autocmd BufNewFile,BufRead *.sn set ft=hjson
