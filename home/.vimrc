set mouse=
" number of space chars inserted for indendation
set shiftwidth=4
" insert spaces instead of \t
set expandtab
" number of chars inserted when tab key pressed
set tabstop=4

set ignorecase
set smartcase
set incsearch hlsearch " highlight matches
set showmatch " highlight matched brackets when inserting

filetype on
filetype plugin indent on

set cursorline
"set cursorcolumn

set background=dark
colorscheme ir_black
syntax on

" invis chars
set listchars=trail:.,tab:>-,eol:$
set nolist

set backspace=indent,eol,start
set number
set matchpairs+=<:>

" Make cursor move by visual lines instead of file lines (when wrapping)
map <up> gk
map k gk
imap <up> <C-o>gk
map <down> gj
map j gj
imap <down> <C-o>gj
map E ge

" never see ^M again! (DOS text files)
autocmd BufRead * silent! %s/^M$//

" Host specific
if filereadable(expand("~/.vimrc.local"))
  source ~/.vimrc.local
endif


