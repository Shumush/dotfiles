syntax on
filetype plugin indent on

"
set hidden "This allows vim to put buffers in the bg without saving, and then allows undoes when you fg them again.
set history=1000 "Longer history
set number
set hlsearch
set autoindent
set smartindent
set expandtab
set wildmenu
set wildmode=list:longest
set scrolloff=3 " This keeps three lines of context when scrolling
set title
set expandtab
set smarttab
set ts=4
set sw=4
set sts=4
set expandtab
set encoding=utf-8
set relativenumber
set laststatus=2
set statusline=%<%F%h%m%r%h%w%y\ %{&ff}\ %{strftime(\"%c\",getftime(expand(\"%:p\")))}%=\ lin:%l\,%L\ col:%c%V\ pos:%o\ ascii:%b\ %P
set ignorecase
set smartcase
set incsearch
set showmatch
set undofile
set backspace=indent,eol,start
set linespace=3

set wrap
set textwidth=79
set formatoptions=qrn1
set colorcolumn=85

" remap copy paste
nmap <C-S-V> "+gP
imap <C-S-V> <ESC><C-S-V>i
vmap <C-S-C> "+y

" disable arrow keys
nnoremap <up> <nop>
nnoremap <down> <nop>
nnoremap <left> <nop>
nnoremap <right> <nop>
inoremap <up> <nop>
inoremap <down> <nop>
inoremap <left> <nop>
inoremap <right> <nop>
nnoremap j gj
nnoremap k gk

"save on loss of focus
"au FocusLost * :wa

let mapleader = ","

" match bracket pairs with tab
nnoremap <tab> %
vnoremap <tab> %
" The following two lines are for highlighting lines which are too long
":au BufWinEnter *c,*.cpp,*.h,*.py let w:m1=matchadd('Search', '\%<101v.\%>97v', -1)
":au BufWinEnter *c,*.cpp,*.h,*.py,*.rb,*.mxml,*.as let w:m2=matchadd('ErrorMsg', '\%>110v.\+', -1)


" Mappings:
map <F6> :b#<CR>
map <C-n> :noh<CR>
map <C-Space> <C-x><C-o>

" Taglist stuff
nmap ,tu :!(ctags *.[ch])&<CR><CR>
map ,tl :TlistToggle<CR>
let Tlist_Exit_OnlyWindow = 1


" Colorscheme bullshittery:
set t_Co=16
set background=dark
colors zenburn

" Random commandline shortcuts
"
" latex build + evince ps view
" This one is weird...what is with the :t:r.tex?
"nmap ,tex :!(texbuildps.py %:t:r.tex)<CR><CR>
nmap ,tex :!(texbuildps.py %)<CR><CR>

" Specify filetypes
au BufNewFile,BufRead *.i set filetype=swig

" For pydiction:
let g:pydiction_location='~/.vim/pydiction-1.2/complete-dict'

" Gundo settings
nnoremap <F5> :GundoToggle<CR>

" NERDTree settings
let g:NERDTreeChDirMode=2
let g:NERDChristmasTree=1

" Stupid NERDCommenter warning
let NERDShutUp=1

" Better close functionality
nmap ,fc :call CleanClose(1)<CR>
nmap ,fq :call CleanClose(0)<CR>

function! CleanClose(tosave)
  if (a:tosave == 1)
    w!
  endif
let todelbufNr = bufnr("%")
let newbufNr = bufnr("#")
if ((newbufNr != -1) && (newbufNr != todelbufNr) && buflisted(newbufNr))
    exe "b".newbufNr
else
    bnext
endif

if (bufnr("%") == todelbufNr)
    new
endif
exe "bd".todelbufNr
endfunction

" Audio bell == annoying
set vb t_vb=

" More ruby settings
let ruby_space_settings = 1
highlight ExtraWhitespace ctermbg=green guibg=green
match ExtraWhitespace /\s\+$/
autocmd BufWinEnter * match ExtraWhitespace /\s+$/
autocmd InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
autocmd InsertLeave * match ExtraWhitespace /\s\+$/
autocmd BufWinLeave * call clearmatches()

" Make it easier to move around through blocks of text:
noremap <C-J> gj
noremap <C-k> gk
noremap U 30k
noremap D 30j

" Go specific settings
augroup golang
  au!
  au FileType go set noexpandtab
augroup END

" Pathogen == teh awesomes
call pathogen#runtime_append_all_bundles()

" Ack >> grep
nnoremap <leader>a :Ack

" A command to delete all trailing whitespace from a file.
command DeleteTrailingWhitespace %s:\(\S*\)\s\+$:\1:


" begin VIM Clojure
if has("win32") || has("win64")
    let windows=1
    let vimfiles=$HOME . "/vimfiles"
    let sep=";"
else
    let windows=0
    let vimfiles=$HOME . "/.vim"
    let sep=":"
endif


let classpath = join(
   \[".",
   \ "src", "src/main/clojure", "src/main/resources",
   \ "test", "src/test/clojure", "src/test/resources",
   \ "classes", "target/classes",
   \ "lib/*", "lib/dev/*",
   \ "bin",
   \ vimfiles."/lib/*",
   \ $HOME."/.clojure/*"
   \],
   \ sep)
" Settings for VimClojure
let vimclojureRoot = vimfiles."/bundle/vimclojure-2.2.0"
let vimclojure#HighlightBuiltins=1
let vimclojure#HighlightContrib=1
let vimclojure#DynamicHighlighting=1
let vimclojure#ParenRainbow=1
let vimclojure#WantNailgun = 1
let vimclojure#NailgunClient = vimclojureRoot."/lib/nailgun/ng"
if windows
    " In stupid windows, no forward slashes, and tack on .exe
    let vimclojure#NailgunClient = substitute(vimclojure#NailgunClient, "/", "\\", "g") . ".exe"
endif

" Start vimclojure nailgun server (uses screen.vim to manage lifetime)
nmap <silent> <Leader>sc :execute "ScreenShell java -cp \"" . classpath . sep . vimclojureRoot . "/lib/*" . "\" vimclojure.nailgun.NGServer 127.0.0.1" <cr>
" Start a generic Clojure repl (uses screen.vim)
nmap <silent> <Leader>sC :execute "ScreenShell java -cp \"" . classpath . "\" clojure.main" <cr>
