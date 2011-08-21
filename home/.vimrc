" For great win!
let mapleader = ","
let maplocalleader = ","
"
set hidden "This allows vim to put buffers in the bg without saving, and then allows undoes when you fg them again.

set history=1000 "need more history
set number "what line is that?
set hlsearch "highlight them searches
set autoindent "cause i'm too lazy
set smartindent "smarter is better
set expandtab "hate tabs
set wildmenu " improved tab completion
set wildmode=list:longest " be bash like
set scrolloff=3 " This keeps three lines of context when scrolling
set title " so we know whats open
set smarttab "smarter is still better
set ts=4 "sane indent settings
set sw=4
set sts=4
set encoding=utf-8 "UTF-8 FOR LIFE
set relativenumber "much more useful
set laststatus=2
set statusline=%<%F%h%m%r%h%w%y\ %{&ff}\ %{strftime(\"%c\",getftime(expand(\"%:p\")))}%=\ lin:%l\,%L\ col:%c%V\ pos:%o\ ascii:%b\ %P
set ignorecase "case insens searches
set smartcase "figures it out!
set incsearch "incremental search is incremental
set showmatch "usability++
set undofile "unlimited undo!
set backspace=indent,eol,start
set linespace=3

" wrap settings for great readability
set wrap
set textwidth=79
set formatoptions=qrn1
set colorcolumn=85

" love me some current line highlighting
set cul
hi CursorLine term=none cterm=none ctermbg=8 

" Pathogen == teh awesomes
call pathogen#infect()
filetype off
syntax on
filetype plugin indent on

" legendary efficiency boost
nnoremap ; :

" remap copy paste
nmap <C-S-V> "+gP
imap <C-S-V> <ESC><C-S-V>i
vmap <C-S-C> "+y
set pastetoggle=<F2> " F2 toggles paste mode

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

" CTAGS - code completion is nice
set tags=tags;/
" load specific tags
so ~/.vim/tags/tags.vim
" create tags file for your project (includes local vars)
nnoremap <F12> :!ctags -R --sort=yes --c++-kinds=+pl --fields=+iaS --extra=+q .<CR>
so ~/.vim/autotag.vim

" OmniCppComplete
let OmniCpp_NamespaceSearch = 1
let OmniCpp_GlobalScopeSearch = 1
let OmniCpp_ShowAccess = 1
let OmniCpp_ShowPrototypeInAbbr = 1 " show function parameters
let OmniCpp_MayCompleteDot = 1 " autocomplete after .
let OmniCpp_MayCompleteArrow = 1 " autocomplete after ->
let OmniCpp_MayCompleteScope = 1 " autocomplete after ::
let OmniCpp_DefaultNamespaces = ["std", "_GLIBCXX_STD"]
" automatically open and close the popup menu / preview window
au CursorMovedI,InsertLeave * if pumvisible() == 0|silent! pclose|endif
set completeopt=menuone,menu,longest,preview


" C/C++ sweetness
" Switch between header and impl
nnoremap <F4> :A<CR>
" Switch (vert split) between header and impl
nnoremap <C-F4> :AV<CR>


" Colorscheme bullshittery:
" set t_Co=256 " this should be detected by vim automatically if your terminfo
" is correct
set background=dark
colors molokai

" Random commandline shortcuts
" Specify filetypes
au BufNewFile,BufRead *.i set filetype=swig

" Gundo settings
"nnoremap <F5> :GundoToggle<CR>

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
noremap U 30k
noremap D 30j

" buffer navigation
map <left> :bprev<CR>
map <right> :bnext<CR>

" Go specific settings
augroup golang
  au!
  au FileType go set noexpandtab
augroup END


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
