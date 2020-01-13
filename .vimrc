set nocompatible
syntax sync ccomment
colorscheme elflord

" ==================
" Key mapping
" ==================
map <F10> :set encoding=koi8-r<CR>
map <F11> :set encoding=8bit-cp1251<CR>
map <F12> :set encoding=utf-8<CR>

" ==================
" Vundle section
" ==================
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'VundleVim/Vundle.vim'
Plugin 'othree/html5.vim'
Plugin 'elixir-lang/vim-elixir'
Plugin 'ctrlpvim/ctrlp.vim'
Plugin 'elmcast/elm-vim'
"Plugin 'w0rp/ale' 				# vim8+ or neovim
call vundle#end()
filetype plugin indent on

" ==================
" Setting per FileType
" ==================
autocmd FileType text setlocal textwidth=78
autocmd FileType erlang setlocal expandtab
autocmd FileType java setlocal expandtab
autocmd FileType nix setlocal expandtab
autocmd FileType asciidoc setlocal expandtab
autocmd FileType yaml setlocal ts=2 sts=2 sw=2 expandtab
autocmd FileType elixir setlocal ts=2 sts=2 sw=2 expandtab

autocmd BufWritePre *.ex %s/\s\+$//e
autocmd BufWritePre *.eex %s/\s\+$//e
autocmd BufWritePre *.leex %s/\s\+$//e

autocmd BufWritePre *.exs %s/\s\+$//e
autocmd BufWritePre *.erl %s/\s\+$//e
autocmd BufWritePre *.hrl %s/\s\+$//e
autocmd BufWritePre Makefile %s/\s\+$//e

"autocmd BufWritePost *.exs silent :!mix format %
"autocmd BufWritePost *.ex silent :!mix format %

" ------------------
" End of setting per FileType
" ------------------

autocmd BufReadPost *
  \ if line("'\"") > 0 && line("'\"") <= line("$") |
  \   exe "normal g`\"" |
  \ endif

" ==================
" Settings for screen title
" ==================

autocmd BufEnter * let &titlestring = hostname() . " " . expand("%:p")
set title
set titlelen=70
set titleold=""
set titlestring=%<%F%=%l/%L-%P
set redrawtime=10000
" ------------------
" End of settings for title
" ------------------

"set autoindent
set smartindent
set smarttab

set ignorecase					" use \C to Force a pattern to be case sensitive
set smartcase					" scs: override 'ignorecase' when pattern has upper case characters

set textwidth=120
set colorcolumn=120				" right line after at 120th column

set autoread					" ar: automatically read a file when it was modified outside of Vim
set backspace=indent,eol,start
set backup
set breakat=\ \	!*+;,?			" brk: which characters might cause a line break
set gdefault
set guifont=8x13 
set history=50
set hlsearch
set incsearch
set linebreak
set magic						" change the way backslashes are used in search patterns
set modified
set noswapfile
set nolinebreak					" nolbr: wrap long lines at a character in 'breakat'
set ruler
set shiftwidth=4
set showcmd
set showmatch
set startofline
set tabstop=4
set viminfo='50,\"1000,:50,/50,%,n~/.viminfo
set wrapscan					" ws: search commands wrap around the end of the buffer
set wildmenu					" wmnu: command-line completion shows a list of matches
set nomodeline

set enc=utf-8
set termencoding=utf-8

" Switch syntax highlighting on, when the terminal has colors
" Also switch on highlighting the last used search pattern.
if &t_Co > 2 || has("gui_running")
  syntax on
endif
