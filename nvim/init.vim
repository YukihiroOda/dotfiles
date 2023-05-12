" Ward off unexpected things that your distro might have made, as
" well as sanely reset options when re-sourcing .vimrc
set nocompatible

" Set Dein base path (required)
let s:dein_base = '/home/yukihiro/.cache/dein'

" Set Dein source path (required)
let s:dein_src = '/home/yukihiro/.cache/dein/repos/github.com/Shougo/dein.vim'

" Set Dein runtime path (required)
execute 'set runtimepath+=' . s:dein_src

" Call Dein initialization (required)
call dein#begin(s:dein_base)

call dein#add(s:dein_src)

" Your plugins go here:
call dein#add('airblade/vim-gitgutter')
call dein#add('gianarb/coc-grammarly')
call dein#add('justmao945/vim-clang')
call dein#add('neoclide/coc.nvim', {'merged':0, 'rev': 'release'})
call dein#add('rhysd/vim-grammarous')
call dein#add('Shougo/neosnippet.vim')
call dein#add('Shougo/neosnippet-snippets')
call dein#add('tpope/vim-rails')
call dein#add('tpope/vim-surround')
call dein#add('tpope/vim-unimpaired')
call dein#add('vim-latex/vim-latex')

"
" Finish Dein initialization (required)
call dein#end()

" Attempt to determine the type of a file based on its name and possibly its
" contents. Use this to allow intelligent auto-indenting for each filetype,
" and for plugins that are filetype specific.
if has('filetype')
  filetype indent plugin on
endif

" Enable syntax highlighting
if has('syntax')
  syntax on
endif

" Uncomment if you want to install not-installed plugins on startup.
"if dein#check_install()
" call dein#install()
"endif

" To prohibit using arrow keys
noremap <Up> <Nop>
noremap <Down> <Nop>
noremap <Left> <Nop>
noremap <Right> <Nop>
" %% as %:h
cnoremap <expr> %% getcmdtype() == ':' ? expand('%:h').'/':'%%'
" Display line numbers
set number
" Colorscheme 
syntax enable
colorscheme darkblue
"matchit-------------------
filetype plugin on
runtime macros/matchit.vim
"nrformats
set nrformats=
