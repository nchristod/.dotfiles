set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'

""""""""""""""""""""""""""" Examples """""""""""""""""""""""""""""""""""""""""""
" The following are examples of different formats supported.
" Keep Plugin commands between vundle#begin/end.
" plugin on GitHub repo
" Plugin 'tpope/vim-fugitive'
" plugin from http://vim-scripts.org/vim/scripts.html
" Plugin 'L9'
" Git plugin not hosted on GitHub
" Plugin 'git://git.wincent.com/command-t.git'
" git repos on your local machine (i.e. when working on your own plugin)
" Plugin 'file:///home/gmarik/path/to/plugin'
" The sparkup vim script is in a subdirectory of this repo called vim.
" Pass the path to set the runtimepath properly.
" Plugin 'rstacruz/sparkup', {'rtp': 'vim/'}
" Avoid a name conflict with L9
" Plugin 'user/L9', {'name': 'newL9'}

""""""""""""""""""""""""""" Plugins """"""""""""""""""""""""""""""""""""""""""""
Plugin 'tpope/vim-fugitive'
" Plugin 'scrooloose/nerdtree'
Plugin 'scrooloose/syntastic'
Plugin 'ctrlpvim/ctrlp.vim'
Plugin 'railscasts'
Plugin 'tpope/vim-rails'
Plugin 'tpope/vim-bundler'
Plugin 'tpope/vim-rake'
Plugin 'vim-ruby/vim-ruby'
Plugin 'airblade/vim-gitgutter'
Plugin 'skalnik/vim-vroom'
Plugin 'rking/ag.vim'

Plugin 'MarcWeber/vim-addon-mw-utils'
Plugin 'tomtom/tlib_vim'
Plugin 'garbas/vim-snipmate'
" Snippets collection
Plugin 'honza/vim-snippets'

"
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
"Plugin 'altercation/vim-colors-solarized'
Plugin 'tomasr/molokai'
Plugin 'tpope/vim-surround'
Plugin 'jiangmiao/auto-pairs'
Plugin 'ervandew/supertab'
Plugin 'easymotion/vim-easymotion'
Plugin 'tomtom/tcomment_vim'

Plugin 'valloric/YouCompleteMe'

"""""""" All of your Plugins must be added before the following line """""""""""
call vundle#end()            " required
filetype plugin indent on    " required
" To ignore plugin indent changes, instead use:
"filetype plugin on
"
" Brief help
" :PluginList       - lists configured plugins
" :PluginInstall    - installs plugins; append `!` to update or just :PluginUpdate
" :PluginSearch foo - searches for foo; append `!` to refresh local cache
" :PluginClean      - confirms removal of unused plugins; append `!` to auto-approve removal
"
" see :h vundle for more details or wiki for FAQ
" Put your non-Plugin stuff after this line

""""""""""" NERDTree config """"""""""""
" map <C-n> :NERDTreeToggle<CR>

""""""""""" YouCompleteMe """"""""""""""
let g:ycm_global_ycm_extra_conf = '~/.vim/bundle/YouCompleteMe/third_party/ycmd/cpp/ycm/.ycm_extra_conf.py'
let g:airline#extensions#ycm#enabled = 1 " Airline integration

""""""""""" CTRL-P config """"""""""""""
let g:ctrlp_show_hidden = 1
let g:ctrlp_custom_ignore = {
  \ 'dir': '\v[\/](\.(git|hg|svn)|\_site)$',
  \ 'file': '\v\.(exe|so|o|dll|class|png|jpg|jpeg)$'
\}

let g:ctrlp_working_path_mode = 'r'
let g:ctrlp_root_markers = [
  \ 'pom.xml', '.p4ignore', '*.sublime-project',
  \ '*.jucer'
\]

set wildignore+=*/tmp/*,*.so,*.swp,*.zip

""""""""""" Syntastic config """""""""""
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

"""""""""" Airlinebar Config """"""""""""
let g:airline#extensions#tabline#enabled = 1
let g:airline_theme='luna'
let g:airline_powerline_fonts=1
set laststatus=2

"""""""""" Ag Config """""""""""""""""""
" let g:agprg="<custom-ag-path-goes-here> --vimgrep" " Specify custom ag name and path
let g:ag_working_path_mode="r" " Start searching from the project root instead of cwd

"""""""""" Theme - colorscheme """""""""
set background=dark
colorscheme railscasts

""""""""""""" Formating """"""""""""""""
set tabstop=2 " number of spaces for tab character
set softtabstop=4 " number of spaces when tabbing
set shiftwidth=2
set autoindent
set smarttab
set expandtab " turns tab to spaces

"""""""""""" Visual """""""""""""""""""
syntax enable
syntax on
set showmatch " show matching brackets
set lazyredraw " redraw the screen only when its needed

"""""""""""" Other """"""""""""""""""""
set encoding=utf-8
set ruler
set nu " Line numbers on
set clipboard+=unnamed " Yanks go on clipboard
set nowrap " Line wrapping off

" Make it obvious where 80 characters is
set textwidth=80
set colorcolumn=+1

" Search
set incsearch           " search as characters are entered
set hlsearch            " highlight matches

" Autocompletion
set wildmode=longest,list,full
set wildmenu

" Storing the backup files
if isdirectory($HOME . '/.vim/backup') == 0
        :silent !mkdir -p ~/.vim/backup >/dev/null 2>&1
endif
set backupdir-=.
set backupdir+=.
set backupdir-=~/
set backupdir^=~/.vim/backup/
set backupdir^=./.vim-backup/

" Store the swp files somewhere convinient.
" If you have a .vim-swap in the current dir it'll use that
" Otherwise, it saves it to ~/.vim/swap, ~/tmp or .
if isdirectory($HOME . '/.vim/swap') == 0
        :silent !mkdir -p ~/.vim/swap >/dev/null 2>&1
endif
set directory=./.vim-swap//
set directory+=~/.vim/swap//
set directory+=~/tmp//
set directory+=.

" Store the state of the previous editing session
set viminfo+=n~/.vim/viminfo

if exists("+undofile")
        " undofile - This allows you to use undos after exiting and restarting
        " :help undo-persistence
        " needs Vim 7.3+
        if isdirectory($HOME . '/.vim/undo') == 0
                :silent !mkdir -p ~/.vim/undo > /dev/null 2>&1
        endif
        set undodir=./.vim-undo//
        set undodir+=~/.vim/undo//
        set undofile
endif

" Open new split panes to right and bottom, which feels more natural
set splitbelow
set splitright

" From Thoughtbot dotfiles
augroup vimrcEx
  autocmd!

  " When editing a file, always jump to the last known cursor position.
  " Don't do it for commit messages, when the position is invalid, or when
  " inside an event handler (happens when dropping a file on gvim).
  autocmd BufReadPost *
    \ if &ft != 'gitcommit' && line("'\"") > 0 && line("'\"") <= line("$") |
    \   exe "normal g`\"" |
    \ endif

  " Set syntax highlighting for specific file types
  autocmd BufRead,BufNewFile Appraisals set filetype=ruby
  autocmd BufRead,BufNewFile *.md set filetype=markdown

  " Enable spellchecking for Markdown
  autocmd FileType markdown setlocal spell

  " Automatically wrap at 80 characters for Markdown
  autocmd BufRead,BufNewFile *.md setlocal textwidth=80

  " Automatically wrap at 72 characters and spell check git commit messages
  autocmd FileType gitcommit setlocal textwidth=72
  autocmd FileType gitcommit setlocal spell

  " Allow stylesheets to autocomplete hyphenated words
  autocmd FileType css,scss,sass setlocal iskeyword+=-
augroup END

" Fonts
if has('gui_running')
  set guifont=Literation\ Mono\ Powerline\ 10
endif
