if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall | source $MYVIMRC
endif

call plug#begin('~/.vim/bundle')
""""""""""""""""""""""""""" Plugins """"""""""""""""""""""""""""""""""""""""""""
" Generics
" Plug 'scrooloose/nerdtree'
" Plug 'scrooloose/syntastic'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'rking/ag.vim'
Plug 'tomtom/tcomment_vim'
Plug 'ervandew/supertab'
Plug 'tpope/vim-surround'
Plug 'jiangmiao/auto-pairs'
Plug 'easymotion/vim-easymotion'

" Git integration
" Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'

" Plug 'MarcWeber/vim-addon-mw-utils'
" Plug 'tomtom/tlib_vim'

" Snippets collection
" Plug 'garbas/vim-snipmate' " VimL only
Plug 'sirver/ultisnips' " Requires Python
Plug 'honza/vim-snippets'

" Styling Plugins
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
" Plug 'altercation/vim-colors-solarized'
" Plug 'railscasts'
Plug 'tomasr/molokai'
Plug 'morhetz/gruvbox'

" C++ specific
Plug 'valloric/YouCompleteMe'", {'do': './install.py --ninja --clang-completer', 'for': ['cpp', 'h'] }
" , 'cs', 'rs', 'js' ] }
Plug 'rdnetto/YCM-Generator', { 'branch': 'stable'}
Plug 'rhysd/vim-clang-format'
Plug 'vim-scripts/DoxygenToolkit.vim'

" Ruby specifics
" Plug 'tpope/vim-rails'
" Plug 'tpope/vim-bundler'
" Plug 'tpope/vim-rake'
" Plug 'vim-ruby/vim-ruby'
" Plug 'skalnik/vim-vroom'

call plug#end()
" Put your non-Plugin stuff after this line

