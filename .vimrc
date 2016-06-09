set nocompatible              " be iMproved, required

" Include Vim-Plug's manager config
source ~/.vim/plug.vimrc

""""""""""" NERDTree config """"""""""""
" map <C-n> :NERDTreeToggle<CR>

""""""""""" YouCompleteMe """"""""""""""
let g:ycm_global_ycm_extra_conf = '~/.vim/bundle/YouCompleteMe/third_party/ycmd/cpp/ycm/.ycm_extra_conf.py'
let g:airline#extensions#ycm#enabled = 1 " Airline integration
" make YCM compatible with UltiSnips (using supertab)
let g:ycm_key_list_select_completion = ['<C-n>', '<Down>']
let g:ycm_key_list_previous_completion = ['<C-p>', '<Up>']
let g:SuperTabDefaultCompletionType = '<C-n>'

""""""""""""" UltiSnips """"""""""""""""
set runtimepath+=~/.vim/mySnippets/
let g:UltiSnipsEditSplit="vertical"

" better key bindings for UltiSnipsExpandTrigger
let g:UltiSnipsExpandTrigger = "<tab>"
let g:UltiSnipsJumpForwardTrigger = "<tab>"
let g:UltiSnipsJumpBackwardTrigger = "<s-tab>"



""""""""""" Doxygen ToolKit """"""""""""
" let g:DoxygenToolkit_briefTag_pre="@Synopsis    "
" let g:DoxygenToolkit_paramTag_pre="@Param    "
" let g:DoxygenToolkit_returnTag="@Returns    "
" let g:DoxygenToolkit_blockHeader="============================================="
" let g:DoxygenToolkit_blockFooter="============================================="
let g:DoxygenToolkit_authorName=
    \ "Christodoulopoulos Nikolaos (nchristod)"
let g:DoxygenToolkit_licenceTag="MIT"
" Doxygen syntax auto enable for C, C++, c#, IDL, PHP
let g:load_doxygen_syntax=1

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
" colorscheme railscasts
colorscheme molokai

""""""""""" Clang-Format """"""""""""""
let g:clang_format#command = "clang-format-3.7"
let g:clang_format#style_options = {
            \ "AccessModifierOffset" : -4,
            \ "AllowShortIfStatementsOnASingleLine" : "false",
            \ "AlwaysBreakTemplateDeclarations" : "true",
            \ "Standard" : "C++11",
            \ "UseTab" : "Never",
            \ "IndentWidth" : 4,
            \ "BreakBeforeBraces" : "Allman",
            \ "IndentCaseLabels" : "false",
            \ "ColumnLimit" : 100}

" map to <Leader>cf in C++ code
autocmd FileType c,cpp,objc nnoremap <buffer><Leader>cf :<C-u>ClangFormat<CR>
autocmd FileType c,cpp,objc vnoremap <buffer><Leader>cf :ClangFormat<CR>

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

"Remove all trailing whitespace by pressing F5
nnoremap <F5> :let _s=@/<Bar>:%s/\s\+$//e<Bar>:let @/=_s<Bar><CR>

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
  set guifont=Literation\ Mono\ Powerline\ 8
endif
