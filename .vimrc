set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'

" All of your Plugins must be added before the following line
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

" List of plugins managed by Vundle
Plugin 'nathanaelkane/vim-indent-guides'
Plugin 'airblade/vim-gitgutter'
" Plugin 'altercation/vim-colors-solarized'
Plugin 'frankier/neovim-colors-solarized-truecolor-only'
Plugin 'scrooloose/nerdtree'
Bundle 'jistr/vim-nerdtree-tabs'
" Plugin 'godlygeek/tabular'
Plugin 'plasticboy/vim-markdown'
" Plugin 'andviro/flake8-vim'
Plugin 'Raimondi/delimitMate'
Plugin 'scrooloose/syntastic'
" Plugin 'pangloss/vim-javascript'
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'ctrlpvim/ctrlp.vim'
Plugin 'junegunn/goyo.vim'
" Plugin 'junegunn/limelight.vim'
" Plugin 'editorconfig/editorconfig-vim'
" Plugin 'mxw/vim-jsx'
Plugin 'tpope/vim-fugitive'
Plugin 'Valloric/MatchTagAlways'
Plugin 'terryma/vim-multiple-cursors'
Plugin 'tpope/vim-surround'

" leader to comma
let mapleader=","

" encoding
set encoding=utf-8

" Indentation
set expandtab
set shiftwidth=2
set softtabstop=2
set autoindent


" Set to auto read when a file is changed from the outside
set autoread

" Configure backspace so it acts as it should act
set backspace=eol,start,indent
set whichwrap+=<,>,h,l

" Ignore case when searching
set ignorecase

" When searching try to be smart about cases
set smartcase

" Highlight search results
set hlsearch

" Makes search act like search in modern browsers
set incsearch

" Show matching brackets when text indicator is over them
set showmatch

" Treat long lines as break lines (useful when moving around in them)
map j gj
map k gk

" Theme solarized dark
syntax enable
" set t_Co=256
colorscheme solarized
set termguicolors
set background=dark
" colorscheme solarized

set rnu!

" Ctrl C and V work as a human expects it?
vmap <C-c> "+yi<ESC>
vmap <C-x> "+c
vmap <C-v> c<ESC>"+p
imap <C-v> <C-r><C-o>+

" Styles netrw, file explorer
let g:netrw_liststyle=3

" <Ctrl-l> redraws the screen and removes any search highlighting.
nnoremap <silent> <C-l> :nohl<CR><C-l>

" <Ctrl-x> leaves vim in normal mode
nnoremap <C-x> <C-x><ESC><dd>

" No toolbar in gvim
:set guioptions-=T
:set guioptions-=m
:set guioptions-=r

" Ctrl-n opens Nerdtree
map <C-n> :NERDTreeTabsToggle<CR>
"
" Closes vim if only window left open is nerdtree
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif

" Ensures markdown files are read properly
let g:vim_markdown_folding_disabled=1
autocmd BufNewFile,BufRead *.md set filetype=markdown

" Indent guides show at startup. Only works in GVim, for some reason
let g:indent_guides_enable_on_vim_startup=1

" Sets font
set guifont=Input\ 11

" Highlight whitespace
:highlight ExtraWhitespace ctermbg=red guibg=red
:match ExtraWhitespace /\s\+$/

" Send to pi
" command Publish execute "!pandoc % -s -c buttondown.css -o %<.html"

" Statusline
set laststatus=2
let g:airline_powerline_fonts = 1
set timeoutlen=500
if !exists('g:airline_symbols')
    let g:airline_symbols = {}
endif

" unicode symbols
let g:airline_left_sep = '»'
let g:airline_left_sep = '▶'
let g:airline_right_sep = '«'
let g:airline_right_sep = '◀'
let g:airline_symbols.linenr = '␊'
let g:airline_symbols.linenr = '␤'
let g:airline_symbols.linenr = '¶'
let g:airline_symbols.branch = '⎇'
let g:airline_symbols.paste = 'ρ'
let g:airline_symbols.paste = 'Þ'
let g:airline_symbols.paste = '∥'
let g:airline_symbols.whitespace = 'Ξ'

" airline symbols
let g:airline_left_sep = ''
let g:airline_left_alt_sep = ''
let g:airline_right_sep = ''
let g:airline_right_alt_sep = ''
let g:airline_symbols.branch = ''
let g:airline_symbols.readonly = ''
let g:airline_symbols.linenr = ''

" F9 toggles paste
map <F9> :set paste!<CR>

" Switch between dark/light background
map <F12> :let &background = ( &background == "dark"? "light" : "dark" )<CR>

" CtrlP settings
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP'
set wildignore+=*/tmp/*,*.so,*.swp,*.zip     " MacOSX/Linux
let g:ctrlp_custom_ignore = '\v[\/]\.(git|hg|svn)$'

" Color name (:help cterm-colors) or ANSI code
" let g:limelight_conceal_ctermfg = 241

" Highlight line
set cursorline

" Highligt JSX also on .js files
let g:jsx_ext_required = 0

" Multiple cursors magic key is Ctrl+k
let g:multi_cursor_use_default_mapping=0
" Default mapping
let g:multi_cursor_next_key='<C-k>'
let g:multi_cursor_prev_key='<C-p>'
let g:multi_cursor_skip_key='<C-x>'
let g:multi_cursor_quit_key='<Esc>'

" Number of lines
set relativenumber
