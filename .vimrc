" Display line number
set number

" Highlight matching paren
set showmatch

" In windows, change '\' in the file path to '/'
set shellslash

" Syntax highlight
syntax on

" Tabs and indentation
set expandtab " Expand tabs to spaces
set tabstop=4 " The width of the space recognized as tab
set shiftwidth=4 " Auto indent width
set softtabstop=2 " Number of spaces to enter when pressing the tab key

" swp, un~, ~ file
"set noswapfile " Don't create swap file
set nobackup " Don't create backup file
set noundofile " Don't create undo file

" search
set hlsearch " Highlight search result
set ignorecase " Case insensitive
set wrapscan

" Display invisible chars
set list
set listchars=tab:^\ ,trail:-
autocmd VimEnter,WinEnter * match IdeographicSpace /　/ " Highlight zenkaku space

" Enable backspace in insert mode
set backspace=indent,eol,start

" Color settings
autocmd Colorscheme * hi Visual ctermbg=lightgray ctermfg=black
autocmd Colorscheme * hi SpecialKey term=underline ctermbg=darkblue ctermfg=lightgray
autocmd Colorscheme * hi IdeographicSpace term=underline ctermbg=darkmagenta
colorscheme default
