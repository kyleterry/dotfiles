call pathogen#infect()

let mapleader=","
syntax on
filetype plugin indent on
filetype on
set encoding=utf-8
set t_Co=256
set number
set numberwidth=1
set title
set pastetoggle=<F2>
set confirm
colorscheme badwolf

" sudo go make me a sandwich
command! W :w
cmap W! w !sudo tee % >/dev/null

"searching
set ignorecase " Default to using case insensitive searches,
set smartcase " unless uppercase letters are used in the regex.
set smarttab " Handle tabs more intelligently
set hlsearch " Highlight searches by default.
set incsearch " Incrementally search while typing a /regex
nnoremap <leader><space> :nohlsearch<cr> " hide the matches (:noh)

" cursor
set cursorline
set cursorcolumn
set colorcolumn=79
"highlight CursorColumn ctermbg=0 cterm=NONE
"highlight CursorLine ctermbg=0 cterm=NONE
set ruler

" editing
set matchpairs+=<:>
set scrolloff=3 " Keep 3 context lines above and below the cursor
set backspace=2 " Allow backspacing over autoindent, EOL, and BOL
set showmatch " Briefly jump to a paren once it's balanced
set nowrap " don't wrap text
set linebreak " don't wrap textin the middle of a word
set autoindent " always set autoindenting on
set smartindent " use smart indent if there is no indent file
set tabstop=4 " <tab> inserts 4 spaces
set shiftwidth=4 " but an indent level is 2 spaces wide.
set softtabstop=4 " <BS> over an autoindent deletes both spaces.
set expandtab " Use spaces, not tabs, for autoindent/tab key.
set shiftround " rounds indent to a multiple of shiftwidth
set laststatus=2

" completion
set completeopt=menuone,longest,preview
set pumheight=6
autocmd CursorMovedI * if pumvisible() == 0|pclose|endif
autocmd InsertLeave * if pumvisible() == 0|pclose|endif

" ctrlp
let g:ctrlp_map = '<leader>f'

" NERDtree
map <leader>n :NERDTreeToggle<CR>

" ack
nmap <leader>a <Esc>:Ack!

" powerline
let g:Powerline_symbols = 'fancy'

" tasklist
map <leader>td <Plug>TaskList

" ctrl-jklm navigates around split windows
map <c-j> <c-w>j
map <c-k> <c-w>k
map <c-l> <c-w>l
map <c-h> <c-w>h
