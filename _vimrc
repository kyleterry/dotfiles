call pathogen#infect()

let mapleader=","
syntax on
filetype plugin indent on
filetype on
set encoding=utf-8
set t_Co=256
set relativenumber
set numberwidth=1
set title
set pastetoggle=<F2>
set confirm
set list
set listchars=eol:$,tab:>-,trail:~,extends:>,precedes:<
hi NonText ctermfg=7 guifg=gray
colorscheme badwolf

" kill whitespace at EOL
nnoremap <leader>S :%s/\s\+$//<cr>:let @/=''<CR>

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
map <leader>b :CtrlPBuffer<CR>
let g:ctrlp_custom_ignore = {
  \ 'dir':  '\v[\/]\.(git|hg|svn|collected_static)$',
  \ 'file': '\v\.(exe|so|dll|pyc|swp|swo)$',
  \ }

" NERDtree
map <leader>nt :NERDTreeToggle<CR>

" ack
nmap <leader>a <Esc>:Ack!

" powerline
"let g:Powerline_symbols = 'fancy'

" tasklist
map <leader>td <Plug>TaskList

" ctrl-jklm navigates around split windows
map <c-j> <c-w>j
map <c-k> <c-w>k
map <c-l> <c-w>l
map <c-h> <c-w>h

" pyflakes
let g:pyflakes_use_quickfix = 0

" virtualenv
let g:pymode_virtualenv = 1

" vim-pad
let g:pad_dir = '~/Documents/pad'

let g:slimv_swank_cmd = '! sbcl --load /usr/share/common-lisp/source/slime/start-swank.lisp &'

" based on:
" http://vim.1045645.n5.nabble.com/editing-Python-files-how-to-keep-track-of-class-membership-td1189290.html

function! s:get_last_python_class()
    let l:retval = ""
    let l:last_line_declaring_a_class = search('^class', 'bnW')
    let l:last_line_starting_with_a_word_other_than_class = search('^\ \(\<\)\@=\(class\)\@!', 'bnW')
    if l:last_line_starting_with_a_word_other_than_class < l:last_line_declaring_a_class
        let l:nameline = getline(l:last_line_declaring_a_class)
        let l:classend = matchend(l:nameline, 'class\s\+')
        let l:classnameend = matchend(l:nameline, 'class\s\+[A-Za-z0-9_]\+')
        let l:retval = strpart(l:nameline, l:classend, l:classnameend-l:classend)
    endif
    return l:retval
endfunction

function! s:get_last_python_def()
    let l:retval = ""
    let l:last_line_declaring_a_def = search('^\s*def', 'bnW')
    let l:last_line_starting_with_a_word_other_than_def = search('^\ \(\<\)\@=\(def\)\@!', 'bnW')
    if l:last_line_starting_with_a_word_other_than_def < l:last_line_declaring_a_def
        let l:nameline = getline(l:last_line_declaring_a_def)
        let l:defend = matchend(l:nameline, '\s*def\s\+')
        let l:defnameend = matchend(l:nameline, '\s*def\s\+[A-Za-z0-9_]\+')
        let l:retval = strpart(l:nameline, l:defend, l:defnameend-l:defend)
    endif
    return l:retval
endfunction

function! s:compose_python_location()
    let l:pyloc = s:get_last_python_class()
    if !empty(pyloc)
        let pyloc = pyloc . "."
    endif
    let pyloc = pyloc . s:get_last_python_def()
    return pyloc
endfunction

function! <SID>EchoPythonLocation()
    echo s:compose_python_location()
endfunction

command! PythonLocation :call <SID>EchoPythonLocation()
nnoremap <Leader>? :PythonLocation<CR>
