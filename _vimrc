let g:pathogen_disable = ["vim-sleuth", "sleuth"]
call pathogen#infect()
set nocompatible

let mapleader=","
syntax on
filetype plugin indent on
filetype on
set encoding=utf-8
"set t_Co=256
"set term=screen-256color
set relativenumber
set number
set numberwidth=1
set title
set pastetoggle=<F2>
set confirm
set list
" disable mouse in neovim
set mouse-=a
"set listchars=eol:$,tab:>-,trail:~,extends:>,precedes:<
set listchars=trail:·,tab:»·,eol:$
set ttyfast
"hi nontext ctermfg=0 guifg=gray
let g:gitgutter_override_sign_column_highlight = 0
set background=dark
let g:gruvbox_italic=1
colorscheme gruvbox
let &t_ZH="\e[3m"
let &t_ZR="\e[23m"
let t_ZH="[3m"
let t_ZR="[23m"

set guioptions-=L
set guioptions-=R

" Directories for vim-created files
set directory=~/.vimtmp
set backupdir=~/.vimtmp

" kill whitespace at EOL
nnoremap <leader>S :%s/\s\+$//<cr>:let @/=''<CR>

" sudo go make me a sandwich
command! W :w
cmap W! w !sudo tee % >/dev/null

" completion
set omnifunc=syntaxcomplete#Complete
let g:neocomplete#enable_at_startup = 1

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
nnoremap <Leader>c :set cursorline! cursorcolumn!<CR>
set ruler

" editing
set matchpairs+=<:>
set scrolloff=3 " Keep 3 context lines above and below the cursor
set backspace=2 " Allow backspacing over autoindent, EOL, and BOL
set showmatch " Briefly jump to a paren once it's balanced
set nowrap " don't wrap text
set linebreak " don't wrap textin the middle of a word
set autoindent " always set autoindenting on
"set smartindent " use smart indent if there is no indent file
set tabstop=4 " <tab> inserts 4 spaces
set shiftwidth=4 " but an indent level is 2 spaces wide.
set softtabstop=4 " <BS> over an autoindent deletes both spaces.
set expandtab " Use spaces, not tabs, for autoindent/tab key.
set shiftround " rounds indent to a multiple of shiftwidth
set laststatus=2

" tabs
nnoremap <C-Left> :tabprevious<CR>
nnoremap <C-Right> :tabnext<CR>
nnoremap <silent> <A-Left> :execute 'silent! tabmove ' . (tabpagenr()-2)<CR>
nnoremap <silent> <A-Right> :execute 'silent! tabmove ' . tabpagenr()<CR>

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
" let NERDTreeDirArrows=0
let NERDTreeIgnore = ['\.pyc$']
nmap <leader>nn :NERDTreeFocus<CR>

" ack
nmap <leader>a <Esc>:Ack!
let g:ackprg = 'ag --nogroup --nocolor --column'

" Airline settings

if !exists('g:airline_symbols')
    let g:airline_symbols = {}
endif

"let g:airline_powerline_fonts = 1
let g:airline_theme='gruvbox'
let g:airline_left_sep = ''
let g:airline_left_alt_sep = ''
let g:airline_right_sep = ''
let g:airline_right_alt_sep = ''
let g:airline_symbols.branch = ''
let g:airline_symbols.readonly = ''
let g:airline_symbols.linenr = ''

" unicode symbols
" let g:airline_left_sep = ''
" let g:airline_right_sep = ''
let g:airline_symbols.linenr = '␤'
" let g:airline_symbols.branch = '⎇'
let g:airline_symbols.paste = '✹'
let g:airline_symbols.whitespace = '¶'
" let g:airline_solarized_bg = 'light'

let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#show_buffers = 1
let g:airline#extensions#tabline#show_tab_type = 0
let g:airline#extensions#tabline#close_symbol = '✖'
let g:airline#extensions#tabline#formatter = 'unique_tail_improved'
let g:airline#extensions#tabline#tab_nr_type = 1

let g:airline#extensions#tabline#left_sep = ''
let g:airline#extensions#tabline#left_alt_sep = ''
let g:airline#extensions#tabline#right_sep = ''
let g:airline#extensions#tabline#right_alt_sep = ''

" tasklist
map <leader>td <Plug>TaskList

" ctrl-jklm navigates around split windows
map <c-j> <c-w>j
map <c-k> <c-w>k
map <c-l> <c-w>l
map <c-h> <c-w>h

" Tagbar
nmap <leader>tb :TagbarToggle<CR>
nmap <leader>tf :TagbarOpen j<CR>

" pyflakes
let g:pyflakes_use_quickfix = 0

" virtualenv
let g:pymode_virtualenv = 1
let g:pymode_run_key = '<leader>r'

let g:pymode_rope_lookup_project = 0

" vim-pad
let g:pad_dir = '~/Documents/pad'

" Debugging
"let g:vdebug_options['port'] = "9050"

let g:slimv_swank_cmd = '! sbcl --load /usr/share/common-lisp/source/slime/start-swank.lisp &'

" mutt
au BufRead /tmp/mutt-* set tw=72

" markdown
au BufRead,BufNewFile *.markdown,*.md,*.txt setlocal spell textwidth=80

" scratchpad (set spell and autosave)
au BufRead,BufNewFile *.scratch setlocal spell textwidth=80
au BufRead,BufNewFile *.scratch let g:auto_save = 1

" Git commits
au BufNewFile,BufRead COMMIT_EDITMSG set spell

" move text and rehighlight -- vim tip_id=224
vnoremap > ><CR>gv
vnoremap < <<CR>gv 

" C programming
" TODO

" Go programming
let g:tagbar_type_go = {
    \ 'ctagstype' : 'go',
    \ 'kinds'     : [
        \ 'p:package',
        \ 'i:imports:1',
        \ 'c:constants',
        \ 'v:variables',
        \ 't:types',
        \ 'n:interfaces',
        \ 'w:fields',
        \ 'e:embedded',
        \ 'm:methods',
        \ 'r:constructor',
        \ 'f:functions'
    \ ],
    \ 'sro' : '.',
    \ 'kind2scope' : {
        \ 't' : 'ctype',
        \ 'n' : 'ntype'
    \ },
    \ 'scope2kind' : {
        \ 'ctype' : 't',
        \ 'ntype' : 'n'
    \ },
    \ 'ctagsbin'  : 'gotags',
    \ 'ctagsargs' : '-sort -silent'
\ }
set rtp+=$GOPATH/src/github.com/golang/lint/misc/vim
let g:go_fmt_command = "goimports"
let g:syntastic_go_checkers = ['golint', 'govet', 'errcheck']
au FileType go nmap <leader>r <Plug>(go-run)
au FileType go nmap <leader>b <Plug>(go-build)
au FileType go nmap <leader>t <Plug>(go-test)
au FileType go nmap <leader>c <Plug>(go-coverage)

" sql
au FileType sql nmap <leader>sf :SQLUFormatStmts<CR>

" ruby, html, json and stuff
autocmd FileType json,ruby,eruby,haml,yaml,coffee,js,javascript,html,css,sass,gitconfig setlocal expandtab shiftwidth=2 softtabstop=2 tabstop=2

" python
autocmd FileType python setlocal expandtab shiftwidth=4 softtabstop=4 tabstop=4

" gpg
command -nargs=1 WriteEncrypted w !gpg -c -o <q-args>

" csv
let g:csv_delim=','

" snippets
imap <C-\> <Plug>snipMateNextOrTrigger
smap <C-\> <Plug>snipMateNextOrTrigger

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
