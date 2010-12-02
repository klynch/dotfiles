"call pathogen#runtime_append_all_bundles()

set tabstop=2
set smarttab
set shiftwidth=2
set autoindent
set expandtab


set nowrap
set textwidth=80
set background=dark
set incsearch
set smartindent
set showcmd
set splitbelow
set nohlsearch
syntax on

"set statusline=%<%F%h%m%r%h%w%y\ %{&ff}\ %{strftime(\"%d/%m/%Y-%H:%M\")}%=\ col:%c%V\ ascii:%b\ pos:%o\ lin:%l\,%L\ %P
"set statusline=%<%F%h%m%r%h%w%y\ %{&ff}\ %{strftime(\"%c\",getftime(expand(\"%:p\")))}%=\ lin:%l\,%L\ col:%c%V\ pos:%o\ %ascii:%b\ %P
set statusline=%<%F%h%m%r%h%w%y\ %{&ff}\ %{strftime(\"%c\",getftime(expand(\"%:p\")))}%=\ lin:%l\,%L\ col:%c%V\ pos:%o\ %ascii:%b\ %P
set laststatus=2

let themeindex=0
map <F5> :make<Enter> 
map <F4> :noh<Enter> 
map <C-z> :set invnumber<Enter>

nnoremap <F2> :set invpaste paste?<CR>
set pastetoggle=<F2>
set showmode

map <C-y> ,#
map <C-t> ,$

map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l

map <C-f> {gq}

" lhs comments
map ,# :s/^/#/<CR>
map ,/ :s/^/\/\//<CR>
map ,> :s/^/> /<CR>
map ," :s/^/\"/<CR>
map ,% :s/^/%/<CR>
map ,! :s/^/!/<CR>
map ,; :s/^/;/<CR>
map ,- :s/^/--/<CR>
map ,c :s/^\/\/\\|^--\\|^> \\|^[#"%!;]//<CR> 

au BufNewFile,BufRead *.rhtml set syn=eruby
au BufNewFile,BufRead *.rsel set syn=ruby
au BufNewFile,BufRead *.erb set syn=eruby
au BufNewFile,BufRead *.tex set syn=tex
augroup syntax
    autocmd!
    autocmd BufRead,BufNewFile *.template setfiletype cpp
augroup end

augroup commenting
    autocmd!
    " comment
    autocmd FileType ruby,python,perl,sh map ,# :s/^/#/<CR>
    autocmd FileType c,cpp map ,# :s/^/\/\//<CR>
    autocmd FileType tex,matlab map ,# :s/^/%/<CR>
    " uncomment
    autocmd FileType ruby,python,perl,sh map ,$ :s/^#//<CR>
    autocmd FileType tex,matlab map ,$ :s/^%//<CR>
    autocmd FileType c,cpp map ,$ :s/^\/\///g<CR>
augroup end

augroup formating
    autocmd!
    autocmd BufRead,BufNewFile Makefile* set noexpandtab
    autocmd BufRead *.py set noexpandtab
    autocmd BufRead,BufNewFile *.txt set nosmartindent
    autocmd BufRead,BufNewFile *.m set filetype=matlab
augroup end

