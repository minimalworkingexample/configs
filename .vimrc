


" to execute a line (or selected), try yy:@"

" notes on using vim {{{

" we'd like to use vim similarly to our use of emacs. 
" this would include: 
"   - explorer [:E] (dired)
"   - buffers [f5]
"   - bookmarks [create keybindings manually?]
" 
" let's figure out how to install vim plugins
" we'll try pathogen
"   1. put pathogen.vim in ~\.vim\autoload\
"   2. blah blah in ~\.vimrc
"   3. put plugins in ~\.vim\bundle\
" 

" }}}
" technicals {{{

" set nocompatible
runtime! ftdetect\*.vim
if has('win32') || has('win64')
    set runtimepath=~\.vim,$VIM/vimfiles,$VIMRUNTIME,$VIM/vimfiles/after,~\.vim/after
endif
execute pathogen#infect()
filetype plugin indent on

" }}}
" aesthetics{{{
" colors {{{

colorscheme xoria256
if ! exists("g:zenburn_high_Contrast")
  let g:zenburn_high_Contrast = 1 
endif
" color zenburn

" }}}
" gui, etc. {{{

set visualbell
set guicursor=n:blinkon0
set guioptions=e
syntax on
set wildmenu
set wildmode=longest:list,full
set laststatus=2
set clipboard=unnamed
" set clipboard=unnamedplus
set guifont=Consolas:h10
set nowrap

" }}}
" statusline {{{

" set statusline=%t       "tail of the filename
" set statusline+=[%{strlen(&fenc)?&fenc:'none'}, "file encoding
" set statusline+=%{&ff}] "file format
" set statusline+=%h      "help file flag
" set statusline+=%m      "modified flag
" set statusline+=%r      "read only flag
" set statusline+=%y      "filetype
" set statusline+=%=      "left/right separator
" set statusline+=%c,     "cursor column
" set statusline+=%l/%L   "cursor line/total lines
" set statusline+=\ %P    "percent through file
set wrapscan

" }}}
" }}}
" key-bindings {{{

inoremap jj <Esc>
nnoremap ; :
" nnoremap : ; 
let mapleader=","
set timeoutlen=2000
nnoremap <silent> <leader>ev :e $MYVIMRC<CR>
nnoremap <silent> <leader>em :e ~\ownCloud\main.txt<CR>
nnoremap <silent> <leader>et :e ~\ownCloud\tech.txt<CR>
nnoremap <silent> <leader>ei :e ~\AppData\Roaming\.emacs.d\init.el<CR>
nnoremap <space> za
nnoremap <leader>m :if &go=~#'m'<Bar>set go-=m<Bar>else<Bar>set go+=m<Bar>endif<CR>


" }}}
" buffers, files, nav {{{

nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

inoremap <A-h> <C-o>h
inoremap <A-j> <C-o>j
inoremap <A-k> <C-o>k
inoremap <A-l> <C-o>l

nnoremap <F5> :buffers<CR>:buffer<Space>
set hidden
" map <leader>n :bn<cr>
" map <leader>p :bp<cr>
" map <leader>d :bd<cr> 

" }}}
" search {{{

set incsearch     " show matches as search term is types
set hlsearch      " highlight search matches
" clear current search
nmap <silent> <leader>/ :nohlsearch<CR>

" }}}
" plugins {{{

au! BufRead,BufWrite,BufWritePost,BufNewFile *.org 
au BufEnter *.org call org#SetOrgFileType()
" autocmd! BufNewFile,BufRead *.org set wrap linebreak 

" }}}


" autocmd VimEnter * 'edit ~\'



" vim:foldmethod=marker:foldlevel=0
