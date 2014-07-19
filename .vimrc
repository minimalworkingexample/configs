" vim: set fdm=marker nowrap: 

colo codeschool

" pathogen setup {{{

exe pathogen#infect()
syntax on
filetype plugin indent on

"}}}
" basic settings {{{

" guioptions, showcmd, lazyredraw
" hidden, autowrite, autochdir, modeline, 
" tabstop, shiftwidth, expandtab
set go= sc lz hid aw acd modeline
set ts=8 sw=8 et

" }}}
" netrw {{{

let g:netrw_banner=0
let g:netrw_list_hide= '.*\.swp$,.*\~'

" }}}
" easier default keys {{{

inoremap jj <Esc>
cnoremap jj <Esc>
nnoremap ; :
let mapleader=','
nnoremap <leader>2 :@"<CR>
nnoremap <space> za
cmap w!! w !sudo tee %
nnoremap <return> O<Esc>

" }}}
" wilds {{{

" wildmenu, wildmode, wildcharm, wildignore, wildignorecase
set wmnu wim=list:full
set wcm=<C-z>
set wig+=*~ wic

" }}}
" nav keys {{{

" window nav
nnoremap <leader>w <C-w>

nnoremap <S-left> <C-w>h
nnoremap <S-down> <C-w>j
nnoremap <S-up> <C-w>k
nnoremap <S-right> <C-w>l

" edit (file or netrw)
nnoremap <leader>e :edit <C-z><S-Tab>
nnoremap <leader>. :edit .<CR>

" buffers
nnoremap <leader>b :buffer <C-z><S-Tab>
nmap <C-n> :bn<cr>
nmap <C-p> :bp<cr>

" marks
nnoremap <leader>l :marks<CR>
nnoremap <leader>L :marks<CR>:normal! `

" find
set path+=/e/**
nnoremap <leader>f :find<Space>

" tabs
nnoremap <leader>t :tab<C-z>

" }}}
" settings keys {{{

" colorscheme switcher
nnoremap <leader>c :colorscheme <C-z><S-Tab>

" toggle menu, scroll bar
nnoremap <leader>m :if &go=~#'m'<Bar>set go-=m<Bar>else<Bar>set go+=m<Bar>endif<CR>
nnoremap <leader>r :if &go=~#'r'<Bar>set go-=r<Bar>else<Bar>set go+=r<Bar>endif<CR>

" Vim help re: word under cursor
nnoremap <leader>k :help <C-r><C-w><CR>
" better solution
au FileType vim setl kp=:help

" view help index
nnoremap <leader>h :help index.txt<CR>

" }}}
" plugins, autocmd, etc {{{

au! BufRead,BufWrite,BufWritePost,BufNewFile *.org 
au BufEnter *.org call org#SetOrgFileType()

aug cloudtxt
    au! BufRead,BufWrite,BufWritePost,BufNewFile *.txt 
    au BufEnter */ownCloud/*.txt call org#SetOrgFileType()
aug END

au BufRead,BufNewFile *.md set filetype=markdown

aug filetype_lisp
    au!
    " au FileType lisp setl fdm=marker fmr=;;;,;;+
    au FileType lisp setl fdm=expr fde=ELispLevel()
aug END

function! ELispLevel() 
    let n = len(matchstr(getline(v:lnum), '^;\+'))
    let l = n - 2
    if n >= 3
        return ">" . l
    else 
        return "=" 
    endif 
endfunction

" }}}
