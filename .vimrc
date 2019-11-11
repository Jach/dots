nnoremap <F2> :set invpaste paste?<CR>
imap <F2> <C-O><F2>
set pastetoggle=<F2>
set hidden " keep buffers open without necessarily displaying them
" note to self, <leader> is typically \...
cnoreabbrev wq w<bar>bd
cnoreabbrev q bfirst<bar>bd#
" note: now you must use :quit to quit...

set incsearch
set ignorecase
set smartcase
set scrolloff=2
set wildmode=longest,list
set showcmd
"set tw=80
set runtimepath+=/home/kevin/.vim/marc-plugins/vim-addon-manager
" activate the addons called 'vim-addon-manager', 'JSON', 'name1', 'name2'
" This adds them to runtimepath and ensures that plugin/* and after/plugin/*
" files are sourced
call scriptmanager#Activate(['vim-addon-manager','JSON',"vim-addon-fcsh"])
filetype plugin indent on
map \b i\textbf{<ESC>ea}<ESC>
"map \p i(<ESC>ea)<ESC>
map \t i&lt;<ESC>ea&gt;<ESC>
map \tt :tabnew<cr>
map \tm :tabmove
"map \tn :tabnext<cr>
"map \tp :tabprevious<cr>
"I can't unmap these from my fingers! Trying to use buffers...
map \bn :bnext<cr>
map \bp :bprev<cr>
map \tn :bnext<cr>
map \tp :bprev<cr>
"map \gc :w<cr>:!git commit -a -m "kevin@`date`"
map \gc :w<cr>:!git commit -a -m "
map \sp :setlocal spell spelllang=en_us<cr>
imap \itemz \begin{itemize} \end{itemize}<Esc>Bba
" zg - add word to dict
" z= - bring up list of suggestions
" zug - undo add to word
" zw - mark as wrong
" zuw - undo wrong
" @@ or @: - repeat last colon command
imap jk <Esc>
imap zz <C-X><C-O>
set nohls
set tabstop=2
set shiftwidth=2
set softtabstop=2
set expandtab
map \tabs2 :set tabstop=2<CR>:set shiftwidth=2<CR>:set softtabstop=2
map \tabs4 :set tabstop=4<CR>:set shiftwidth=4<CR>:set softtabstop=4
map \tabs5 :set tabstop=5<CR>:set shiftwidth=5<CR>:set softtabstop=5
set grepprg=grep\ -nH\ $*
let g:tex_flavor='latex'
"set nu
"syntax on " ubuntu specific
"au BufAdd,BufNewFile * nested tab sball
"map \gt :tab split<CR>:exec("tag ".expand("<cword>"))<CR>
"map \tags :!ctags -R --c++-kinds=+pl --fields=+iaS --extra=+q . <CR>
"ctags -R --language-force=lisp ~/quicklisp/dists/quicklisp/software
set tags+=./tags;/
set tags+=~/.vim/tags/cpp
set tags+=~/.vim/tags/gl
set tags+=~/.vim/tags/sdl
set tags+=~/.vim/tags/qt4
"set tags+=~/.vim/tags/quicklisp

au BufNewFile,BufRead *.mxml set filetype=mxml
au BufNewFile,BufRead *.as set filetype=actionscript
au BufNewFile,BufRead *.tpl set filetype=php
au BufNewFile,BufRead *.js set filetype=javascript
au BufNewFile,BufRead *.tex set tw=80
au BufNewFile,BufRead *.cljs set filetype=clojure

map \cbase i#include <stdio.h><ESC>2o<ESC>iint main(void) {<ESC>2o<ESC>i  return 0;<ESC>o<ESC>i}<ESC>
map \jbase ipublic class FileName {<ESC>2o<ESC>i  public static void main(String args[]) {<ESC>3o<ESC>i  }<ESC>o<ESC>i}<ESC>
map \cppbase i#include <iostream><ESC>2o<ESC>iusing namespace std;<ESC>2o<ESC>iint main(int argc, char* argv[]) {<ESC>2o<ESC>i  return 0;<ESC>2o<ESC>i}<ESC>

map \gpp :!g++ <C-R>=expand("%:t")<cr><cr>
"set makeprg=g++\ %

" OmniCppComplete
let OmniCpp_NamespaceSearch = 1
let OmniCpp_GlobalScopeSearch = 1
let OmniCpp_ShowAccess = 1
let OmniCpp_ShowPrototypeInAbbr = 1 " show function parameters
let OmniCpp_MayCompleteDot = 1 " autocomplete after .
let OmniCpp_MayCompleteArrow = 1 " autocomplete after ->
let OmniCpp_MayCompleteScope = 1 " autocomplete after ::
let OmniCpp_DefaultNamespaces = ["std", "_GLIBCXX_STD"]
" automatically open and close the popup menu / preview window
"au CursorMovedI,InsertLeave * if pumvisible() == 0|silent! pclose|endif
"set completeopt=menuone,menu,longest,preview
"set ofu=syntaxcomplete#Complete

" Pydiction
let g:pydiction_location = '/home/kevin/.vim/pydiction-1.2/complete-dict'
" let g:pydiction_menu_height = 20 (default 15)

" Clever tab completion
fun! OmniComplete()
  let left = strpart(getline('.'), col('.') - 2, 1)
  if left =~ "^$"
    return ""
  elseif left =~ ' $'
    return ""
  else
    return "\<C-x>\<C-o>"
  endfun
"inoremap <silent> <S-Tab> <C-R>=OmniComplete()<CR>

au BufWinLeave *? silent! mkview "no silent
au BufWinEnter *? silent! loadview "no !
command! Rmview execute "!rm -i ~/.vim/view/~=+" . substitute(substitute(expand('%:p'), "/home/kevin/", "", ""), "/", "=+", "g") . "="

"call ToggleRaibowParenthesis
"call rainbow_parenthsis#LoadRound()
"call rainbow_parenthsis#LoadSquare()
"call rainbow_parenthsis#Activate()
let vimclojure#HighlightBuiltins = 1
let vimclojure#ParenRainbow = 1
let vimclojure#FuzzyIndent = 1

set maxmem=104857600
set maxmemtot=104857600
set maxmempattern=300000

execute pathogen#infect()
Helptags

" line:
nmap <c-c><c-l> <Plug>SlimeLineSend
nmap \in ain-<ESC>wi'<ESC>$a)<ESC><s-v><Plug>SlimeRegionSend<ESC>uuuh
" form:
nmap <c-c><c-f> v%<Plug>SlimeRegionSend<ESC>
" move to nearest ( then form:
nmap <c-c><c-h> [(v%<Plug>SlimeRegionSend<ESC>
" macroexpand-1 form:
nmap <c-c><c-m> i(macroexpand-1 '<ESC>l%a)<ESC>%v%<Plug>SlimeRegionSend<ESC>uu
"(use :reload-all 'my-project.core)
" #_ to toss out next read form

augroup myvimrc
  au!
  au BufWritePost .vimrc,_vimrc,vimrc,.gvimrc,_gvimrc,gvimrc so $MYVIMRC | if has('gui_running') | so $MYGVIMRC | endif
augroup END

"nnoremap <F3> :NumbersOnOff<cr>

fun! ISlamhound()
  write
  set autoread
  silent execute "!lein slamhound '%:p'"
  redraw!
  set autoread<
endfun

command! Slamhound call ISlamhound()

command! Kibit execute "r!lein kibit '%:p'"

" linter stuff
let g:syntastic_always_populate_loc_list = 1

"map \ln :lnext<cr>
"map \lp :lprev<cr>
" If there's only one error, these fail, use :ll. :/
" Also :lopen :lclose
" Now fixing \ln \lp
fun! LocFix(cmd)
  redir => output
  silent! exec a:cmd
  redir END
  if match(output, "E553: No more items") >= 0
    exec ':ll'
  else
    echom output
  endif
endfun
map \ln :call LocFix(":lnext")<CR>
map \lp :call LocFix(":lprev")<cr>
let g:syntastic_cpp_checkers = ['cppcheck', 'gcc']
let g:syntastic_c_checkers = ['cppcheck', 'splint', 'gcc']
let g:syntastic_asciidoc_checkers = []
let g:syntastic_python_checkers = ['pylint'] " seemed to have changed default to flake8, boo

let g:sexp_enable_insert_mode_mappings = 0

"nim:
fun! JumpToDef()
  if exists("*GotoDefinition_" . &filetype)
    call GotoDefinition_{&filetype}()
  else
    exe "norm! \<C-]>"
  endif
endf

map \ngt :tab split<CR>:call JumpToDef()<CR>
"endNim

" slimv
let g:paredit_mode=0
let g:lisp_rainbow=1
" doc in slimv.txt but most useful:
" ,s describes symbol
" ,h looks up symbol in hyperspec
" ,d evaluates form
" ,D compiles DeFun
" ,L compile and load current file
" ,F compiles file
" ,b eval buffer
" ,u undefine function
" ,e evals current exp
" ,r evals 'region' or selection
" ,g set-package
" ctrl+x + 0 => close form
" ,1 macroexpand-1
" ,m macroexpand all
" ,B set breakpoint
" ,l disassemble
" ,a abort
" ,v eval-in-debug-frame. e.g. (swank-backend:restart-frame idx)
"      (swank-backend:activate-stepping frame) (swank-backend:sldb-step-into) (swank-backend:sldb-step-next) (swank-backend:sldb-step-out)
" ,i inspect frame
" ,n continue
" ,- clear repl
" ,y interrupt repl
"    ,xc          Who Calls
"    ,xr          Who References
"    ,xs          Who Sets
"    ,xb          Who Binds
"    ,xm          Who Macroexpands
"    ,xp          Who Specializes
"    ,xl          List Callers
"    ,xe          List Callees
let g:slimv_repl_split=2
let g:slimv_repl_split_size=20
" ctags -R -a --language-force=lisp ~/quicklisp/dists/quicklisp/software
"let g:slimv_unmap_tab=1
"let g:slimv_ctags='ctags -R -a --language-force=lisp *'
let g:slimv_swank_cmd='!xterm -e sbcl --dynamic-space-size 10000 --load /home/kevin/.vim/bundle/slimv/slime/start-swank.lisp &'
" when using cross ref, jump to file name printed in repl
" and move it to other window, then switch back to repl
map \jf gf<cr>:call WindowSwap#EasyWindowSwap()<cr><C-w>w:call WindowSwap#EasyWindowSwap()<cr><C-w>w<C-o><C-o>
" note that ctrl+] for def (works for make-instance args)
" ctrl+t to jump back, ctrl+o/ctrl+i to toggle back/forth...

"let g:vlime_compiler_policy = {"DEBUG": 3}

let g:large_file = 1024 * 1024 * 1024 " 1 GB is 'large'
" don't make swap file for large files
autocmd BufReadPre * let f=expand("<afile>") | if getfsize(f)
            \ > g:large_file | set noswapfile | endif

map \justify gwip
"map \table :TableModeToggle
"already \tm
" use || for boundary row
let g:table_mode_corner='+'
let g:table_mode_corner_corner='+'
let g:table_mode_header_fillchar='='

set wildignore+=*.swp,*.fasl,*.o
let g:ctrlp_custom_ignore = {
  \ 'dir':  '\v[\/](\.(git|hg|svn)|quicklisp)$',
  \ 'file': '\v\.(exe|so|dll|o)$',
  \ 'link': 'some_bad_symbolic_links',
  \ }

function! SlimvXrefSysDependsOn()
    call SlimvXrefBase( 'System Depends On: ', ':sys-depends-on' )
endfunction

function! SlimvXrefEditUses()
    call SlimvXrefBase( 'Who Edits/Uses: ', ':edit-uses' )
endfunction

map ,xd :call SlimvXrefSysDependsOn()<cr>

map ,xx :call SlimvXrefEditUses()<cr>

function! StripTrailingWhitespace()
  normal mZ
  %s/\s\+$//e
  if line("'Z") != line(".")
    echo "Stripped whitespace\n"
  endif
  normal `Z
endfunction
autocmd BufWritePre * :call StripTrailingWhitespace()

set autochdir
