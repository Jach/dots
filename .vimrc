set sessionoptions-=options " don't let sessions capture all global options including 'runtimepath'

" no system beep:
set vb t_vb=
set hidden " when set on, keep buffers open without necessarily displaying them, or switching them without having to save them

" looking for a more intuitive way to handle closing buffers when the
" slime repl is one of them...
" Though :q is convenient, just going to train myself to :bd instead. or :Bd
" now with vim-bbye.
"cnoreabbrev wq w<bar>bd
"cnoreabbrev q bfirst<bar>bd#

" note: now you must use :quit to quit...

set incsearch " incremental highlighting
set ignorecase " case-insensitive search...
set smartcase " ...unless search string has a capital letter
set wildmode=longest,list " tab completion behavior when opening files
set wildignore+=*.swp,*.fasl,*.o " don't show these in tab completed list
set showcmd " ensures chording/command preview is shown as you type
set scrolloff=2 " scrolling starts two lines from bottom instead of bottom
"set tw=80 " textwidth 80 chars, auto-insert line breaks
"set colorcolumn=160 " visible color column at character offset to remind about long lines
autocmd ColorScheme * highlight VertSplit cterm=NONE ctermfg=None ctermbg=58
set linebreak " for display only; instead of line-wrapping at the last character of the line, breaks the line at a space or other custom char
"set formatoptions+=t
"set runtimepath+=/home/kevin/.vim/marc-plugins/vim-addon-manager
" activate the addons called 'vim-addon-manager', 'JSON', 'name1', 'name2'
" This adds them to runtimepath and ensures that plugin/* and after/plugin/*
" files are sourced
"call scriptmanager#Activate(['vim-addon-manager','JSON',"vim-addon-fcsh"])
filetype plugin indent on
"syntax on " ubuntu specific

set nohls " no search highlights after searching
set expandtab " spaces instead of tabs like a civilized person
set tabstop=2
set shiftwidth=2
set softtabstop=2
" the only valid spacings are 2 and 4!
map \tabs2 :set tabstop=2<CR>:set shiftwidth=2<CR>:set softtabstop=2
map \tabs4 :set tabstop=4<CR>:set shiftwidth=4<CR>:set softtabstop=4
imap jk <Esc>

"set number relativenumber " relative line numbers, but with current line being the real line

map \b i\textbf{<ESC>ea}<ESC>
"map \p i(<ESC>ea)<ESC>
map \t i&lt;<ESC>ea&gt;<ESC>
map \tm :tabmove
"map \tn :tabnext<cr>
"map \tp :tabprevious<cr>
"I can't unmap these from my fingers! Trying to use buffers...
map \bn :bnext<cr>
map \bp :bprev<cr>
map \bb :b#<cr>
map \tn :bnext<cr>
map \tp :bprev<cr>

map \q :Bwipeout<cr>
map \q! :Bwipeout!<cr>
" I probably want to start getting in the habit of using :bw / wipeout instead
" of delete, because otherwise the file can remain open in the jumplist and
" lead to some confusing behavior.

"map \gc :w<cr>:!git commit -a -m "kevin@`date`"
map \gc :w<cr>:!git commit -a -m "
map \sp :setlocal spell spelllang=en_us<cr>
imap \itemz \begin{itemize} \end{itemize}<Esc>Bba
"imap zz <C-X><C-O>
"useless shortcut that has only ever gotten in my way...
map \tabs5 :set tabstop=5<CR>:set shiftwidth=5<CR>:set softtabstop=5
set grepprg=grep\ -nH\ $*
let g:tex_flavor='latex'
"set nu
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
au BufNewFile,BufRead *.tex set tw=160
au BufNewFile,BufRead *.md set tw=160 " enforce text width
au BufNewFile,BufRead *.txt set tw=160
au BufNewFile,BufRead *.cljs set filetype=clojure
au BufNewFile,BufRead *.asd set filetype=lisp
au BufNewFile,BufRead *.ros set filetype=lisp
au BufnewFile,BufRead *.ten set filetype=html
au BufEnter *.lisp :syntax sync fromstart " a bit extreme, but guarantees we won't lose syntax highlighting...
" may need to adjust redrawtime to a bigger value if we find a big slow file
if has('gui_running')
  let g:slimv_balloon=1
  au BufnewFile,BufRead *.lisp set balloonexpr=SlimvDescribe(v:beval_text)
endif

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
command! P4Ed execute "!p4 edit " . expand('%p')


"call ToggleRaibowParenthesis
"call rainbow_parenthsis#LoadRound()
"call rainbow_parenthsis#LoadSquare()
"call rainbow_parenthsis#Activate()
let vimclojure#HighlightBuiltins = 1
let vimclojure#ParenRainbow = 1
let vimclojure#FuzzyIndent = 1

if !has('nvim')
  set maxmem=104857600
  set maxmemtot=104857600
  cs add $CSCOPE_DB
  nnoremap <F2> :set invpaste paste?<CR>
  imap <F2> <C-O><F2>
  set pastetoggle=<F2>

else
  " neovim specific options
  colorscheme vim " f you neovim 0.10
  set notermguicolors
  set guicursor=n-v-c-i:block
  " orig: guicursor=n-v-c-sm:block,i-ci-ve:ver25,r-cr-o:hor20
  set title
  set mouse=r " allow highlighting with mouse and middle-click pasting
endif

" diff color scheme:
hi DiffAdd      ctermfg=NONE          ctermbg=Green
hi DiffChange   ctermfg=NONE          ctermbg=NONE
hi DiffDelete   ctermfg=LightBlue     ctermbg=Red
hi DiffText     ctermfg=Yellow        ctermbg=Red

set maxmempattern=300000

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
  au BufWritePost .vimrc,_vimrc,vimrc,.gvimrc,_gvimrc,gvimrc so $MYVIMRC " | if has('gui_running') | so $MYGVIMRC | endif
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
"let g:syntastic_python_checkers = ['pylint'] " seemed to have changed default to flake8, boo
" though both are very slow...

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

" autocmd BufEnter * :syntax sync fromstart
" uncomment above if experiencing syntax highlight problems,
" should force buffer to redo syntax...

set dir=~/.vim/swpfiles
" uncomment above to make vim swapfiles files stop showing up in project folders
map \what :echo expand('%:p')<cr>

" allow clicking into what windows we want and click-dragging borders for sizes
set mouse=a

let g:buftabline_indicators = 1

" for lisp, see also .vim/after/syntax/lisp.vim to fix up
" some stuff from default vim syntax.
" Useful commands to figure out syntax issues:
" :echo synIDattr(synID(line('.'), col('.'), 1), 'name')
" figure out what rule is being applied.
" :syntax list
" :syntax list <specificThing>
" shows all loaded syntax rules or just the rules for one thing.
" :put =execute('syntax list')
" puts the results of command in buffer for copy/pasting..
"
" Anyway,
" slimv:
"let g:slimv_indent_disable=1
"let g:slimv_completions='fuzzy'
" helps with fuzzy defs, e.g. mvb<tab> to multiple-value-bind. if commented out, will use swank completions, which is more accurate in context and handles package-local nicknames but less helpful when you only know part of the identifier symbol
" self-suggestion: locally set this to swank when dealing with package local
" nicknames, otherwise keep it fuzzy.
map \fuzz :let g:slimv_completions='fuzzy'<cr>
map \nofuzz :let g:slimv_completions='swank'<cr>

let g:paredit_mode=0
let g:lisp_rainbow=1
let g:slimv_repl_split=2
let g:slimv_repl_split_size=15
let g:scheme_builtin_swank=1
let g:slimv_timeout=10
let g:slimv_fasl_directory = '/tmp/'
let g:slimv_swank_cmd='!xterm -iconic -e sbcl --dynamic-space-size 10GB --core ~/sbcl-core --load /home/kevin/.vim/bundle/slimv/slime/start-swank.lisp &'
"let g:swank_log=1
" also consider in vim :set verbosefile=test.log && :set verbose=20

let g:vlime_window_settings = {
  \ "repl": { -> {"size": 15}},
  \ "arglist": { -> {"pos": "belowright"}},
  \ "preview": { -> {"pos": "belowright"}}
  \ }

" ctags -R -a --language-force=lisp ~/quicklisp/dists/quicklisp/software
"let g:slimv_unmap_tab=1
"let g:slimv_ctags='ctags -R -a --language-force=lisp *'

" when using cross ref, jump to file name printed in repl with gF (gf to not
" hit line number). ctrl+o to jump back to repl buf.
map \gF gF<esc>:call WindowSwap#EasyWindowSwap()<cr><C-w>w:call WindowSwap#EasyWindowSwap()<cr><C-w>w<C-o><C-o>G<C-w>w
" ctrl+t to jump back after a ] jump., ctrl+o/ctrl+i to toggle back/forth...
" note that ctrl+] for def (works for make-instance args)
map \sblint :e! scratch<cr>:1,$d<cr>:silent r!sblint<cr>

"let g:vlime_compiler_policy = {"DEBUG": 3}

let g:large_file = 1024 * 1024 * 1024 " 1 GB is 'large'
" don't make swap file for large files
autocmd BufReadPre * let f=expand("<afile>") | if getfsize(f)
            \ > g:large_file | set noswapfile | endif

map \justify gwip
set nojoinspaces " don't double-space after periods...
"map \table :TableModeToggle
"already \tm
" use || for boundary row
let g:table_mode_corner='+'
let g:table_mode_corner_corner='+'
let g:table_mode_header_fillchar='='

"let g:ctrlp_custom_ignore = {
"  \ 'dir':  '\v[\/](\.(git|hg|svn)|quicklisp)$',
"  \ 'file': '\v\.(exe|so|dll|o)$',
"  \ 'link': 'some_bad_symbolic_links',
"  \ }

function! SlimvXrefSysDependsOn()
    call SlimvXrefBase( 'System Depends On: ', ':sys-depends-on' )
endfunction

function! SlimvXrefEditUses()
    call SlimvXrefBase( 'Who Edits/Uses: ', ':edit-uses' )
endfunction

map ,xd :call SlimvXrefSysDependsOn()<cr>

map ,xx :call SlimvXrefEditUses()<cr>

map ,xg :call SlimvXrefBase('Who Specializes Generally: ', ':specializes-generally')<cr>

" Inspect, but using clouseau
function! SlimvClouseauInspect()
    if !SlimvConnectSwank()
        return
    endif
    let s:inspect_path = []
    let s = input( 'Clouseau Inspect: ', SlimvSelectSymbolExt() )
    if s != ''
      let s:inspect_path = [s]
      "call SlimvCommandUsePackage( s:py_cmd . 'swank_inspect("' . s . '")' )
      call SlimvFindPackage()
      call SlimvEval(['(cl-user::clouseau-inspect ' . s . ')'])
    endif
endfunction

map ,ci :call SlimvClouseauInspect()<cr>
map ,fc  :call SlimvFindPackage()<cr>:call SlimvEval(['(' . SlimvSelectSymbolExt() . ')'])<cr>

let g:grepper = {}
let g:grepper.tools = ['ag']
let g:grepper.dir = 'repo,cwd'
command! Todo :Grepper -noprompt -query '(todo|fixme)'

" ALE plugin options
let g:ale_warn_about_trailing_whitespace=0
let g:ale_pattern_options = {'.c$\|.h$\|.cpp$\|.cxx$': {'ale_enabled': 0}}


function! StripTrailingWhitespace()
  "random note, should use normal! instead, which avoids
  "custom mappings
  normal mZ
  %s/\s\+$//e
  if line("'Z") != line(".")
    echo "Stripped whitespace\n"
  endif
  normal `Z
endfunction
autocmd BufWritePre * :call StripTrailingWhitespace()

" project-specific vimrc
" e.g. create a .vimrc in the project root and bind custom g:whatever settings
" in it for just that project.
set exrc " local rc files
set secure " but not allowing them to use autocmd, shell, and write commands

"set autochdir

set rtp+=~/git_repos/not_mine/fzf
" remap ctrl-p, fzf is better...
"let g:ctrlp_map='<c-`>'
map <c-p> :FZF<cr>

let g:rooter_patterns = ['.git', '_darcs/', '.hg/', '.bzr/', '.svn/', '.fslckout']
let g:rooter_change_directory_for_non_project_files = 'current'
" when rooter or whatever fails...
map \fixdir :cd %:h

"set cmdheight=3 " avoids having to press enter so much e.g. when cwd changes because of rooter
let g:rooter_silent_chdir = 1

" right-align from 80th col
nnoremap \<tab> mc80A <esc>080lDgelD`cP


":verbose set bg?
"echo g:colors_name

" Align line-wise comment delimiters flush left instead of following code indentation
let g:NERDDefaultAlign = 'left'
" Add spaces after comment delimiters by default
let g:NERDSpaceDelims = 1
" Allow commenting and inverting empty lines (useful when commenting a region)
let g:NERDCommentEmptyLines = 1

set clipboard=unnamedplus "automatically make yank/delete use system clipboard


function! CreateQuicklispSymlink()
  " Get the full path of the current file
  let l:filepath = expand('%:p')
  if l:filepath == ''
    echoerr "No file loaded in the buffer."
    return
  endif

  " Get the directory of the file
  let l:filedir = fnamemodify(l:filepath, ':h')
  let l:dirname = fnamemodify(l:filedir, ':t')

  " Define the target symlink path
  let l:link_path = expand('~/quicklisp/local-projects/' . l:dirname)

  " If the symlink already exists, notify the user
  if filereadable(l:link_path) || isdirectory(l:link_path)
    echoerr "Link or directory already exists at: " . l:link_path
    return
  endif

  " Construct and run the shell command to create the symlink
  let l:cmd = 'ln -s ' . shellescape(l:filedir) . ' ' . shellescape(l:link_path)
  call system(l:cmd)

  " Report success or failure
  if v:shell_error
    echoerr "Failed to create symlink."
  else
    echom "Symlink created: " . l:link_path . " -> " . l:filedir
  endif
endfunction

" Optional: Bind it to a command
command! QuicklispLink call CreateQuicklispSymlink()


source /home/kevin/.vimrc-lisp-refactoring

source /home/kevin/.vimrc-claude

execute pathogen#infect()
Helptags

" Random vim notes to self to save a google
"
" <leader> is typically \...
"
" spellcheck:
" zg - add word to dict
" z= - bring up list of suggestions
" zug - undo add to word
" zw - mark as wrong
" zuw - undo wrong
" @@ or @: - repeat last colon command
"
" can echo value of a setting with :set thing?
" can get it into clipboard with
" :redir @+ | set thing? | redir END
"
" Run :syntax sync fromstart
" if mysteriously lose syntax highlighting
"
" Navigation notes
"
" Use :b# to toggle between two buffers
" When using cross ref, jump to file name printed in repl with gF (gf to not use
" line number)
" ctrl+o will jump back to previous buf.
" ctrl+t to jump back after a ] jump., ctrl+o/ctrl+i to toggle back/forth...
" note that ctrl+] for def (works for make-instance args)
"
" Lisp development notes
"
" the doc for commands is all in slimv.txt but these are most useful to me. (adding some
" vlime equivalents as well, though I sorta gave up on vlime.)
" Majority of my commands are ,e and ,d and ,v-then-up-arrow-history to
" reload a system.
" Commands I use very rarely (and sometimes forget exist) are thus listed first,
" then some commands that I usually remember and use frequently but not as much
" as top 3 listed next (in roughly inverse frequency order), lastly the top 3.
"
"
" ,B set breakpoint
" ,l disassemble
" ,b eval buffer
" ,u undefine function
" ,fc my custom find package
" ,xd who sys depends on
" ,xx who edits/uses symbol
" ,xg who specializes generally
" \rf2s refator symbol to string
"
" ,- clear repl
" cross referencing
"    ,xc          Who Calls
"    ,xr          Who References
"    ,xs          Who Sets
"    ,xb          Who Binds
"    ,xm          Who Macroexpands
"    ,xp          Who Specializes
"    ,xl          List Callers
"    ,xe          List Callees
" ,L compile and load current file
" ,F compiles file
" ctrl+x + 0 => close form
"
" debug window
"      (swank-backend:activate-stepping frame) (swank-backend:sldb-step-into) (swank-backend:sldb-step-next) (swank-backend:sldb-step-out)
" ,a abort, ,q quit, ,n continue, ,N restart frame,
"      ,si step-into, ,sn step-next, ,so step-out
"
" ,i inspect frame
" ,y interrupt repl
" ,1 macroexpand-1 ::: \m1
" ,m macroexpand all ::: \mm
" ,s describes symbol ::: \do, \da
" ,h looks up symbol in hyperspec
" ,r evals 'region' or selection :::  \s after selecting
" ,fe funcall no-arg symbol name under cursor
"
" ,d evaluates form ::: \st
" ,e evals current exp :::  \ss or \se, \ss handles atoms
" ,v eval-in-debug-frame. e.g. (swank-backend:restart-frame idx)
"
" Refactoring notes
"
" general mass-rename:
" :Grepper -noprompt -query symbol
" (can use cword, or just :Grepper<cr> and type it in
" populates both quickfix list and location list.
" :cdo %s/symbol/replacement/gce
" -> run search-replace over all quickfix spots. the e suppresses errors.
"  (also can use (Subvert)
" :cdo update
" -> saves buffers
"  (can just add this to the end of the other cdo command as | update)
" :cdfo :bd
" -> close the buffers
"
" also generally useful, :cclose to close the quickfix window e.g. after
" jumping to a file
" and <c-w><c-p> to jump back to quickfix window if not wanting to close
"
