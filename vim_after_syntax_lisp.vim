" This file should be ~/.vim/after/syntax/lisp.vim
"
" Main Purpose...
" fix up vim's built-in lisp highlighting of symbols to allow earmuffed
" question marks, e.g. *is-foo?*

" Thank you Claude...
" https://claude.ai/share/f1f593be-c9b8-4220-9d91-32db15a6ce30

" can run
" :echo synIDattr(synID(line('.'),col('.'),1),'name')
" on any symbol to see what it is.

" also highlight when symbol is used with the explicit package
syntax match lispEscapeSpecial "[^[:space:]()\"]*[^[:space:]()\"']*:\?[*+][^[:space:]()\"'][^[:space:]()\"']*[*+]"
"syntax match lispEscapeSpecial "\w\?[a-z_0-9-.]*:\?\*\w[a-z_0-9-.?]*\*"
"highlight lispEscapeSpecial ctermbg=red guibg=red

" now fix up indentation (actually just syntax highlighting...) with custom define/with/make macros.
" this might not be popular because these are custom, not part of the lang,
" but I like them to be highlighted.
syntax match lispDecl "\([a-z_0-9-.]*:\?def-[a-z0-9-.?]*\)"
syntax match lispDecl "\([a-z_0-9-.]*:\?define-\?[a-z0-9-.?]*\)"
syntax match lispDecl "\([a-z_0-9-.]*:\?with-[a-z0-9-.?]*\)"
syntax match lispDecl "\([a-z_0-9-.]*:\?make-[a-z0-9-.?]*\)"

" Note: indentation and syntax highlighting are somewhat separate.
" Something might be in lispFunc but be expected to indent like a lispDecl --
" e.g. various with- macros.
" The default list is seen by running :set lispwords?
" (or :pu=execute('set lispwords?') to paste it in your buffer)
" and it's missing some.
setlocal lispwords+=with-hash-table-iterator,with-compilation-unit,catch,block,define-modify-macro,define-compiler-macro,define-method-combination,define-setf-expander,def-suite

" Fix up comment highlighting
" Original:
"match /;.*$/  contains=@lispCommentGroup
"start=/#+nil/ end=/\ze)/  contains=@lispCommentGroup
"match /^\s*#+nil.*$/  contains=@lispCommentGroup

" Clear the broken default rule for #+nil
syntax clear lispComment

" Keep the semicolon comment match
syntax match lispComment  /;.*$/

" Match a list following #+nil, non-greedily
syntax region lispNilFormComment
      \ matchgroup=lispNilFormComment
      \ start=/#+nil\s*(/
      \ end=/)/
      \ contains=NONE
      \ keepend
      \ containedin=ALL

" Match a single non-list form (symbol, number, etc) after #+nil
syntax match lispNilFormComment /#+nil\s\+\S\+/ containedin=ALL

" Highlight both as comment
highlight default link lispNilFormComment Comment

" can still mess up with nested forms but better than before..
