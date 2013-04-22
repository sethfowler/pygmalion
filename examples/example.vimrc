" Pygmalion commands.
" <Leader>] - Go to the definition of the identifier under the cursor.
" <Leader>pc - List the callers of the function or method under the cursor.
" <Leader>pC - List the callees of the function or method under the cursor.

command! PygGoToDefinition cexpr system('pygmalion --definition ' .expand('%:p') . ' ' . line('.') . ' ' . col('.'))
command! PygCallers cexpr system('pygmalion --callers ' .expand('%:p') . ' ' . line('.') . ' ' . col('.'))
command! PygCallees cexpr system('pygmalion --callees ' .expand('%:p') . ' ' . line('.') . ' ' . col('.'))
augroup pygmalion
  au BufEnter *.c,*.cpp,*.C,*.h,*.hpp,*.H  nnoremap <buffer><silent> <Leader>] :PygGoToDefinition<CR>
  au BufEnter *.c,*.cpp,*.C,*.h,*.hpp,*.H  nnoremap <buffer><silent> <Leader>pc :PygCallers<CR>
  au BufEnter *.c,*.cpp,*.C,*.h,*.hpp,*.H  nnoremap <buffer><silent> <Leader>pC :PygCallees<CR>
augroup END
