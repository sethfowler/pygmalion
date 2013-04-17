" Pygmalion commands.
" <Leader>] - Go to the definition of the identifier under the cursor.
 
command! PygGoToDefinition cexpr system('pygmalion --definition-for ' .expand('%:p') . ' ' . line('.') . ' ' . col('.'))
augroup pygmalion
  au BufEnter *.c,*.cpp,*.C,*.h,*.hpp,*.H  nnoremap <buffer><silent> <Leader>] :PygGoToDefinition<CR>
augroup END
