" Pygmalion commands.
" <Leader>]  - Go to the definition of the identifier under the cursor.
" <Leader>[  - Go back.
" <Leader>pc - List the callers of the function or method under the cursor.
" <Leader>pC - List the callees of the function or method under the cursor.
" <Leader>pb - List the bases of the identifier under the cursor.
" <Leader>po - List the overrides of the identifier under the cursor.
" <Leader>pr - List locations which reference the identifier under the cursor.

command! PygGoToDefinition cexpr system('pygmalion --definition ' .expand('%:p') . ' ' . line('.') . ' ' . col('.'))
command! PygCallers cexpr system('pygmalion --callers ' .expand('%:p') . ' ' . line('.') . ' ' . col('.'))
command! PygCallees cexpr system('pygmalion --callees ' .expand('%:p') . ' ' . line('.') . ' ' . col('.'))
command! PygBases cexpr system('pygmalion --bases ' .expand('%:p') . ' ' . line('.') . ' ' . col('.'))
command! PygOverrides cexpr system('pygmalion --overrides ' .expand('%:p') . ' ' . line('.') . ' ' . col('.'))
command! PygReferences cexpr system('pygmalion --references ' .expand('%:p') . ' ' . line('.') . ' ' . col('.'))

augroup pygmalion
  au BufEnter *.c,*.cpp,*.C,*.h,*.hpp,*.H  nnoremap <buffer><silent> <Leader>] :PygGoToDefinition<CR>
  au BufEnter *.c,*.cpp,*.C,*.h,*.hpp,*.H  nnoremap <buffer><silent> <Leader>[ <C-O>
  au BufEnter *.c,*.cpp,*.C,*.h,*.hpp,*.H  nnoremap <buffer><silent> <Leader>pc :PygCallers<CR>
  au BufEnter *.c,*.cpp,*.C,*.h,*.hpp,*.H  nnoremap <buffer><silent> <Leader>pC :PygCallees<CR>
  au BufEnter *.c,*.cpp,*.C,*.h,*.hpp,*.H  nnoremap <buffer><silent> <Leader>pb :PygBases<CR>
  au BufEnter *.c,*.cpp,*.C,*.h,*.hpp,*.H  nnoremap <buffer><silent> <Leader>po :PygOverrides<CR>
  au BufEnter *.c,*.cpp,*.C,*.h,*.hpp,*.H  nnoremap <buffer><silent> <Leader>pr :PygReferences<CR>
augroup END
