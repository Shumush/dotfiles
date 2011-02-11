augroup filetypedetect
" Promela
au BufNewFile,BufRead *.promela,*.prm,*.pr	setf promela
augroup END

augroup filetypedetect
	" lolcode
au BufNewFile,BufRead *.lol setf lolcode
augroup END

augroup filetypedetect
	" objective-C
au BufRead,BufNewFile *.m	setf objc
augroup END

augroup filetypedetect
	" Cobra
au BufNewFile,BufRead *.cobra setf cobra
augroup END

augroup filetypedetect
	" Scala
au BufNewFile,BufRead *.scala setf scala
augroup END

augroup filetypedetect
	" Go
  au BufNewFile,BufRead *.go setf go
augroup END

augroup filetypedetect
    " Cilk++
au BufNewFile,BufRead *.cilk setf cpp
augroup END

augroup filetypedetect
    " HJ
au BufNewFile,BufRead *.hj setf java
augroup END

augroup filetypedetect
    " Google protocol buffers
au BufNewFile,BufRead *.proto setfiletype proto
augroup END

augroup filetypedetect
    " Thrift files
au BufNewFile,BufRead *.thrift setfiletype thrift
augroup END

augroup filetypedetect
    " Cuda header files.
au BufNewFile,BufRead *.cuh setfiletype cuda
augroup END

augroup filetypedetect
  " Actionscript
au BufNewFile,BufRead *.as setfiletype actionscript
augroup END

augroup filetypedetect
  " MXML
au BufNewFile,BufRead *.mxml setfiletype mxml
augroup END
