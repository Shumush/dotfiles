STL_VER=4.6.1
CTAGS=/usr/bin/ctags

default: all

all: qt4 boost stl

qt4:
	${CTAGS} -R --sort=foldcase --c++-kinds=+p --fields=+iaS --extra=+q --language-force=C++ -f qt4 /usr/include/Qt

boost:
	${CTAGS} -R --sort=foldcase --exclude="*typeof*" --c++-kinds=+p --fields=+iaS --extra=+q -f boost /usr/include/boost

stl:
	${CTAGS} -R --c++-kinds=+p --fields=+iaS --extra=+q -f stdlibcpp /usr/include/c++/${STL_VER}

clean:
	@rm qt4 boost stdlibcpp &> /dev/null

