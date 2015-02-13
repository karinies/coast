OS=$(shell echo `uname`)
CC=gcc
LIB=/usr/local/lib

all:
ifeq ($(OS),Darwin)
	$(CC) -m64 -c -fPIC -O3 -fomit-frame-pointer -o fastlz.o fastlz.c
	$(CC) -m64 -dynamiclib -o libfastlz.dylib -dylib fastlz.o
	install libfastlz.dylib $(LIB)
else
	$(CC) -fPIC -O3 -fomit-frame-pointer -shared -o libfastlz.so fastlz.c
	install libfastlz.so $(LIB)
endif

clean:
	rm fastlz.o libfastlz.*