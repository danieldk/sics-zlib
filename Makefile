all: zlib.so

zlib.so: zlib.c zlib.pl
	splfr zlib.pl zlib.c \
		-lz -LD -Wl,-rpath,$(PWD) $(LDFLAGS)
