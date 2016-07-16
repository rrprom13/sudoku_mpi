all: build

build:
	mpicc sptree.c -o main

clean:
	rm main
