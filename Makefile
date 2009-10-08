
backend.so: backend.o
	gcc --shared backend.o -o backend.so -lGL -lGLU -lSDL_image `sdl-config --libs`

backend.o: backend.c
	gcc -c backend.c -o backend.o `sdl-config --libs`
