
backend.so: texture.o image.o animation.o sprite.o
	gcc --shared texture.o image.o animation.o sprite.o -o ../backend.so -lGL -lGLU -lSDL_image `sdl-config --libs`

texture.o: texture.c backend.h
	gcc -c texture.c -o texture.o `sdl-config --cflags`

image.o: image.c backend.h
	gcc -c image.c -o image.o `sdl-config --cflags`

animation.o: animation.c backend.h
	gcc -c animation.c -o animation.o `sdl-config --cflags`

sprite.o: sprite.c backend.h
	gcc -c sprite.c -o sprite.o `sdl-config --cflags`
