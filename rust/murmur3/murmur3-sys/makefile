CFLAGS = -O3 -Wall

.PHONY: all clean tests

shared: murmur3.c murmur3.h
	$(CC) -fPIC -O3 -c murmur3.c -o $(OUT_DIR)/murmur3.o
	$(CC) -shared -Wl,--export-dynamic $(OUT_DIR)/murmur3.o -o $(OUT_DIR)/libmurmur3.so

clean:
	rm -rf *.o *.so
