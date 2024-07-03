# gcc -DDEBUG -gdwarf-4 -O0 -g3 ./lib/checksum.c btest.c

# unoptimized
# gcc -DDEBUG -O0 -g3 ./lib/checksum.c read-demo.c -o read-demo
# gcc -DDEBUG -O0 -g3 ./lib/checksum.c write-demo.c -o write-demo

# optimized
gcc -O3 ./lib/checksum.c read-demo.c -o read-demo
gcc -O3 ./lib/checksum.c write-demo.c -o write-demo
