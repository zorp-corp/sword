gcc -O0 -DDEBUG -g3 -c ./lib/checksum.c ./btree.c

gcc -Wno-unused-result -pipe -O0 -g3 -finline-limit=5000 -fkeep-inline-functions -finline-functions -ffast-math -fomit-frame-pointer -DNDEBUG  -UDEBUG -D_REENTRANT=1 -c phkmalloc.c

gcc -O0 -DDEBUG -g3 ./btree.o ./checksum.o ./phkmalloc.o


# gcc -Wno-unused-result -pipe -O3 -finline-limit=5000 -fkeep-inline-functions -finline-functions -ffast-math -fomit-frame-pointer -DNDEBUG  -UDEBUG -D_REENTRANT=1 phkmalloc.c
