gcc -Wno-unused-result -pipe -O3 -finline-limit=5000 -fkeep-inline-functions -finline-functions -ffast-math -fomit-frame-pointer -DNDEBUG  -UDEBUG -D_REENTRANT=1 -c phkmalloc.c
