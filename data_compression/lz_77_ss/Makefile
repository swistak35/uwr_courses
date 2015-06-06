CPPFLAGS=-Wall -Wextra -std=c++11
DEBUGFLAGS=-g

%.o: %.cpp %.h
	g++ $(CPPFLAGS) $(DEBUGFLAGS) -c $< 

# suffix: suffix.cpp SuffixBWT.o LexiBWT.o
# 	g++ $(CPPFLAGS) $(DEBUGFLAGS) -o suffix suffix.cpp SuffixBWT.o LexiBWT.o

# uni: uni.cpp UniBWT.o LexiBWT.o
# 	g++ $(CPPFLAGS) $(DEBUGFLAGS) -o uni uni.cpp UniBWT.o LexiBWT.o

COMPRESS_LZ77_SRCS=SimpleSW.cpp Huffman.cpp
COMPRESS_LZ77_OBJS=$(subst .cpp,.o,$(COMPRESS_LZ77_SRCS))
compress-lz77: compress-lz77.cpp $(COMPRESS_LZ77_OBJS)
	g++ $(CPPFLAGS) $(DEBUGFLAGS) -o compress-lz77 compress-lz77.cpp $(COMPRESS_LZ77_OBJS)

# DECOMPRESS0_SRCS=DeBWT.cpp Huffman.cpp DemoveToFront.cpp
# DECOMPRESS0_OBJS=$(subst .cpp,.o,$(DECOMPRESS0_SRCS))
# decompress0: decompress0.cpp $(DECOMPRESS0_OBJS)
# 	g++ $(CPPFLAGS) $(DEBUGFLAGS) -o decompress0 decompress0.cpp $(DECOMPRESS0_OBJS)

clean:
	rm -f *.o
	rm -f tests/*.compressed
	rm -f tests/*.decompressed
	rm -f compress-lz77 compress-lzss decompress-lz77 decompress-lzss
