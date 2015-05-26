CPPFLAGS=-Wall -Wextra -std=c++11
DEBUGFLAGS=-g
# LDFLAGS=-g $(shell root-config --ldflags)
# LDLIBS=$(shell root-config --libs)

SRCS=LexiBWT.cpp LexiDeBWT.cpp Huffman.cpp MoveToFront.cpp DemoveToFront.cpp SuffixBWT.cpp
OBJS=$(subst .cpp,.o,$(SRCS))

# tool: $(OBJS)
#     g++ -o tool tool.o support.o $(LDLIBS) 

%.o: %.cpp
	g++ $(CPPFLAGS) -c $< 

main: main.cpp $(OBJS)
	g++ $(CPPFLAGS) -o main main.cpp $(OBJS)

testing: testing.cpp $(OBJS)
	g++ $(CPPFLAGS) -o testing testing.cpp $(OBJS)

COMPRESS_LEXI_SRCS=LexiBWT.cpp Huffman.cpp MoveToFront.cpp
COMPRESS_LEXI_OBJS=$(subst .cpp,.o,$(COMPRESS_LEXI_SRCS))
compress-lexi: compress-lexi.cpp $(COMPRESS_LEXI_OBJS)
	g++ $(CPPFLAGS) $(DEBUGFLAGS) -o compress-lexi compress-lexi.cpp $(COMPRESS_LEXI_OBJS)

DECOMPRESS0_SRCS=LexiDeBWT.cpp Huffman.cpp DemoveToFront.cpp
DECOMPRESS0_OBJS=$(subst .cpp,.o,$(DECOMPRESS0_SRCS))
decompress0: decompress0.cpp $(DECOMPRESS0_OBJS)
	g++ $(CPPFLAGS) $(DEBUGFLAGS) -o decompress0 decompress0.cpp $(DECOMPRESS0_OBJS)

test: testing
	rm -f minipantadeusz.bin > /dev/null
	rm -f minipantadeusz2.txt > /dev/null
	time (./testing > /dev/null)
	diff minipantadeusz.txt minipantadeusz2.txt

clean:
	rm -f tests/*.compressed
	rm -f tests/*.decompressed
	rm -f compress-lexi compress-suffix compress-lecture decompress0 decompress1
