CPPFLAGS=-g -Wall -Wextra -std=c++11
# LDFLAGS=-g $(shell root-config --ldflags)
# LDLIBS=$(shell root-config --libs)

SRCS=LexiBWT.cpp LexiDeBWT.cpp Huffman.cpp MoveToFront.cpp DemoveToFront.cpp
OBJS=$(subst .cpp,.o,$(SRCS))

# tool: $(OBJS)
#     g++ -o tool tool.o support.o $(LDLIBS) 

%.o: %.cpp
	g++ $(CPPFLAGS) -c $< 

main: main.cpp $(OBJS)
	g++ $(CPPFLAGS) -o main main.cpp $(OBJS)

testing: testing.cpp $(OBJS)
	g++ $(CPPFLAGS) -o testing testing.cpp $(OBJS)
