CFLAGS = -O0 -I. -x c++

%.o : %.cpp
	$(CXX) $(CFLAGS) $^ -c -o $@

program: a.o b.o c.o
	$(CXX) a.o b.o c.o -o program

clean:
	rm -f *.o program
