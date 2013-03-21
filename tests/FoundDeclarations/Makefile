CFLAGS = -O0 -I.

%.o : %.cpp
	$(CXX) $(CFLAGS) $^ -c -o $@

program: a.o b.o c.o
	$(CXX) $(CFLAGS) a.o b.o c.o -o program

clean:
	rm -f *.o program
