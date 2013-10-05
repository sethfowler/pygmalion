CFLAGS = -O0 -I.

%.o : %.c
	$(CC) $(CFLAGS) $^ -c -o $@

program: a.o b.o c.o
	$(CC) $(CFLAGS) a.o b.o c.o -o program

clean:
	rm -f *.o program
