CFLAGS = -O0 -I.

program: a.o b.o c.o d.o e.o
	$(CC) $(CFLAGS) a.o b.o c.o -o program

# -c, -o, and the source file itself should be removed
a.o : a.c
	$(CC) $(CFLAGS) $^ -c -o $@

# -MD and -MF should be removed
b.o : b.c
	$(CC) $(CFLAGS) $^ -MD -MF $@.d -c -o $@

# -M, -MG, -MP, and -fpch-deps should be removed
c.o : c.c
	$(CC) $(CFLAGS) $^ -M -MG -MP -fpch-deps -MF $@.d
	$(CC) $(CFLAGS) $^ -c -o $@

# -MT, -MQ, and -MM should be removed
d.o : d.c
	$(CC) $(CFLAGS) $^ -MT h -MQ h2 -MM -MF $@.d
	$(CC) $(CFLAGS) $^ -c -o $@

# -MMD should be removed
e.o : e.c
	$(CC) $(CFLAGS) $^ -MMD -MF $@.d -c -o $@

clean:
	rm -f *.o *.d program
