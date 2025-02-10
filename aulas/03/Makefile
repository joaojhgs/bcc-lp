all:
	g++ -c list_seq.cpp
	g++ -c list_seq_main.cpp
	# g++ -O3 list_seq.o list_seq_main.o -o main
	ld list_seq.o list_seq_main.o -o main
clean:
	rm -f *.o main