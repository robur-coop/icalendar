all: clean
	dune build -p icalendar
test: all
	dune runtest --no-buffer -j 1
clean:
	dune clean
utop:
	dune utop src --profile=release
