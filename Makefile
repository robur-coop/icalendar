all: clean
	jbuilder build -p icalendar
test: all
	jbuilder runtest --no-buffer -j 1
clean:
	jbuilder clean
utop:
	dune utop src --profile=release
