all:
	jbuilder build -p icalendar
test: clean
	jbuilder runtest --no-buffer -j 1
clean:
	jbuilder clean
utop:
	dune utop src --profile=release
