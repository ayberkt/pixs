hist:
	stack build\
	&& stack exec pixs histogram data/in.png\
	&& open -a Google\ Chrome example.svg
