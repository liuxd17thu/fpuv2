init:
	git submodule update --init

compile:
	mill -i -j 0 __.compile

clean:
	git clean -fd

idea:
	mill -i mill.scalalib.GenIdea/idea