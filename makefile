


FSHARPC    = fsharpc
SRC_ENGINE = Piece Board GameState Rules
DIR_ENGINE = ./src/engine/

INPUT_ENGINE = $(addsuffix .fs,$(addprefix $(DIR_ENGINE),$(SRC_ENGINE)))

build:
	$(FSHARPC) $(INPUT_ENGINE) test.fs
