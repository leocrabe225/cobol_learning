SRC = cobcrud.cbl
PRECOMPILE = cobcrud.cob
BIN = cobcrud

all: $(BIN)
				./$(BIN)

$(BIN): $(PRECOMPILE)
				cobc -x -locesql $(PRECOMPILE) -o $(BIN)

$(PRECOMPILE): $(SRC)
				ocesql $(SRC) $(PRECOMPILE)

clean:
				rm $(BIN)