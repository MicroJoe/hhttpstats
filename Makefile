EXEC=stats
MAIN=Main.hs

.PHONY:clean mrproper

all:
	ghc $(MAIN) -o $(EXEC)

clean:
	rm -f *.hi *.o

mrproper: clean
	rm -f $(EXEC)
