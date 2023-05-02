.PHONY : all clean

all : Interpreter clean

Interpreter : Interpreter.hs Run.hs TypeCheck.hs AbsGramatyka.hs LexGramatyka.hs ParGramatyka.hs PrintGramatyka.hs TestGramatyka.hs
	ghc Interpreter.hs -o interpreter

clean :
	-rm -f *.hi *.o *.log *.aux *.dvi
