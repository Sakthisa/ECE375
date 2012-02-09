TEXFILE="lab4-prelab"
lab4-prelab-view:
	pdflatex $(TEXFILE).tex
	rm $(TEXFILE).log
	rm $(TEXFILE).aux
	evince $(TEXFILE).pdf

TEXFILE="ece375-lab4"
lab4:
	pdflatex $(TEXFILE).tex
	rm $(TEXFILE).log
	rm $(TEXFILE).aux
	evince $(TEXFILE).pdf

build:
	../../avra-1.3.0/src/avra $(FILE).asm
	rm $(FILE).obj
	rm $(FILE).cof
	rm $(FILE).eep.hex


clean:
	rm $(FILE1).hex
	rm $(FILE1).hex
	rm *.aux
	rm *.log

