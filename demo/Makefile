
PROGS  = pdfviewer
SOURCES = PdfViewer.hs

all : $(PROGS)

pdfviewer : PdfViewer.hs
	$(HC) --make $< -o $@ $(HCFLAGS)

clean:
	rm -f $(SOURCES:.hs=.hi) $(SOURCES:.hs=.o) $(PROGS)

HC=ghc
