LIB=tools

OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLOPTFLAGS=-inline 32#-unsafe -ccopt "-O3 -march=i686"

OCAMLFLAGS=-g
STD_LIBS=unix.cma str.cma
STD_LIBSA=$(patsubst %.cma,%.a,$(STD_LIBS))
STD_LIBSX=$(patsubst %.cma,%.cmx,$(STD_LIBS))

ML_SOURCES=x.ml delim.ml mstring.ml file.ml latex.ml table.ml \
mlist.ml mbool.ml mset.ml mmap.ml xset.ml xlist.ml xmap.ml \
abstract_string.ml

MLI_SOURCES=$(patsubst %.ml,%.mli,$(ML_SOURCES))
CMOFILES=$(patsubst %.ml,%.cmo,$(ML_SOURCES))
CMIFILES=$(patsubst %.ml,%.cmi,$(ML_SOURCES))
CMXFILES=$(patsubst %.ml,%.cmx,$(ML_SOURCES))

all:	$(CMOFILES) $(CMIFILES)
	$(OCAMLC) -a $(OCAMLFLAGS) $(STD_LIBS) $(ML_SOURCES) -o $(LIB).cma

opt:	$(CMXFILES) $(CMIFILES)
	$(OCAMLOPT) -cclib -lunix.cmxa -cclib -lstr.cmxa -a $(OCAMLOPTFLAGS) -o $(LIB).cmxa unix.a str.a $(CMXFILES)

.SUFFIXES: .ml .mli .cmo .cmi .cmx .mly .mll

%.cmo: %.ml %.cmi
	$(OCAMLC) $(OCAMLFLAGS) $(STD_LIBS) -c $<

%.cmi: %.mli
	$(OCAMLC) $(OCAMLFLAGS) -c $<

%.cmx: %.ml %.cmi
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<


.PHONY: clean doc

clean:
	rm -f *.cm*
	rm -f *.o
	rm -f *.obj
	rm -f *.d
	rm -f *.a
	rm -f *~

doc: 	all
	ocamldoc -html -d doc $(ML_SOURCES) $(MLI_SOURCES)
	ocamldoc -dot -o doc/tools.dot $(ML_SOURCES) $(MLI_SOURCES)
	ocamldoc -texi -o doc/tools.texinfo $(ML_SOURCES) $(MLI_SOURCES)

#newtop: Tools.cma
#	ocamlmktop -o tools Tools.cma

