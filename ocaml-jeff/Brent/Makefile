LIB=brent
EXEC=segment

TOOLS=tools
TOOLS_DIR=../Tools/
TOOLS_LIB=$(TOOLS).cma
TOOLS_ML_SOURCES=x.ml delim.ml mstring.ml file.ml latex.ml table.ml \
mlist.ml mbool.ml mset.ml mmap.ml xset.ml xlist.ml xmap.ml
TOOLS_SOURCES=$(patsubst %,$(TOOLS_DIR)%,$(TOOLS_ML_SOURCES))
TOOLS_CMX=$(patsubst %.ml,%.cmx,$(TOOLS_SOURCES))

OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLOPTFLAGS=-inline 32#-unsafe -ccopt "-O3 -march=i686"
INCLUDE = -I $(TOOLS_DIR)
OCAMLFLAGS = -g $(INCLUDE)

ML_SOURCES = s.ml brent.ml
MLI_SOURCES=$(patsubst %.ml,%.mli,$(ML_SOURCES)) $(EXEC).mli
CMOFILES=$(patsubst %.ml,%.cmo,$(ML_SOURCES))
CMIFILES=$(patsubst %.ml,%.cmi,$(ML_SOURCES)) $(EXEC).cmi
CMXFILES=$(patsubst %.ml,%.cmx,$(ML_SOURCES)) $(EXEC).cmx

.PHONY:	all opt clean doc

all:	$(TOOLS_LIB) $(CMOFILES) $(CMIFILES)
	ocamlc -a $(OCAMLFLAGS) $(TOOLS_LIB) $(CMOFILES) -o $(LIB).cma
	$(OCAMLC) $(OCAMLFLAGS) $(TOOLS_LIB) $(CMOFILES) $(EXEC).ml -o $(EXEC)

$(TOOLS_LIB): $(TOOLS_SOURCES)
	cd $(TOOLS_DIR) && $(MAKE)

opt:	$(TOOLS_LIBCMX) $(CMXFILES) $(CMIFILES)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -o $(EXEC) unix.cmxa str.cmxa $(TOOLS_CMX) $(CMXFILES)

.SUFFIXES: .ml .mli .cmo .cmi .cmx

%.cmo: %.ml %.cmi
	$(OCAMLC) $(OCAMLFLAGS) $(TOOLS_LIB) -c $<

%.cmi: %.mli
	$(OCAMLC) $(OCAMLFLAGS) $(TOOLS_LIB) -c $<

%.cmx: %.ml %.cmi
	$(OCAMLOPT) $(INCLUDE) $(TOOLS_LIBA) $(OCAMLOPTFLAGS) -c $<

clean:
	rm -f *.cm*
	rm -f *.o
	rm -f *.obj
	rm -f *.d
	rm -f *.a
	rm -f *~
	rm -f $(EXEC)

doc: all
	ocamldoc  $(INCLUDE) -html -d doc $(ML_SOURCES) $(MLI_SOURCES)
	ocamldoc  $(INCLUDE) -dot -o doc/brent.dot $(ML_SOURCES) $(MLI_SOURCES) 
	ocamldoc  $(INCLUDE) -texi -o doc/brent.texinfo  $(ML_SOURCES) $(MLI_SOURCES)

