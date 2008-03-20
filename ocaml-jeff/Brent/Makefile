LIB=brent
EXEC1=segment
EXEC2=check

TOOLS=tools
TOOLS_DIR=../Tools/
TOOLS_LIB=$(TOOLS).cma
TOOLS_ML_SOURCES=x.ml delim.ml mstring.ml file.ml latex.ml table.ml \
mlist.ml mbool.ml mset.ml mmap.ml xset.ml xlist.ml xmap.ml abstract_string.ml
TOOLS_SOURCES=$(patsubst %,$(TOOLS_DIR)%,$(TOOLS_ML_SOURCES))
TOOLS_CMX=$(patsubst %.ml,%.cmx,$(TOOLS_SOURCES))

OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLOPTFLAGS=-inline 32#-unsafe -ccopt "-O3 -march=i686"
INCLUDE = -I $(TOOLS_DIR)
OCAMLFLAGS = -g $(INCLUDE)

ML_SOURCES = s.ml brent.ml 
MLI_SOURCES=$(patsubst %.ml,%.mli,$(ML_SOURCES)) $(EXEC1).mli $(EXEC2).mli
CMOFILES=$(patsubst %.ml,%.cmo,$(ML_SOURCES))
CMIFILES1=$(patsubst %.ml,%.cmi,$(ML_SOURCES)) $(EXEC1).cmi 
CMIFILES2=$(patsubst %.ml,%.cmi,$(ML_SOURCES)) $(EXEC2).cmi 
CMXFILES1=$(patsubst %.ml,%.cmx,$(ML_SOURCES)) $(EXEC1).cmx 
CMXFILES2=$(patsubst %.ml,%.cmx,$(ML_SOURCES)) $(EXEC2).cmx 

.PHONY:	all opt clean doc

all:	seg check

seg:	$(TOOLS_LIB) $(CMOFILES) $(CMIFILES1)					
	$(OCAMLC) -a $(OCAMLFLAGS) $(TOOLS_LIB) $(CMOFILES) -o $(LIB).cma	
	$(OCAMLC) $(OCAMLFLAGS) $(TOOLS_LIB) $(CMOFILES) $(EXEC1).ml -o $(EXEC1)

check:	$(TOOLS_LIB) $(CMOFILES) $(CMIFILES2)					
	$(OCAMLC) $(OCAMLFLAGS) $(TOOLS_LIB) $(CMOFILES) $(EXEC2).ml -o $(EXEC2)


$(TOOLS_LIB): $(TOOLS_SOURCES)
	cd $(TOOLS_DIR) && $(MAKE)

opt:	segopt checkopt

segopt:	$(TOOLS_LIBCMX) $(CMXFILES1) $(CMIFILES1) 
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -o $(EXEC1) unix.cmxa str.cmxa $(TOOLS_CMX) $(CMXFILES1)

checkopt:	$(TOOLS_LIBCMX) $(CMXFILES2) $(CMIFILES2) 
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -o $(EXEC2) unix.cmxa str.cmxa $(TOOLS_CMX) $(CMXFILES2)

.SUFFIXES: .ml .mli .cmo .cmi .cmx

%.cmo: %.ml %.cmi
	$(OCAMLC) $(OCAMLFLAGS) $(TOOLS_LIB) -c $<

%.cmi: %.mli
	$(OCAMLC) $(OCAMLFLAGS) $(TOOLS_LIB) -c $<

%.cmx: %.ml %.cmi
	$(OCAMLOPT) $(INCLUDE) $(OCAMLOPTFLAGS) -c $<

clean:
	rm -f *.cm*
	rm -f *.o
	rm -f *.obj
	rm -f *.d
	rm -f *.a
	rm -f $(EXEC1)
	rm -f $(EXEC2)

doc: all
	ocamldoc  $(INCLUDE) -html -d doc $(ML_SOURCES) $(MLI_SOURCES)
	ocamldoc  $(INCLUDE) -dot -o doc/brent.dot $(ML_SOURCES) $(MLI_SOURCES) 
	ocamldoc  $(INCLUDE) -texi -o doc/brent.texinfo  $(ML_SOURCES) $(MLI_SOURCES)

