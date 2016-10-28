CAMLTKDIR=../lib
SUPPORTDIR=../libsupport
CAMLC=camlc
CAMLCOMP=$(CAMLC) -c
CPP=/lib/cpp -P -Dunix
COMPFLAGS=-g -O fast -I $(CAMLTKDIR)
LINKFLAGS=-g -custom
CAMLYACC=camlyacc -v
CAMLLEX=camllex
CAMLDEP=../../../src/tools/camldep

TKLINKOPT=-ccopt -L$(TCLLIBDIR) -ccopt -L$(TKLIBDIR) -ccopt -L$(X11_LIB) \
	  -ccopt -L$(SUPPORTDIR) -lcaml -lcamltk $(TKLIBS)


# Programs in the single-process architecture
PGM1=  addition.one \
	bindtest.one \
	buttons.one \
	calc.one \
	canvastest.one \
	entryparams.one \
	lbox.one \
	menutest.one \
	text1.one \
	text2.one \
	tkmesbox.one \
        helloworld.one

all: $(PGM1)

clean : 
	rm -f *.z? $(PGM2) $(PGM1)


.SUFFIXES :
.SUFFIXES : .mli .ml .zi .zo .mlp .two .one

.mli.zi:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.zo:
	$(CAMLCOMP) $(COMPFLAGS) $<

.zo.one:
	$(CAMLC) $(LINKFLAGS) -I $(CAMLTKDIR) -o $@ \
		tklib.zo $< $(TKLINKOPT)

addition.one: addition.zo
bindtest.one: bindtest.zo
buttons.one: buttons.zo
calc.one: calc.zo
canvastest.one: canvastest.zo
entryparams.one: entryparams.zo
lbox.one: lbox.zo
menutest.one: menutest.zo
text1.one: text1.zo
text2.one: text2.zo
tkmesbox.one: tkmesbox.zo
helloworld.one: helloworld.zo
