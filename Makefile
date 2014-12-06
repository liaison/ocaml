# 
# Rules to compile the ocaml source code into byte code or binary, 
#  and to clean up some intermediate object files. 
#
# Author:  Lisong Guo <lisong.guo@lip6.fr>
# Date:    8 April, 2014
#

# 'native_code' or 'byte_code', the 'byte_code" mode would allow one to use debugging tools.
#OUTPUT := native_code
OUTPUT := byte_code

ifeq ($(OUTPUT), byte_code)
    # Compiler for bytecode, add debugging information by default
    CC=ocamlc -g 
    SYSLIBS=str.cma unix.cma
    OBJECTS=$(SOURCES:.ml=.cmo)
else
    # Compiler for optimized native code 
    CC=ocamlopt
    SYSLIBS=str.cmxa unix.cmxa
    OBJECTS=$(SOURCES:.ml=.cmx)
endif

# -g: add debugging information 
CFLAGS=-c
LDFLAGS=

OCAMLDEP=ocamldep
DEPENDENCIES=$(shell touch .depend)

TARGET=a.out


# Put the entrance program at the end of the list for the sake of compiling
MAIN_SOURCE_FILE=main.ml
SOURCES=$(shell ls *.ml | grep -v $(MAIN_SOURCE_FILE)) $(MAIN_SOURCE_FILE)
OBJECTS_PAT=*.cmi *.cmo *.cmx *.o *.mli
TEST_SOURCES=$(shell ls *.ml | grep -v $(MAIN_SOURCE_FILE))


all: $(SOURCES) $(TARGET)


$(TARGET): $(OBJECTS)
	$(CC) -o $(TARGET) $(SYSLIBS) $^

# Control the order of compiling through defining the dependencies
main.ml: 

%.cmx: 
	$(CC) $(CFLAGS) $*.ml

%.cmo: 
	$(CC) $(CFLAGS) $*.ml

test: $(TEST_SOURCES)
	$(CC) -o test $(SYSLIBS) $(TEST_SOURCES)

cleanobj:
	rm -f $(OBJECTS_PAT)

# Note: always keeping the .depend file 
clean:
	rm -f $(TARGET) $(OBJECTS_PAT)


