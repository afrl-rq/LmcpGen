## This file was auto-created by LmcpGen. Modifications will be overwritten.

SRCFILES=main.cpp

CC=g++

CFLAGS=-DLINUX -std=c++11 -Wall

LDFLAGS=-static

SRC=$(SRCFILES)

LIBS=-L../..
LIBS+=-llmcp

INCS=-I../.. 

OBJ=$(SRC:.cpp=.o)

EXE=test_client

RM=rm -f

.PHONY: all 
.PHONY: clean
.PHONY: invoker 

all: $(LIB) $(EXE)

exe: $(EXE)
	
%.o: %.cpp 
	$(CC) $(CFLAGS) $(INCS) -o $@ -c $<

$(EXE): $(OBJ)
	$(CC) $(OBJ) -o $@ $(LDFLAGS) $(LIBS)

clean: 
	-$(RM) $(OBJ) $(EXE)
