## This file was auto-created by LmcpGen. Modifications will be overwritten.

LIBNAME := liblmcp
STATICLIBNAME := $(LIBNAME).a
LIBDIR := ./

LIBS := $(LIBDIR)/$(STATICLIBNAME)

MKDIR   := mkdir

# Compiler
CXX := g++

SOURCES := avtas/lmcp/ByteBuffer.cpp \
	avtas/lmcp/Factory.cpp \
	avtas/lmcp/NodeUtil.cpp \
	avtas/lmcp/Node.cpp \
	avtas/lmcp/XMLParser.cpp \
        -<makefile_source_list>-

OBJS    := $(SOURCES:.cpp=.o)

INCDIR  := -I.

DEPS	:= avtas/lmcp/Object.h 

COMOPTS := -fPIC -std=c++11 -Wall

DEFINES := -DLINUX

.PHONY=all
all:    $(LIBS)

%.o:    %.cpp $(DEPS)
	@echo "     Compiling $< ($@) ..."
	$(CXX) $(INCDIR) $(DEFINES) $(COMOPTS) -c $< -o $@

.PHONY=$(LIBDIR)/$(STATICLIBNAME)
$(LIBDIR)/$(STATICLIBNAME):     $(OBJS)
	@echo "  Creating static library $(STATICLIBNAME)"
	@ar r $(LIBDIR)/$(STATICLIBNAME) $(OBJS)

.PHONY=clean
clean:
	@echo "   Removing libraries ($(STATICLIBNAME)) ..."
	@rm -f $(LIBS)
	@echo "   Removing object code ..."
	@rm -f $(OBJS)
