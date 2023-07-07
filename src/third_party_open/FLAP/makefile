#!/usr/bin/make

#main building variables
DSRC    = src/
DOBJ    = Test_Driver/obj/
DMOD    = Test_Driver/mod/
DEXE    = Test_Driver/
LIBS    =
ifeq ("${FC}","")
	FC      = gfortran
endif
ifeq ("${FC}","f77")
  # under OSX FC is set by default to the nonexistent f77
  FC      = gfortran
endif
# This should not only detect gfortran/f77, but also a GNU-based mpif90:
DOGNU = $(shell ${FC} --version | grep -i GNU > /dev/null; expr 1 - $$?)

OPTSC   =  -cpp -c -O2
ifeq ("$(DOGNU)", "1")
OPTSC+=-J Test_Driver/mod/ -frealloc-lhs -ffree-line-length-none
OPTSL+=-J Test_Driver/mod/
else
OPTSC+=-module Test_Driver/mod/ -assume realloc_lhs
OPTSL+=-module Test_Driver/mod/
endif

VPATH   = $(DSRC) $(DOBJ) $(DMOD)
MKDIRS  = $(DOBJ) $(DMOD) $(DEXE)
LCEXES  = $(shell echo $(EXES) | tr '[:upper:]' '[:lower:]')
EXESPO  = $(addsuffix .o,$(LCEXES))
EXESOBJ = $(addprefix $(DOBJ),$(EXESPO))

#auxiliary variables
COTEXT  = "Compiling $(<F)"
LITEXT  = "Assembling $@"

#building rules
$(DEXE)TEST_DRIVER: $(MKDIRS) $(DOBJ)test_driver.o
	@rm -f $(filter-out $(DOBJ)test_driver.o,$(EXESOBJ))
	@echo $(LITEXT)
	@$(FC) $(OPTSL) $(DOBJ)*.o $(LIBS) -o $@
EXES := $(EXES) $(DEXE)TEST_DRIVER

all: $(EXES) $(LIBS)

install: all
# empty rule as long as this makefile does not build a library (only .mods' and .o's, NOINST)

#Emulate automake's `make dist` behavior for inclusion in other packages that build source dists.
# make  top_distdir=../../pkgname-1.0 distdir=../../pkgname-1.0/subdir/FLAP \
#     am__remove_distdir=: am__skip_length_check=: am__skip_mode_fix=: distdir
distdir:
	cp -rpf . $(distdir)


#compiling rules
$(DOBJ)data_type_command_line_interface.o: src/Data_Type_Command_Line_Interface.F90 \
	$(DOBJ)ir_precision.o
	@echo $(COTEXT)
	@$(FC) $(OPTSC)  $< -o $@

$(DOBJ)ir_precision.o: src/IR_Precision.F90
	@echo $(COTEXT)
	@$(FC) $(OPTSC)  $< -o $@

$(DOBJ)test_driver.o: src/Test_Driver.f90 \
	$(DOBJ)ir_precision.o \
	$(DOBJ)data_type_command_line_interface.o
	@echo $(COTEXT)
	@$(FC) $(OPTSC)  $< -o $@

#phony auxiliary rules
.PHONY : $(MKDIRS)
$(MKDIRS):
	@mkdir -p $@
.PHONY : cleanobj
cleanobj:
	@echo deleting objects
	@rm -fr $(DOBJ)
.PHONY : cleanmod
cleanmod:
	@echo deleting mods
	@rm -fr $(DMOD)
.PHONY : cleanexe
cleanexe:
	@echo deleting exes
	@rm -f $(addprefix $(DEXE),$(EXES))
.PHONY : clean
clean: cleanobj cleanmod
.PHONY : cleanall
cleanall: clean cleanexe
