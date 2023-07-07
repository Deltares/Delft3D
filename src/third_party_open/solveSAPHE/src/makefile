
.SUFFIXES:

.SUFFIXES: .o .f90 .F90

RM = /bin/rm -f

FC = gfortran
FFLAGS =
OPTIONDEFS =



MODULE_OBJS_ACB  = mod_precision.o mod_random.o mod_chemconst.o mod_acb_phsolvers.o
MODULE_OBJS_ACBW = mod_precision.o mod_random.o mod_chemconst.o mod_acbw_phsolvers.o

MODULE_OBJS_ATGEN = mod_precision.o mod_random.o mod_chemconst.o mod_phsolvers.o
MODULE_OBJS_ATGENLOG = mod_precision.o mod_random.o mod_chemconst.o mod_phsolvers_logging.o

MODULE_OBJS_CHECK = mod_precision.o mod_chemconst.o

default: $(MODULE_OBJS)

acb: $(MODULE_OBJS_ACB) driver_acb.o
	$(FC) $(OPTIONDEFS) $(FFLAGS) $(MODULE_OBJS_ACB) driver_acb.o -o $@

acbw: $(MODULE_OBJS_ACBW) driver_acbw.o
	$(FC) $(OPTIONDEFS) $(FFLAGS) $(MODULE_OBJS_ACBW) driver_acbw.o -o $@

at_general: $(MODULE_OBJS_ATGEN) driver_at_general.o
	$(FC) $(OPTIONDEFS) $(FFLAGS) $(MODULE_OBJS_ATGEN) driver_at_general.o -o $@

at_logging: $(MODULE_OBJS_ATGENLOG) driver_at_logging.o
	$(FC) $(OPTIONDEFS) $(FFLAGS) $(MODULE_OBJS_ATGENLOG) driver_at_logging.o -o $@

at_random: $(MODULE_OBJS_ATGENLOG) driver_at_random.o
	$(FC) $(OPTIONDEFS) $(FFLAGS) $(MODULE_OBJS_ATGENLOG) driver_at_random.o -o $@

checkconsts: $(MODULE_OBJS_CHECK) main_check.o
	$(FC) $(OPTIONDEFS) $(FFLAGS) $(MODULE_OBJS_CHECK) main_check.o -o $@

.f90.o:;
	$(FC) $(OPTIONDEFS) -c $(FFLAGS) $*.f90 -o $*.o

.F90.o:;
	$(FC) $(OPTIONDEFS) -c $(FFLAGS) $*.F90 -o $*.o

clean:
	$(RM) *.o *.mod

cleanexes:
	$(RM) acb acbw at_general at_logging at_random checkconsts

tidy: clean
	$(RM) *.res checkconsts.log

