# Defining Dummy Makefile for multiple compilation (ifort/gfotran)
#
#
binaries       = image_consumer

CC       = gcc
CFLAGS   = -g -c -fPIC -Wall -std=gnu99 
LDFLAGS  = -shared -fPIC 
LDOPTS   = -lc
FORTRAN  = ifort

ifeq ($(FORTRAN),ifort)
	FC              = /opt/intel/bin/ifort
	FFLAGS          = -c -free -fPIC -heap-arrays -O3 -g -traceback -Tf 
	FLDFLAGS        = -g 
	FLDFLAGS_SHARED = -g -shared
	FLDOPTS         = -ldl
else
	FC              = gfortran
	FFLAGS          = -c -fPIC -fcray-pointer -O3 -g 
	FLDFLAGS        = -g 
	FLDFLAGS_SHARED = -g -shared
	FLDOPTS         = -ldl
endif
all: image_consumer

image_consumer:  generic_data_plugin.o image_consumer.o
	$(FC) $(FLDFLAGS) -o $@ $^  $(FLDOPTS)

image_consumer.o: image_consumer.f90
	$(FC) $(FFLAGS) $^ -o $@

generic_data_plugin.o: generic_data_plugin.f90
	$(FC) $(FFLAGS) $^ -o $@


# Cleaning everything
clean:
	rm -rf $(binaries)
	rm -rf *.o
#	rm -rf *.so
	rm -rf *.mod
# End of the makefile

