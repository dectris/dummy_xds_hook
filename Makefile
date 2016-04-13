# Defining Dummy Makefile for multiple compilation (ifort/gfotran)
# https://software.intel.com/en-us/forums/intel-fortran-compiler-for-linux-and-mac-os-x/topic/268114
#
#
binaries       = image_consumer
#objects        = dectris_image_read.o    external_image_provider.o   image_consumer.o
#shared_objects = libDectrisImageRead.so  libExternalImageProvider.so

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

image_consumer:  image_consumer.o libDectrisImageRead.so
	$(FC) $(FLDFLAGS) -o $@ image_consumer.o  $(FLDOPTS)

image_consumer.o: image_consumer.f03 
	$(FC) $(FFLAGS) $^ -o $@

#libDectrisImageRead.so: dectris_image_read.o
#	$(FC) $(FLDFLAGS_SHARED) -o $@ $^ $(FLDOPTS)

#dectris_image_read.o: dectris_image_read.f03
#	$(FC) $(FFLAGS) $^ -o $@

libDectrisImageRead.so: dectris_image_read.o
	$(CC) $(LDFLAGS) -o $@ $^ $(LDOPTS)

dectris_image_read.o: dectris_image_read.c
	$(CC) $(CFLAGS) $^ -o $@

# Cleaning everything
clean:
	rm -rf $(binaries)
	rm -rf *.o
	rm -rf *.so
	rm -rf *.mod
# End of the makefile




#dlfcn.o: dlfcn.f90 
#	$(FC) $(FFLAGS) $^ -o $@

#dlfcn.mod: dlfcn.o 
#	$(FC) $(FLFLAGS) $^ $(FLDOPTS)
