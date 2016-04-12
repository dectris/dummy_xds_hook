# Defining Dummy Makefile for multiple compilation (ifort/gfotran)
#
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
	FC       = /opt/intel/bin/ifort
	FFLAGS   = -c -free -Tf 
	FL       = /opt/intel/bin/ifort
	FLDFLAGS = -o
	FLDOPTS  = -ldl
else
	FC       = gfortran
	FFLAGS   = -c
	FL       = gfortran
	FLDFLAGS = -o
	FLDOPTS  = -ldl
endif
all: image_consumer

image_consumer: libDectrisImageRead.so libExternalImageProvider.so image_consumer.o
	$(FL) $(FLDFLAGS) $@ image_consumer.o libExternalImageProvider.so $(FLDOPTS)

image_consumer.o: image_consumer.f03 
	$(FC) $(FFLAGS) $^ -o $@

libDectrisImageRead.so: dectris_image_read.o
	$(CC) $(LDFLAGS) -Wl,-soname,$@ -o $@ $^ $(LDOPTS)

libExternalImageProvider.so: external_image_provider.o
	$(CC) $(LDFLAGS) -Wl,-soname,$@ -o $@ $^ $(LDOPTS)

dectris_image_read.o: dectris_image_read.c
	$(CC) $(CFLAGS) $^ -o $@

external_image_provider.o: external_image_provider.c 
	$(CC) $(CFLAGS) $^ -o $@

# Cleaning everything
clean:
	rm -rf $(binaries)
	rm -rf *.o
	rm -rf *.so
# End of the makefile
