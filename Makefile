# Defining variables
binaries       = image_consumer
objects        = dectris_image_read.o    external_image_provider.o   image_consumer.o
shared_objects = libDectrisImageRead.so  libExternalImageProvider.so

f90comp = gfortran
# f90comp = /opt/intel/bin/ifort

# Makefile

image_consumer: image_consumer.o libExternalImageProvider.so
	gfortran -o image_consumer image_consumer.o libExternalImageProvider.so -ldl

image_consumer.o: image_consumer.f03 
	gfortran  -c image_consumer.f03 

libDectrisImageRead.so: dectris_image_read.o
	gcc  -shared -fPIC -Wl,-soname,libDectrisImageRead.so -o libDectrisImageRead.so dectris_image_read.o  -lc

libExternalImageProvider.so: external_image_provider.o
	gcc  -shared -fPIC -Wl,-soname,libExternalImageProvider.so -o libExternalImageProvider.so external_image_provider.o -lc

dectris_image_read.o: dectris_image_read.c
	gcc -g -c -fPIC -Wall -std=gnu99 dectris_image_read.c

external_image_provider.o: external_image_provider.c 
	gcc -g -c -fPIC -Wall -std=gnu99 external_image_provider.c

# Cleaning everything
clean:
	rm -rf $(binaries)
	rm -rf $(objects)
	rm -rf $(shared_objects)
# End of the makefile
