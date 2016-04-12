# Defining variables
binaries       = image_consumer
objects        = dectris_image_read.o    external_image_provider.o   image_consumer.o
shared_objects = libDectrisImageRead.so  libExternalImageProvider.so

gfortran               = gfortran
gfortran_compile       = $(gfortran) -c
gfortran_compile_args  =
gfortran_link          = $(gfortran) -o
gfortran_link_args     = -ldl


ifort                  = /opt/intel/bin/ifort
ifort_compile          = $(ifort) 
ifort_compile_args     = -free -Tf 
ifort_link             = $(ifort) 
ifort_link_args        = 

f_compile      = $(ifort) -c
f_compile_args = $(ifort_compile_args)

f_link         = $(ifort) -o
f_link_arg     = $(ifort_link_args)
# f_compile      = $(gfortran) -c
# f_link         = $(gfortran) -o
# f_compile_args = $(gfortran_compile_args)
# f_link_arg     = $(gfortran_link_args)

image_consumer: libDectrisImageRead.so libExternalImageProvider.so image_consumer.o
	$(f_link) image_consumer image_consumer.o libExternalImageProvider.so $(f_link_arg)

image_consumer.o: image_consumer.f03 
	$(f_compile) $(f_compile_args) image_consumer.f03 

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
