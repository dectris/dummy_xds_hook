//
// dummy external image provider v0.0.1
//
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <dlfcn.h>


void image_provider_ ( int32_t image [])
{
  void *handle;
  void (*my_image_read)(int32_t []);
  char *error;

  handle = dlopen ("libDectrisImageRead.so", RTLD_LAZY);
  if (!handle) {
    fputs (dlerror(), stderr);
    exit(1);
  }

  my_image_read = dlsym(handle, "image_read");
  if ((error = dlerror()) != NULL)  {
    fputs(error, stderr);
    exit(1);
  }

  my_image_read(image);
  dlclose(handle);

  return;
  
}
