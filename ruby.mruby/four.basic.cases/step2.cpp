/*
   [step2] run ruby code in file.
   */

#include <mruby.h>
#include <mruby/proc.h>
#include <stdio.h>


int main()
{
  FILE* f = fopen("step2.rb", "r");
  if (f == NULL)
  { 
    printf("ERROR: file not found");
    exit(1);
  }

  auto mrb = mrb_open();
  mrb_load_file(mrb, f);
  fclose(f);
  mrb_close(mrb);
}
