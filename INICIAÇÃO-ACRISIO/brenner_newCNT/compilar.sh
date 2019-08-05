#!/bin/csh -f                                                              

gfortran -I. -o main main.f
gfortran -o make_cnt/cnt_xyz.x make_cnt/cnt_xyz.f
gfortran -o make_cnt/cnt_iso.x make_cnt/cnt_iso.f
gfortran -o make_cnt/filling.x make_cnt/filling.f

gfortran -o file2xyz.x file2xyz.f
gfortran -o file2coord.x file2coord.f

