# CIS3190 (Software for Legacy Systems) Assignment 1

Legacy Fortran: Forest Fire Weather Index

## Author Information

* Name: Rabia Qureshi
* Email: rqureshi@uoguelph.ca
* Student ID: 1046427

## About the Repo

* `Assignment_1.pdf` and `fortranIV_FFWI.pdf`: assignment doc and info about the original Fortran program
* `original/`: contains the original Fortran code that was to be re-engineered, along with the test input file provided by Professor to ensure output did not change after code re-engineering changes
* `FFWIndices.f95` and `ffwi.f95`: my re-engineered Fortran95 code
* `CIS3190 Reflection Report - Rabia Qureshi.pdf`: my reflection report 
* `input.txt`: test input file (this can be replaced with any other valid input file)

## Compiling and Running

In the root directory, run the following two commands:
* To compile: `gfortran -Wall FFWIndices.f95 ffwi.f95`
* To run: `./a.out`
