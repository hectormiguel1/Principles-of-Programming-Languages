##file used to automate the build and run proccess on linux 
##FILE_PATH variable is an enviroment variable set before the make program is run 
COMPILER = fsharpc 
RUNTIME = mono
FILE = $(FILE_PATH).fs
OUT_EXEC = $(FILE_PATH).exe

build:
	$(COMPILER) $(FILE) 
	
run:
	$(RUNTIME) $(OUT_EXE)
	
clean:
	/bin/rm -f *.exe
	
build-run: clean build run
