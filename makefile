# I made some changes here so that you use gfortran rather than f77, as I was using on my machine
# make clean will remove old o and e files
# make gnssSNR or make gnssSNR.e will make the executable 
FFLAGS =
LDARGS =
OBJFILES = gnssSNR.o read_sp3_200sats.o read_header_25obs.o pick_9points.o dpolint.o moving_sites.o get_azel_sp3.o librariesSNRv3.o read_block_gnss.o write_gnss_to_file.o unixlib.o

.PHONY: gnssSNR
gnssSNR: gnssSNR.e

%.o: %.f
	gfortran -c $<

gnssSNR.e: $(OBJFILES)
	gfortran $(OBJFILES) -o gnssSNR.e

.PHONY: clean
clean:
	rm -f *.o *.e
