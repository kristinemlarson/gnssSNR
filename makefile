FC = f77 -fbounds-check
FFLAGS =
LDARGS =
FILES2 = gnssSNR.o read_sp3_200sats.o pick_9points.o dpolint.o moving_sites.o get_azel_sp3.o librariesSNRv3.o read_block.o write_gnss_to_file.o


gnssSNR: $(FILES2)
	f77 -fbounds-check $(LDARGS) $(FILES2) -o gnssSNR.e
