FC := gfortran
IFLAGS := -I .
FFLAGS := -g3 -O3 -Dno_nans
FFLAGS_CHECK := -fbacktrace -fbounds-check -fcheck=all -ffpe-trap=zero,invalid,overflow,underflow,denormal
FFLAGS := $(FFLAGS) $(FFLAGS_CHECK)
FFLAGS_WARNINGS := -Waliasing -Wcharacter-truncation -Wimplicit-interface -Wintrinsics-std -Wline-truncation -Wintrinsic-shadow
FFLAGS := $(FFLAGS) $(FFLAGS_WARNINGS)
FFLAGS_WARNINGS_EXT := -Wconversion -Wsurprising -Wunused
FFLAGS := $(FFLAGS) $(FFLAGS_WARNINGS_EXT)
FFLAGS_WARNINGS_EXT_EXT := -Warray-bounds -Wextra -Wall -pedantic
FFLAGS := $(FFLAGS) $(FFLAGS_WARNINGS_EXT_EXT)
FFLAGS_EXT := -std=gnu -fmax-errors=5
FFLAGS := $(FFLAGS) $(FFLAGS_EXT)
DEPS = harmonic.o theory.o


all: theory

clean:
	@rm -fv *.o *.mod theory

theory: $(DEPS)
	$(FC) $(FFLAGS) $^ -o $@

%.o: %.f90
	$(FC) $(IFLAGS) $(FFLAGS) -c -o $@ $<
