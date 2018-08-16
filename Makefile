# This wraps both the cabal build and also the build in dpi.

all: build

###############################################################################
# SECTION 1: Variables to override:
###############################################################################

# PREFIX doesn't really matter if we're not doing a proper install in
# to the filesystem.
#
# To do a build into a fakeroot using $(DESTDIR), set prefix to
# something more sensible (like /usr) and the install target will do
# what you want.
PREFIX := /usr/local

# BUILD is the directory (relative to the source directory) in which
# we want to place built files.
BUILD := build

# DESTDIR works in the normal way, defaulting to '/'
DESTDIR := /

###############################################################################
# SECTION 2: Configuring with Cabal
###############################################################################

# We always have to run "cabal configure" explicitly, otherwise we
# won't be able to override PREFIX properly.

CABAL_BUILD_DIR       := --builddir=$(BUILD)
# We always pretend we're doing a global install (we don't want to
# deal with stuff ending up in ~/.cabal).
CABAL_CONFIGURE_FLAGS := $(CABAL_BUILD_DIR) --global --prefix=$(PREFIX)

.PHONY: cabal-configure
cabal-configure: $(BUILD)/cabal-config-flags

$(BUILD)/cabal-config-flags: acov.cabal
	./cabal-wrapper.sh configure $(CABAL_CONFIGURE_FLAGS)

###############################################################################
# SECTION 3: Building with Cabal
###############################################################################

# Dependencies for "cabal build". Firstly, we depend on acov.cabal. We
# also depend on src/X.hs whenever $(BUILD)/build/acov/acov-tmp/X.hi
# exists. This seems to ensure all the rebuilds we need.
hi-files-for = $(filter-out %/BuildVersion.hi,\
                   $(wildcard $(BUILD)/build/$(1)/$(1)-tmp/*.hi))
hi-relpaths-for = \
  $(patsubst $(BUILD)/build/$(1)/$(1)-tmp/%,%,$(call hi-files-for,$(1)))
hs-for = $(patsubst %.hi,src/$(1)/%.hs,$(call hi-relpaths-for,$(1)))

hi-files-top = $(filter-out %,BuildVersion.hi,$(wildcard $(BUILD)/build/*.hi))
hi-relpaths-top = $(patsubst $(BUILD)/build/%,%,$(hi-files-top))
hs-top = $(patsubst %.hi,src/frontend/%.hs,$(hi-relpaths-top))

HS_TLS        := acov acov-combine acov-report
HS_FILES      := $(foreach t,$(HS_TLS),$(call hs-for,$(t))) $(hs-top)
HS_BINARIES   := $(foreach b,$(HS_TLS),$(BUILD)/build/$(b)/$(b))
HS_BUILD_DEPS := acov.cabal Setup.hs $(HS_FILES) $(BUILD)/cabal-config-flags

# To actually build the Haskell code, we run cabal build and then
# touch a stamp file.
.PHONY: cabal-build
cabal-build: $(BUILD)/build/.stamp

$(BUILD)/build/.stamp: $(HS_BUILD_DEPS)
	./cabal-wrapper.sh build $(CABAL_BUILD_DIR)
	touch $@

###############################################################################
# SECTION 4: Installing with Cabal
###############################################################################

CABAL_INSTALL_FLAGS := $(CABAL_BUILD_DIR) --destdir=$(DESTDIR)

INSTALLED_BINARY := $(DESTDIR)$(PREFIX)/bin/acov

.PHONY: cabal-copy
cabal-copy: $(INSTALLED_BINARY)

$(INSTALLED_BINARY): $(BUILD)/build/.stamp
	./cabal-wrapper.sh copy $(CABAL_INSTALL_FLAGS)

###############################################################################
# SECTION 5: Building the C++ DPI library
###############################################################################

# At the moment, there's no proper dependency tracking because we only
# have one source file.
DPI_SRC := dpi/cover.cc
DPI_BASENAMES := $(DPI_SRC:dpi/%=%)

DPI64_OBJECTS := $(BUILD)/dpi-64/$(DPI_BASENAMES:%.cc=%.o)
DPI32_OBJECTS := $(BUILD)/dpi-32/$(DPI_BASENAMES:%.cc=%.o)

LIBDPI64 := $(BUILD)/dpi-64/libacovdpi.so
LIBDPI32 := $(BUILD)/dpi-32/libacovdpi.so

COMPILE_FLAGS = \
  -c $(ACOV_CXXFLAGS) $(CXXFLAGS) $(ACOV_CPPFLAGS) $(CPPFLAGS) -fPIC
LINK_FLAGS = \
  $(ACOV_CXXFLAGS) $(CXXFLAGS) $(ACOV_LDFLAGS) $(LDFLAGS) -shared

COMPILE.32    = $(CXX) -c -o $@ $(COMPILE_FLAGS) -m32 $<
COMPILE.64    = $(CXX) -c -o $@ $(COMPILE_FLAGS) -m64 $<
LINK.32       = $(CXX) -o $@ $(LINK_FLAGS) -m32 $^ $(ACOV_LIBS) $(LIBS)
LINK.64       = $(CXX) -o $@ $(LINK_FLAGS) -m64 $^ $(ACOV_LIBS) $(LIBS)

ACOV_CPPFLAGS := -Idata
ACOV_CXXFLAGS := -Os

$(BUILD)/dpi-32:
	mkdir -p $@

$(BUILD)/dpi-64:
	mkdir -p $@

$(DPI32_OBJECTS): $(BUILD)/dpi-32/%.o: dpi/%.cc | $(BUILD)/dpi-32
	$(COMPILE.32)

$(DPI64_OBJECTS): $(BUILD)/dpi-64/%.o: dpi/%.cc | $(BUILD)/dpi-64
	$(COMPILE.64)

$(LIBDPI32): $(DPI32_OBJECTS)
	$(LINK.32)

$(LIBDPI64): $(DPI64_OBJECTS)
	$(LINK.64)

.PHONY: build-dpi-64 build-dpi-32 build-dpi
build-dpi-64: $(LIBDPI64)
build-dpi-32: $(LIBDPI32)
build-dpi: build-dpi-64 build-dpi-32

###############################################################################
# SECTION 6: Installing the C++ DPI library
###############################################################################

# The 64-bit library needs to go into $(DESTDIR)$(PREFIX)/lib; the
# 32-bit library needs to go in $(DESTDIR)$(PREFIX)/usr

.PHONY: install-dpi
install-dpi: $(LIBDPI32) $(LIBDPI64)
	install -d $(DESTDIR)$(PREFIX)/lib
	install -t $(DESTDIR)$(PREFIX)/lib $(LIBDPI64)
	install -d $(DESTDIR)$(PREFIX)/lib32
	install -t $(DESTDIR)$(PREFIX)/lib32 $(LIBDPI32)

###############################################################################
# SECTION 7: Phony targets to build and install everything
###############################################################################

.PHONY: build
build: $(LIBDPI64) $(LIBDPI32) cabal-build

.PHONY: install
install: install-dpi cabal-copy

.PHONY: clean
clean:
	rm -rf $(BUILD)
