PROJECT = teaser

# Change this line for expire cache of deps and plt
MAKEFILE_VERSION = 0.0.1

# --------------------------------------------------------------------
# Defining OTP version for this project which uses by kerl
# --------------------------------------------------------------------

ifneq ($(shell basename $(shell dirname $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST)))))), deps)
ERLANG_OTP = OTP-$(shell cat ./.env | grep ERLANG_VERSION | sed -e s/^ERLANG_VERSION=//)
endif

# --------------------------------------------------------------------
# Dependencies.
# --------------------------------------------------------------------

# if we part of deps directory, we using $(CURDIR)../ as DEPS_DIR
ifeq ($(shell basename $(shell dirname $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST)))))), deps)
    DEPS_DIR ?= $(shell dirname $(CURDIR))
endif

SHELL_DEPS = sync

ifdef CI
    TEST_DEPS += covertool
    dep_covertool = git https://github.com/idubrov/covertool
endif

# --------------------------------------------------------------------
# Development enviroment ("make shell" to run it).
# --------------------------------------------------------------------

SHELL_OPTS = -kernel shell_history enabled -pa ebin/ test/ -env ERL_LIBS deps -eval 'mlibs:discover()' -run mlibs autotest_on_compile

# --------------------------------------------------------------------
# We using erlang.mk
# --------------------------------------------------------------------

include erlang.mk
