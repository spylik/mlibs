PROJECT = teaser

SHELL_DEPS = sync lager

ifeq ($(USER),travis)
    TEST_DEPS += covertool
    dep_covertool = git https://github.com/idubrov/covertool
endif

SHELL_OPTS = -pa ebin/ test/ -env ERL_LIBS deps -eval 'lager:start()' -run mlibs autotest_on_compile

include erlang.mk
