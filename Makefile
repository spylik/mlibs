PROJECT = teaser

SHELL_DEPS = lager sync

SHELL_OPTS = -pa ebin/ test/ -env ERL_LIBS deps -run mlibs autotest_on_compile

include erlang.mk
