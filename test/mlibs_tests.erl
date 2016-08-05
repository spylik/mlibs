-module(mlibs_tests).

-include_lib("eunit/include/eunit.hrl").

% --------------------------------- fixtures ----------------------------------

mlibs_build_atom_key_test_() ->
    {setup,
        fun disable_output/0,
        {inparallel,
            [
                {<<"build_atom_key should work well with multiple atoms">>, 
                    fun() -> 
                        ?assertEqual(
                            test1_test2_test3,
                            mlibs:build_atom_key([test1, test2, test3]) 
                        ) 
                    end},
                {<<"build_atom_key should work well with multiple atoms/lists">>, 
                    fun() -> 
                        ?assertEqual(
                            test1_test2_test3,
                            mlibs:build_atom_key([test1, "test2", test3]) 
                        ) 
                    end},
                {<<"build_atom_key should work well with one atom">>, 
                    fun() -> 
                        ?assertEqual(
                            test1,
                            mlibs:build_atom_key([test1]) 
                        ) 
                    end},
                {<<"build_atom_key should work well with one list">>, 
                    fun() -> 
                        ?assertEqual(
                            test1,
                            mlibs:build_atom_key(["test1"]) 
                        ) 
                    end},
                {<<"build_atom_key should work well with multiple lists">>, 
                    fun() -> 
                        ?assertEqual(
                            test1_test2,
                            mlibs:build_atom_key(["test1","test2"]) 
                        ) 
                    end},
                {<<"build_atom_key should work well with binaries">>, 
                    fun() -> 
                        ?assertEqual(
                            test1,
                            mlibs:build_atom_key([<<"test1">>]) 
                        ) 
                    end},
                {<<"build_atom_key should work well with multiple binaries">>, 
                    fun() -> 
                        ?assertEqual(
                            test1_test2,
                            mlibs:build_atom_key([<<"test1">>,<<"test2">>]) 
                        ) 
                    end} 
            ]
        }
    }.

mlibs_build_binary_key_test_() ->
    {setup,
        fun disable_output/0,
        {inparallel,
            [
                {<<"build_binary_key should work well with multiple atoms">>, 
                    fun() -> 
                        ?assertEqual(
                            <<"test1.test2.test3">>,
                            mlibs:build_binary_key([test1, test2, test3]) 
                        ) 
                    end},
                {<<"build_binary_key should work well with multiple atoms/lists">>, 
                    fun() -> 
                        ?assertEqual(
                            <<"test1.test2.test3">>,
                            mlibs:build_binary_key([test1, "test2", test3]) 
                        ) 
                    end},
                {<<"build_binary_key should work well with one atom">>, 
                    fun() -> 
                        ?assertEqual(
                            <<"test1">>,
                            mlibs:build_binary_key([test1]) 
                        ) 
                    end},
                {<<"build_binary_key should work well with one list">>, 
                    fun() -> 
                        ?assertEqual(
                            <<"test1">>,
                            mlibs:build_binary_key(["test1"]) 
                        ) 
                    end},
                {<<"build_binary_key should work well with multiple lists">>, 
                    fun() -> 
                        ?assertEqual(
                            <<"test1.test2">>,
                            mlibs:build_binary_key(["test1","test2"]) 
                        ) 
                    end},
                {<<"build_binary_key should work well with binaries">>, 
                    fun() -> 
                        ?assertEqual(
                            <<"test1">>,
                            mlibs:build_binary_key([<<"test1">>]) 
                        ) 
                    end},
                {<<"build_binary_key should work well with multiple binaries">>, 
                    fun() -> 
                        ?assertEqual(
                            <<"test1.test2">>,
                            mlibs:build_binary_key([<<"test1">>,<<"test2">>]) 
                        ) 
                    end} 

            ]
        }
    }.

disable_output() ->
    error_logger:tty(false).
