-module(something_tests).

-include_lib("eunit/include/eunit.hrl").

% --------------------------------- fixtures ----------------------------------

something_integer_to_integer_test_() ->
    {inparallel,
        [
            {<<"Input -0 output -0">>,
                fun() ->
                    ?assertEqual(-0,
                        something:to_integer(-0)
                    )
                end},
            {<<"Input 0 output 0">>,
                fun() ->
                    ?assertEqual(0,
                        something:to_integer(0)
                    )
                end},
            {<<"Input -2 output -2">>,
                fun() ->
                    ?assertEqual(-2,
                        something:to_integer(-2)
                    )
                end},
            {<<"Input 2 output 2">>,
                fun() ->
                    ?assertEqual(2,
                        something:to_integer(2)
                    )
              end}
      ]
  }.

something_integer_to_positive_integer_test_() ->
    {inparallel,
        [
            {<<"Input -0 output -0">>,
                fun() ->
                    ?assertEqual(0,
                        something:to_positive_integer(-0)
                    )
                end},
            {<<"Input 0 output 0">>,
                fun() ->
                    ?assertEqual(0,
                        something:to_positive_integer(0)
                    )
                end},
            {<<"Input -2 output error">>,
                fun() ->
                    ?assertThrow({error, {cant_convert_to_positive_integer, _Number}},
                        something:to_positive_integer(-2)
                    )
                end},
            {<<"Input 2 output 2">>,
                fun() ->
                    ?assertEqual(2,
                        something:to_positive_integer(2)
                    )
              end}
      ]
  }.


something_floats_to_integer_test_() ->
    {inparallel,
        [
            {<<"Input -2.0 output -2">>,
                fun() ->
                    ?assertEqual(-2,
                        something:to_integer(-2.0)
                    )
                end},
            {<<"Input -2.01 output: throw">>,
                fun() ->
                    ?assertThrow({error, {cant_convert_to_integer, _Number}},
                        something:to_integer(-2.01)
                    )
                end},
            {<<"Input -0.0 output -0">>,
                fun() ->
                    ?assertEqual(-0,
                        something:to_integer(-0.0)
                    )
                end},
            {<<"Input -0.01 output: throw">>,
                fun() ->
                    ?assertThrow({error, {cant_convert_to_integer, _Number}},
                        something:to_integer(-0.01)
                    )
                end},
            {<<"Input 2.0 output 2">>,
                fun() ->
                    ?assertEqual(2,
                        something:to_integer(2.0)
                    )
                end},
            {<<"Input 2.01 output: throw">>,
                fun() ->
                    ?assertThrow({error, {cant_convert_to_integer, _Number}},
                        something:to_integer(2.01)
                    )
                end},
            {<<"Input 2.00000000 output 2">>,
                fun() ->
                    ?assertEqual(2,
                        something:to_integer(2.00000000)
                    )
                end},
            {<<"Input 2.00000000000 output: 2">>,
                fun() ->
                    ?assertEqual(2,
                        something:to_integer(2.00000000000)
                    )
                end},
            {<<"Input 2.000000000001 output: 2">>,
                fun() ->
                    ?assertEqual(2,
                        something:to_integer(2.000000000001)
                    )
                end}
        ]
    }.


something_floats_to_positive_integer_test_() ->
    {inparallel,
        [
            {<<"Input -2.0 output error">>,
                fun() ->
                    ?assertThrow({error, {cant_convert_to_positive_integer, _Number}},
                        something:to_positive_integer(-2.0)
                    )
                end},
            {<<"Input -2.01 output: throw">>,
                fun() ->
                    ?assertThrow({error, {cant_convert_to_integer, _Number}},
                        something:to_positive_integer(-2.01)
                    )
                end},
            {<<"Input -0.0 output 0">>,
                fun() ->
                    ?assertEqual(0,
                        something:to_positive_integer(-0.0)
                    )
                end},
            {<<"Input -0.01 output: throw">>,
                fun() ->
                    ?assertThrow({error, {cant_convert_to_integer, _Number}},
                        something:to_positive_integer(-0.01)
                    )
                end},
            {<<"Input 2.0 output 2">>,
                fun() ->
                    ?assertEqual(2,
                        something:to_positive_integer(2.0)
                    )
                end},
            {<<"Input 2.01 output: throw">>,
                fun() ->
                    ?assertThrow({error, {cant_convert_to_integer, _Number}},
                        something:to_positive_integer(2.01)
                    )
                end},
            {<<"Input 2.00000000 output 2">>,
                fun() ->
                    ?assertEqual(2,
                        something:to_positive_integer(2.00000000)
                    )
                end},
            {<<"Input 2.00000000000 output: 2">>,
                fun() ->
                    ?assertEqual(2,
                        something:to_positive_integer(2.00000000000)
                    )
                end},
            {<<"Input 2.000000000001 output: 2">>,
                fun() ->
                    ?assertEqual(2,
                        something:to_positive_integer(2.000000000001)
                    )
                end}
        ]
    }.


something_lists_to_integer_test_() ->
    {inparallel,
        [
          {<<"Input \"-0\" output -0">>,
                fun() ->
                    ?assertEqual(-0,
                        something:to_integer("-0")
                    )
                end},
            {<<"Input \"0\" output 0">>,
                fun() ->
                    ?assertEqual(2,
                        something:to_integer("2")
                    )
                end},
            {<<"Input \"-2\" output -2">>,
                fun() ->
                    ?assertEqual(-2,
                        something:to_integer("-2")
                    )
                end},
            {<<"Input \"2\" output 2">>,
                fun() ->
                    ?assertEqual(2,
                        something:to_integer("2")
                    )
              end},
            {<<"Input \"-2.0\" output -2">>,
                fun() ->
                    ?assertEqual(-2,
                        something:to_integer("-2.0")
                    )
                end},
            {<<"Input \"-2.01\" output: throw">>,
                fun() ->
                    ?assertThrow({error, {cant_convert_to_integer, _Number}},
                        something:to_integer("-2.01")
                    )
                end},
            {<<"Input \"-0.0\" output -0">>,
                fun() ->
                    ?assertEqual(-0,
                        something:to_integer("-0.0")
                    )
                end},
            {<<"Input \"-0.01\" output: throw">>,
                fun() ->
                    ?assertThrow({error, {cant_convert_to_integer, _Number}},
                        something:to_integer("-0.01")
                    )
                end},
            {<<"Input \"2.0\" output 2">>,
                fun() ->
                    ?assertEqual(2,
                        something:to_integer("2.0")
                    )
                end},
            {<<"Input \"2.01\" output: throw">>,
                fun() ->
                    ?assertThrow({error, {cant_convert_to_integer, _Number}},
                        something:to_integer("2.01")
                    )
                end},
            {<<"Input \"2.00\" output 2">>,
                fun() ->
                    ?assertEqual(2,
                        something:to_integer("2.00")
                    )
                end},
            {<<"Input \"2.000\" output 2">>,
                fun() ->
                    ?assertEqual(2,
                        something:to_integer("2.000")
                    )
                end},
            {<<"Input \"2.0000\" output 2">>,
                fun() ->
                    ?assertEqual(2,
                        something:to_integer("2.0000")
                    )
                end},
            {<<"Input \"2.00000\" output 2">>,
                fun() ->
                    ?assertEqual(2,
                        something:to_integer("2.00000")
                    )
                end},
            {<<"Input \"2.000000\" output 2">>,
                fun() ->
                    ?assertEqual(2,
                        something:to_integer("2.000000")
                    )
                end},
            {<<"Input \"2.0000000\" output 2">>,
                fun() ->
                    ?assertEqual(2,
                        something:to_integer("2.0000000")
                    )
                end},
            {<<"Input \"2.00000000\" output 2">>,
                fun() ->
                    ?assertEqual(2,
                        something:to_integer("2.00000000")
                    )
                end},
            {<<"Input \"2.00000000000\" output: 2">>,
                fun() ->
                    ?assertEqual(2,
                        something:to_integer("2.00000000000")
                    )
                end},
            {<<"Input \"2.00000000001\" output: 2">>,
                fun() ->
                    ?assertEqual(2,
                        something:to_integer("2.00000000001")
                    )
                end},
            {<<"Input \"2.0001\" output: error">>,
                fun() ->
                    ?assertThrow({error, {cant_convert_to_integer, _Number}},
                        something:to_integer("2.0001")
                    )
                end}
        ]
    }.

something_lists_to_positive_integer_test_() ->
    {inparallel,
        [
          {<<"Input \"-0\" output 0">>,
                fun() ->
                    ?assertEqual(-0,
                        something:to_positive_integer("-0")
                    )
                end},
            {<<"Input \"0\" output 0">>,
                fun() ->
                    ?assertEqual(2,
                        something:to_positive_integer("2")
                    )
                end},
            {<<"Input \"-2\" output -2">>,
                fun() ->
                    ?assertThrow({error, {cant_convert_to_positive_integer, _Number}},
                        something:to_positive_integer("-2")
                    )
                end},
            {<<"Input \"2\" output 2">>,
                fun() ->
                    ?assertEqual(2,
                        something:to_positive_integer("2")
                    )
              end},
            {<<"Input \"-2.0\" output -2">>,
                fun() ->
                    ?assertThrow({error, {cant_convert_to_positive_integer, _Number}},
                        something:to_positive_integer("-2.0")
                    )
                end},
            {<<"Input \"-2.01\" output: throw">>,
                fun() ->
                    ?assertThrow({error, {cant_convert_to_integer, _Number}},
                        something:to_positive_integer("-2.01")
                    )
                end},
            {<<"Input \"-0.0\" output 0">>,
                fun() ->
                    ?assertEqual(0,
                        something:to_positive_integer("-0.0")
                    )
                end},
            {<<"Input \"-0.01\" output: throw">>,
                fun() ->
                    ?assertThrow({error, {cant_convert_to_integer, _Number}},
                        something:to_positive_integer("-0.01")
                    )
                end},
            {<<"Input \"2.0\" output 2">>,
                fun() ->
                    ?assertEqual(2,
                        something:to_positive_integer("2.0")
                    )
                end},
            {<<"Input \"2.01\" output: throw">>,
                fun() ->
                    ?assertThrow({error, {cant_convert_to_integer, _Number}},
                        something:to_positive_integer("2.01")
                    )
                end},
            {<<"Input \"2.00\" output 2">>,
                fun() ->
                    ?assertEqual(2,
                        something:to_positive_integer("2.00")
                    )
                end},
            {<<"Input \"2.000\" output 2">>,
                fun() ->
                    ?assertEqual(2,
                        something:to_positive_integer("2.000")
                    )
                end},
            {<<"Input \"2.0000\" output 2">>,
                fun() ->
                    ?assertEqual(2,
                        something:to_positive_integer("2.0000")
                    )
                end},
            {<<"Input \"2.00000\" output 2">>,
                fun() ->
                    ?assertEqual(2,
                        something:to_positive_integer("2.00000")
                    )
                end},
            {<<"Input \"2.000000\" output 2">>,
                fun() ->
                    ?assertEqual(2,
                        something:to_positive_integer("2.000000")
                    )
                end},
            {<<"Input \"2.0000000\" output 2">>,
                fun() ->
                    ?assertEqual(2,
                        something:to_positive_integer("2.0000000")
                    )
                end},
            {<<"Input \"2.00000000\" output 2">>,
                fun() ->
                    ?assertEqual(2,
                        something:to_positive_integer("2.00000000")
                    )
                end},
            {<<"Input \"2.00000000000\" output: 2">>,
                fun() ->
                    ?assertEqual(2,
                        something:to_positive_integer("2.00000000000")
                    )
                end},
            {<<"Input \"2.00000000001\" output: 2">>,
                fun() ->
                    ?assertEqual(2,
                        something:to_positive_integer("2.00000000001")
                    )
                end},
            {<<"Input \"2.0001\" output: error">>,
                fun() ->
                    ?assertThrow({error, {cant_convert_to_integer, _Number}},
                        something:to_positive_integer("2.0001")
                    )
                end}
        ]
    }.

something_binaries_to_integer_test_() ->
    {inparallel,
        [
          {<<"Input <<\"-0\">> output -0">>,
                fun() ->
                    ?assertEqual(-0,
                        something:to_integer(<<"-0">>)
                    )
                end},
           {<<"Input <<\"0\">> output 0">>,
               fun() ->
                   ?assertEqual(2,
                       something:to_integer(<<"2">>)
                   )
               end},
           {<<"Input <<\"-2\">> output -2">>,
               fun() ->
                   ?assertEqual(-2,
                       something:to_integer(<<"-2">>)
                   )
               end},
           {<<"Input <<\"2\">> output 2">>,
               fun() ->
                   ?assertEqual(2,
                       something:to_integer(<<"2">>)
                   )
              end},
           {<<"Input <<\"-2.0\">> output -2">>,
               fun() ->
                   ?assertEqual(-2,
                       something:to_integer(<<"-2.0">>)
                   )
               end},
           {<<"Input <<\"-2.01\">> output: throw">>,
               fun() ->
                ?assertThrow({error, {cant_convert_to_integer, _Number}},
                       something:to_integer(<<"-2.01">>)
                   )
               end},
           {<<"Input <<\"-0.0\">> output -0">>,
               fun() ->
                   ?assertEqual(-0,
                       something:to_integer(<<"-0.0">>)
                   )
               end},
           {<<"Input <<\"-0.01\">> output: throw">>,
               fun() ->
                ?assertThrow({error, {cant_convert_to_integer, _Number}},
                       something:to_integer(<<"-0.01">>)
                   )
               end},
           {<<"Input <<\"2.0\">> output 2">>,
               fun() ->
                   ?assertEqual(2,
                       something:to_integer(<<"2.0">>)
                   )
               end},
           {<<"Input <<\"2.01\">> output: throw">>,
               fun() ->
                ?assertThrow({error, {cant_convert_to_integer, _Number}},
                       something:to_integer(<<"2.01">>)
                   )
               end},
           {<<"Input <<\"2.00000000\">> output 2">>,
               fun() ->
                   ?assertEqual(2,
                       something:to_integer(<<"2.00000000">>)
                   )
               end},
            {<<"Input 2.00000000000 output: 2">>,
                fun() ->
                    ?assertEqual(2,
                        something:to_integer(<<"2.00000000000">>)
                    )
                end}
        ]
    }.


something_binaries_to_positive_integer_test_() ->
    {inparallel,
        [
          {<<"Input <<\"-0\">> output -0">>,
                fun() ->
                    ?assertEqual(0,
                        something:to_positive_integer(<<"-0">>)
                    )
                end},
           {<<"Input <<\"0\">> output 0">>,
               fun() ->
                   ?assertEqual(2,
                       something:to_positive_integer(<<"2">>)
                   )
               end},
           {<<"Input <<\"-2\">> output eror">>,
               fun() ->
                   ?assertThrow({error, {cant_convert_to_positive_integer, _Number}},
                       something:to_positive_integer(<<"-2">>)
                   )
               end},
           {<<"Input <<\"2\">> output 2">>,
               fun() ->
                   ?assertEqual(2,
                       something:to_positive_integer(<<"2">>)
                   )
              end},
           {<<"Input <<\"-2.0\">> output error">>,
               fun() ->
                   ?assertThrow({error, {cant_convert_to_positive_integer, _Number}},
                       something:to_positive_integer(<<"-2.0">>)
                   )
               end},
           {<<"Input <<\"-2.01\">> output: throw">>,
               fun() ->
                ?assertThrow({error, {cant_convert_to_integer, _Number}},
                       something:to_positive_integer(<<"-2.01">>)
                   )
               end},
           {<<"Input <<\"-0.0\">> output 0">>,
               fun() ->
                   ?assertEqual(0,
                       something:to_positive_integer(<<"-0.0">>)
                   )
               end},
           {<<"Input <<\"-0.01\">> output: throw">>,
               fun() ->
                ?assertThrow({error, {cant_convert_to_integer, _Number}},
                       something:to_positive_integer(<<"-0.01">>)
                   )
               end},
           {<<"Input <<\"2.0\">> output 2">>,
               fun() ->
                   ?assertEqual(2,
                       something:to_positive_integer(<<"2.0">>)
                   )
               end},
           {<<"Input <<\"2.01\">> output: throw">>,
               fun() ->
                ?assertThrow({error, {cant_convert_to_integer, _Number}},
                       something:to_positive_integer(<<"2.01">>)
                   )
               end},
           {<<"Input <<\"2.00000000\">> output 2">>,
               fun() ->
                   ?assertEqual(2,
                       something:to_positive_integer(<<"2.00000000">>)
                   )
               end},
            {<<"Input 2.00000000000 output: 2">>,
                fun() ->
                    ?assertEqual(2,
                        something:to_positive_integer(<<"2.00000000000">>)
                    )
                end}
        ]
    }.

something_to_binaries_test_() ->
    {inparallel,
        [
          {<<"Input <<\"binary\">> output <<\"binary\">>">>,
                fun() ->
                    ?assertEqual(<<"binary">>,
                        something:to_binary(<<"binary">>)
                    )
                end},
          {<<"Input atom output <<\"atom\">>">>,
                fun() ->
                    ?assertEqual(<<"atom">>,
                        something:to_binary(atom)
                    )
                end},
          {<<"Input \"list\" output <<\"list\">>">>,
                fun() ->
                    ?assertEqual(<<"list">>,
                        something:to_binary("list")
                    )
                end},
          {<<"Input 123 output <<\"123\">>">>,
                fun() ->
                    ?assertEqual(<<"123">>,
                        something:to_binary(123)
                    )
                end},
          {<<"Input 123.23 output <<\"123.23\">>">>,
                fun() ->
                    ?assertEqual(<<"123.23">>,
                        something:to_binary(123.23)
                    )
                end},
          {<<"Input 123.23000 output <<\"123.23\">>">>,
                fun() ->
                    ?assertEqual(<<"123.23">>,
                        something:to_binary(123.23000)
                    )
                end}

        ]
    }.



something_integer_to_float_test_() ->
    {inparallel,
        [
            {<<"Input -0 output -0.0">>,
                fun() ->
                    ?assertEqual(-0.0,
                        something:to_float(-0)
                    )
                end},
            {<<"Input 0 output 0.0">>,
                fun() ->
                    ?assertEqual(0.0,
                        something:to_float(0)
                    )
                end},
            {<<"Input -2 output -2.0">>,
                fun() ->
                    ?assertEqual(-2.0,
                        something:to_float(-2)
                    )
                end},
            {<<"Input 2 output 2.0">>,
                fun() ->
                    ?assertEqual(2.0,
                        something:to_float(2)
                    )
              end}
      ]
  }.

something_integer_to_positive_float_test_() ->
    {inparallel,
        [
            {<<"Input -0 output 0.0">>,
                fun() ->
                    ?assertEqual(0.0,
                        something:to_positive_float(-0)
                    )
                end},
            {<<"Input 0 output 0.0">>,
                fun() ->
                    ?assertEqual(0.0,
                        something:to_positive_float(0)
                    )
                end},
            {<<"Input -2 output error">>,
                fun() ->
                    ?assertThrow({error, {cant_convert_to_positive_float, _Number}},
                        something:to_positive_float(-2)
                    )
                end},
            {<<"Input 2 output 2.0">>,
                fun() ->
                    ?assertEqual(2.0,
                        something:to_positive_float(2)
                    )
              end}
      ]
  }.


something_floats_to_floats_test_() ->
    {inparallel,
        [
            {<<"Input -2.0 output -2.0">>,
                fun() ->
                    ?assertEqual(-2.0,
                        something:to_float(-2.0)
                    )
                end},
            {<<"Input -2.01 output: -2.01">>,
                fun() ->
                    ?assertEqual(-2.01,
                        something:to_float(-2.01)
                    )
                end},
            {<<"Input -0.0 output -0.0">>,
                fun() ->
                    ?assertEqual(-0.0,
                        something:to_float(-0.0)
                    )
                end},
            {<<"Input -0.01 output: -0.01">>,
                fun() ->
                    ?assertEqual(-0.01,
                        something:to_float(-0.01)
                    )
                end},
            {<<"Input 2.0 output 2.0">>,
                fun() ->
                    ?assertEqual(2.0,
                        something:to_float(2.0)
                    )
                end},
            {<<"Input 2.01 output: 0.01">>,
                fun() ->
                    ?assertEqual(2.01,
                        something:to_float(2.01)
                    )
                end},
            {<<"Input 2.00000000 output 2.00000000">>,
                fun() ->
                    ?assertEqual(2.0,
                        something:to_float(2.00000000)
                    )
                end},
            {<<"Input 2.00000000000 output: 2.00000000">>,
                fun() ->
                    ?assertEqual(2.0,
                        something:to_float(2.00000000000)
                    )
                end},
            {<<"Input 2.000000000001 output: 2.00000000">>,
                fun() ->
                    ?assertEqual(2.0,
                        something:to_float(2.000000000001)
                    )
                end}
        ]
    }.


something_floats_to_positive_floats_test_() ->
    {inparallel,
        [
            {<<"Input -2.0 output error">>,
                fun() ->
                    ?assertThrow({error, {cant_convert_to_positive_float, _Number}},
                        something:to_positive_float(-2.0)
                    )
                end},
            {<<"Input -2.01 output: error">>,
                fun() ->
                    ?assertThrow({error, {cant_convert_to_positive_float, _Number}},
                        something:to_positive_float(-2.01)
                    )
                end},
            {<<"Input -0.0 output -0.0">>,
                fun() ->
                    ?assertEqual(0.0,
                        something:to_positive_float(-0.0)
                    )
                end},
            {<<"Input -0.01 output: error">>,
                fun() ->
                    ?assertThrow({error, {cant_convert_to_positive_float, _Number}},
                        something:to_positive_float(-0.01)
                    )
                end},
            {<<"Input 2.0 output 2.0">>,
                fun() ->
                    ?assertEqual(2.0,
                        something:to_positive_float(2.0)
                    )
                end},
            {<<"Input 2.01 output: 0.01">>,
                fun() ->
                    ?assertEqual(2.01,
                        something:to_positive_float(2.01)
                    )
                end},
            {<<"Input 2.00000000 output 2.0">>,
                fun() ->
                    ?assertEqual(2.0,
                        something:to_positive_float(2.00000000)
                    )
                end},
            {<<"Input 2.00000000000 output: 2.0">>,
                fun() ->
                    ?assertEqual(2.0,
                        something:to_positive_float(2.00000000000)
                    )
                end},
            {<<"Input 2.000000000001 output: 2.0">>,
                fun() ->
                    ?assertEqual(2.0,
                        something:to_positive_float(2.000000000001)
                    )
                end}
        ]
    }.

something_lists_to_float_test_() ->
    {inparallel,
        [
          {<<"Input \"-0\" output -0.0">>,
                fun() ->
                    ?assertEqual(-0.0,
                        something:to_float("-0")
                    )
                end},
            {<<"Input \"0\" output 0.0">>,
                fun() ->
                    ?assertEqual(0.0,
                        something:to_float("0")
                    )
                end},
            {<<"Input \"-2\" output -2">>,
                fun() ->
                    ?assertEqual(-2.0,
                        something:to_float("-2")
                    )
                end},
            {<<"Input \"2\" output 2.0">>,
                fun() ->
                    ?assertEqual(2.0,
                        something:to_float("2")
                    )
              end},
            {<<"Input \"-2.0\" output -2.0">>,
                fun() ->
                    ?assertEqual(-2.0,
                        something:to_float("-2.0")
                    )
                end},
            {<<"Input \"-2.01\" output: -2.01">>,
                fun() ->
                    ?assertEqual(-2.01,
                        something:to_float("-2.01")
                    )
                end},
            {<<"Input \"-0.0\" output -0.0">>,
                fun() ->
                    ?assertEqual(-0.0,
                        something:to_float("-0.0")
                    )
                end},
            {<<"Input \"-0.01\" output: -0.01">>,
                fun() ->
                    ?assertEqual(-0.01,
                        something:to_float("-0.01")
                    )
                end},
            {<<"Input \"2.0\" output 2.0">>,
                fun() ->
                    ?assertEqual(2.0,
                        something:to_float("2.0")
                    )
                end},
            {<<"Input \"2.01\" output: throw">>,
                fun() ->
                    ?assertEqual(2.01,
                        something:to_float("2.01")
                    )
                end},
            {<<"Input \"2.00\" output 2.0">>,
                fun() ->
                    ?assertEqual(2.0,
                        something:to_float("2.00")
                    )
                end},
            {<<"Input \"2.000\" output 2">>,
                fun() ->
                    ?assertEqual(2.0,
                        something:to_float("2.000")
                    )
                end},
            {<<"Input \"2.0000\" output 2">>,
                fun() ->
                    ?assertEqual(2.0,
                        something:to_float("2.0000")
                    )
                end},
            {<<"Input \"2.00000\" output 2">>,
                fun() ->
                    ?assertEqual(2.0,
                        something:to_float("2.00000")
                    )
                end},
            {<<"Input \"2.000000\" output 2">>,
                fun() ->
                    ?assertEqual(2.0,
                        something:to_float("2.000000")
                    )
                end},
            {<<"Input \"2.0000000\" output 2">>,
                fun() ->
                    ?assertEqual(2.0,
                        something:to_float("2.0000000")
                    )
                end},
            {<<"Input \"2.00000000\" output 2">>,
                fun() ->
                    ?assertEqual(2.0,
                        something:to_float("2.00000000")
                    )
                end},
            {<<"Input \"2.00000000000\" output: 2">>,
                fun() ->
                    ?assertEqual(2.0,
                        something:to_float("2.00000000000")
                    )
                end},
            {<<"Input \"2.00000000001\" output: 2">>,
                fun() ->
                    ?assertEqual(2.0,
                        something:to_float("2.00000000001")
                    )
                end},
            {<<"Input \"2.0001\" output: error">>,
                fun() ->
                    ?assertEqual(2.0001,
                        something:to_float("2.0001")
                    )
                end}
        ]
    }.


something_lists_to_positive_float_test_() ->
    {inparallel,
        [
          {<<"Input \"-0\" output 0.0">>,
                fun() ->
                    ?assertEqual(0.0,
                        something:to_positive_float("-0")
                    )
                end},
            {<<"Input \"0\" output 0.0">>,
                fun() ->
                    ?assertEqual(0.0,
                        something:to_positive_float("0")
                    )
                end},
            {<<"Input \"-2\" output error">>,
                fun() ->
                    ?assertThrow({error, {cant_convert_to_positive_float, _Number}},
                        something:to_positive_float("-2")
                    )
                end},
            {<<"Input \"2\" output 2.0">>,
                fun() ->
                    ?assertEqual(2.0,
                        something:to_positive_float("2")
                    )
              end},
            {<<"Input \"-2.0\" output error">>,
                fun() ->
                    ?assertThrow({error, {cant_convert_to_positive_float, _Number}},
                        something:to_positive_float("-2.0")
                    )
                end},
            {<<"Input \"-2.01\" output: error">>,
                fun() ->
                    ?assertThrow({error, {cant_convert_to_positive_float, _Number}},
                        something:to_positive_float("-2.01")
                    )
                end},
            {<<"Input \"-0.0\" output 0.0">>,
                fun() ->
                    ?assertEqual(0.0,
                        something:to_positive_float("-0.0")
                    )
                end},
            {<<"Input \"-0.01\" output: -0.01">>,
                fun() ->
                    ?assertThrow({error, {cant_convert_to_positive_float, _Number}},
                        something:to_positive_float("-0.01")
                    )
                end},
            {<<"Input \"2.0\" output 2.0">>,
                fun() ->
                    ?assertEqual(2.0,
                        something:to_positive_float("2.0")
                    )
                end},
            {<<"Input \"2.01\" output: throw">>,
                fun() ->
                    ?assertEqual(2.01,
                        something:to_positive_float("2.01")
                    )
                end},
            {<<"Input \"2.00\" output 2.0">>,
                fun() ->
                    ?assertEqual(2.0,
                        something:to_positive_float("2.00")
                    )
                end},
            {<<"Input \"2.000\" output 2">>,
                fun() ->
                    ?assertEqual(2.0,
                        something:to_positive_float("2.000")
                    )
                end},
            {<<"Input \"2.0000\" output 2">>,
                fun() ->
                    ?assertEqual(2.0,
                        something:to_positive_float("2.0000")
                    )
                end},
            {<<"Input \"2.00000\" output 2">>,
                fun() ->
                    ?assertEqual(2.0,
                        something:to_positive_float("2.00000")
                    )
                end},
            {<<"Input \"2.000000\" output 2">>,
                fun() ->
                    ?assertEqual(2.0,
                        something:to_positive_float("2.000000")
                    )
                end},
            {<<"Input \"2.0000000\" output 2">>,
                fun() ->
                    ?assertEqual(2.0,
                        something:to_positive_float("2.0000000")
                    )
                end},
            {<<"Input \"2.00000000\" output 2">>,
                fun() ->
                    ?assertEqual(2.0,
                        something:to_positive_float("2.00000000")
                    )
                end},
            {<<"Input \"2.00000000000\" output: 2">>,
                fun() ->
                    ?assertEqual(2.0,
                        something:to_positive_float("2.00000000000")
                    )
                end},
            {<<"Input \"2.00000000001\" output: 2">>,
                fun() ->
                    ?assertEqual(2.0,
                        something:to_positive_float("2.00000000001")
                    )
                end},
            {<<"Input \"2.0001\" output: error">>,
                fun() ->
                    ?assertEqual(2.0001,
                        something:to_positive_float("2.0001")
                    )
                end}
        ]
    }.

something_binaries_to_float_test_() ->
    {inparallel,
        [
          {<<"Input <<\"-0\">> output -0.0">>,
                fun() ->
                    ?assertEqual(-0.0,
                        something:to_float(<<"-0">>)
                    )
                end},
            {<<"Input <<\"0\">> output 0.0">>,
                fun() ->
                    ?assertEqual(0.0,
                        something:to_float(<<"0">>)
                    )
                end},
            {<<"Input <<\"-2\">> output -2">>,
                fun() ->
                    ?assertEqual(-2.0,
                        something:to_float(<<"-2">>)
                    )
                end},
            {<<"Input <<\"2\">> output 2.0">>,
                fun() ->
                    ?assertEqual(2.0,
                        something:to_float(<<"2">>)
                    )
              end},
            {<<"Input <<\"-2.0\">> output -2.0">>,
                fun() ->
                    ?assertEqual(-2.0,
                        something:to_float(<<"-2.0">>)
                    )
                end},
            {<<"Input <<\"-2.01\">> output: -2.01">>,
                fun() ->
                    ?assertEqual(-2.01,
                        something:to_float(<<"-2.01">>)
                    )
                end},
            {<<"Input <<\"-0.0\">> output -0.0">>,
                fun() ->
                    ?assertEqual(-0.0,
                        something:to_float(<<"-0.0">>)
                    )
                end},
            {<<"Input <<\"-0.01\">> output: -0.01">>,
                fun() ->
                    ?assertEqual(-0.01,
                        something:to_float(<<"-0.01">>)
                    )
                end},
            {<<"Input <<\"2.0\">> output 2.0">>,
                fun() ->
                    ?assertEqual(2.0,
                        something:to_float(<<"2.0">>)
                    )
                end},
            {<<"Input <<\"2.01\">> output: throw">>,
                fun() ->
                    ?assertEqual(2.01,
                        something:to_float(<<"2.01">>)
                    )
                end},
            {<<"Input <<\"2.00\">> output 2.0">>,
                fun() ->
                    ?assertEqual(2.0,
                        something:to_float(<<"2.00">>)
                    )
                end},
            {<<"Input <<\"2.000\">> output 2">>,
                fun() ->
                    ?assertEqual(2.0,
                        something:to_float(<<"2.000">>)
                    )
                end},
            {<<"Input <<\"2.0000\">> output 2">>,
                fun() ->
                    ?assertEqual(2.0,
                        something:to_float(<<"2.0000">>)
                    )
                end},
            {<<"Input <<\"2.00000\">> output 2">>,
                fun() ->
                    ?assertEqual(2.0,
                        something:to_float(<<"2.00000">>)
                    )
                end},
            {<<"Input <<\"2.000000\">> output 2">>,
                fun() ->
                    ?assertEqual(2.0,
                        something:to_float(<<"2.000000">>)
                    )
                end},
            {<<"Input <<\"2.0000000\">> output 2">>,
                fun() ->
                    ?assertEqual(2.0,
                        something:to_float(<<"2.0000000">>)
                    )
                end},
            {<<"Input <<\"2.00000000\">> output 2">>,
                fun() ->
                    ?assertEqual(2.0,
                        something:to_float(<<"2.00000000">>)
                    )
                end},
            {<<"Input <<\"2.00000000000\">> output: 2">>,
                fun() ->
                    ?assertEqual(2.0,
                        something:to_float(<<"2.00000000000">>)
                    )
                end},
            {<<"Input <<\"2.00000000001\">> output: 2">>,
                fun() ->
                    ?assertEqual(2.0,
                        something:to_float(<<"2.00000000001">>)
                    )
                end},
            {<<"Input <<\"2.0001\">> output: error">>,
                fun() ->
                    ?assertEqual(2.0001,
                        something:to_float(<<"2.0001">>)
                    )
                end}
        ]
    }.


something_binaries_to_positive_float_test_() ->
    {inparallel,
        [
          {<<"Input <<\"-0\">> output 0.0">>,
                fun() ->
                    ?assertEqual(0.0,
                        something:to_positive_float(<<"-0">>)
                    )
                end},
            {<<"Input <<\"0\">> output 0.0">>,
                fun() ->
                    ?assertEqual(0.0,
                        something:to_positive_float(<<"0">>)
                    )
                end},
            {<<"Input <<\"-2\">> output error">>,
                fun() ->
                    ?assertThrow({error, {cant_convert_to_positive_float, _Number}},
                        something:to_positive_float(<<"-2">>)
                    )
                end},
            {<<"Input <<\"2\">> output 2.0">>,
                fun() ->
                    ?assertEqual(2.0,
                        something:to_positive_float(<<"2">>)
                    )
              end},
            {<<"Input <<\"-2.0\">> output error">>,
                fun() ->
                    ?assertThrow({error, {cant_convert_to_positive_float, _Number}},
                        something:to_positive_float(<<"-2.0">>)
                    )
                end},
            {<<"Input <<\"-2.01\">> output: error">>,
                fun() ->
                    ?assertThrow({error, {cant_convert_to_positive_float, _Number}},
                        something:to_positive_float(<<"-2.01">>)
                    )
                end},
            {<<"Input <<\"-0.0\">> output 0.0">>,
                fun() ->
                    ?assertEqual(0.0,
                        something:to_positive_float(<<"-0.0">>)
                    )
                end},
            {<<"Input <<\"-0.01\">> output: -0.01">>,
                fun() ->
                    ?assertThrow({error, {cant_convert_to_positive_float, _Number}},
                        something:to_positive_float(<<"-0.01">>)
                    )
                end},
            {<<"Input <<\"2.0\">> output 2.0">>,
                fun() ->
                    ?assertEqual(2.0,
                        something:to_positive_float(<<"2.0">>)
                    )
                end},
            {<<"Input <<\"2.01\">> output: throw">>,
                fun() ->
                    ?assertEqual(2.01,
                        something:to_positive_float(<<"2.01">>)
                    )
                end},
            {<<"Input <<\"2.00\">> output 2.0">>,
                fun() ->
                    ?assertEqual(2.0,
                        something:to_positive_float(<<"2.00">>)
                    )
                end},
            {<<"Input <<\"2.000\">> output 2">>,
                fun() ->
                    ?assertEqual(2.0,
                        something:to_positive_float(<<"2.000">>)
                    )
                end},
            {<<"Input <<\"2.0000\">> output 2">>,
                fun() ->
                    ?assertEqual(2.0,
                        something:to_positive_float(<<"2.0000">>)
                    )
                end},
            {<<"Input <<\"2.00000\">> output 2">>,
                fun() ->
                    ?assertEqual(2.0,
                        something:to_positive_float(<<"2.00000">>)
                    )
                end},
            {<<"Input <<\"2.000000\">> output 2">>,
                fun() ->
                    ?assertEqual(2.0,
                        something:to_positive_float(<<"2.000000">>)
                    )
                end},
            {<<"Input <<\"2.0000000\">> output 2">>,
                fun() ->
                    ?assertEqual(2.0,
                        something:to_positive_float(<<"2.0000000">>)
                    )
                end},
            {<<"Input <<\"2.00000000\">> output 2">>,
                fun() ->
                    ?assertEqual(2.0,
                        something:to_positive_float(<<"2.00000000">>)
                    )
                end},
            {<<"Input <<\"2.00000000000\">> output: 2">>,
                fun() ->
                    ?assertEqual(2.0,
                        something:to_positive_float(<<"2.00000000000">>)
                    )
                end},
            {<<"Input <<\"2.00000000001\">> output: 2">>,
                fun() ->
                    ?assertEqual(2.0,
                        something:to_positive_float(<<"2.00000000001">>)
                    )
                end},
            {<<"Input <<\"2.0001\">> output: error">>,
                fun() ->
                    ?assertEqual(2.0001,
                        something:to_positive_float(<<"2.0001">>)
                    )
                end}
        ]
    }.


something_to_boolean_test_() ->
    {inparallel,
        [
            % binaries
            {<<"Input <<\"1\">> output true">>,
                fun() ->
                    ?assertEqual(true,
                        something:to_boolean(<<"1">>)
                    )
                end},
            {<<"Input <<\"1.0\">> output true">>,
                fun() ->
                    ?assertEqual(true,
                        something:to_boolean(<<"1.0">>)
                    )
                end},
            {<<"Input <<\"0\">> output false">>,
                fun() ->
                    ?assertEqual(false,
                        something:to_boolean(<<"0">>)
                    )
                end},
            {<<"Input <<\"0.0\">> output false">>,
                fun() ->
                    ?assertEqual(false,
                        something:to_boolean(<<"0.0">>)
                    )
                end},
            %lists 
            {<<"Input \"1\" output true">>,
                fun() ->
                    ?assertEqual(true,
                        something:to_boolean("1")
                    )
                end},
            {<<"Input \"1.0\" output true">>,
                fun() ->
                    ?assertEqual(true,
                        something:to_boolean("1.0")
                    )
                end},
            {<<"Input \"0\" output false">>,
                fun() ->
                    ?assertEqual(false,
                        something:to_boolean("0")
                    )
                end},
            {<<"Input \"0.0\" output false">>,
                fun() ->
                    ?assertEqual(false,
                        something:to_boolean("0.0")
                    )
                end},
            % integers 
            {<<"Input 1 output true">>,
                fun() ->
                    ?assertEqual(true,
                        something:to_boolean(1)
                    )
                end},
            {<<"Input 0 output false">>,
                fun() ->
                    ?assertEqual(false,
                        something:to_boolean(0)
                    )
                end},
            % floats
            {<<"Input 1.0 output true">>,
                fun() ->
                    ?assertEqual(true,
                        something:to_boolean(1.0)
                    )
                end},
            {<<"Input 0.0 output false">>,
                fun() ->
                    ?assertEqual(false,
                        something:to_boolean(0.0)
                    )
                end},
            % atoms
            {<<"Input true output true">>,
                fun() ->
                    ?assertEqual(true,
                        something:to_boolean(true)
                    )
                end},
            {<<"Input false output false">>,
                fun() ->
                    ?assertEqual(false,
                        something:to_boolean(false)
                    )
                end}
        ]
    }.

