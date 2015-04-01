% Takes a serialized php object and turns it into an erlang data structure
% This module is heavily inspired by Richard Jones
% Read more here: http://www.metabrew.com/article/reading-serialized-php-objects-from-erlang

-module('php_serializer').
-export([serialize/1, unserialize/1]).
-include_lib("eunit/include/eunit.hrl").

serialize(Item) when is_binary(Item)->
    List = unicode:characters_to_list(Item),
    Length = length(List),
    <<"s:", (integer_to_binary(Length))/binary, ":", (escape_binary(Item))/binary, ";">>;
serialize(Item) when is_float(Item)->
    <<"d:", (float_to_binary(Item))/binary, ";">>;
serialize(Item) when is_integer(Item)->
    <<"i:", (integer_to_binary(Item))/binary, ";">>;
serialize(null) ->
    <<"N;">>;
serialize(true) ->
    <<"b:1;">>;
serialize(false) ->
    <<"b:0;">>;
serialize(Item) when is_list(Item) ->
    %% Now the fun begins!!
    serialize(Item, <<"a:", (integer_to_binary(length(Item)))/binary, ":{">>).

serialize([{Key, Value} | List], Acc) ->
    Acc1 = <<Acc/binary, (serialize(Key))/binary, (serialize(Value))/binary>>,
    serialize(List, Acc1);
serialize([], Acc) ->
    <<Acc/binary, "}">>.


escape_binary(Bin) ->
    <<"\"", Bin/binary, "\"">>.

unserialize(<<"a:", Rest/binary>>) ->
    case re:run(Rest, <<"\\d:{">>) of
        {match, [{_Pos, Length}]} ->
            {_, Rest1} = split_binary(Rest, Length),
            unserialize(Rest1, [])
    end;
unserialize(<<"b:", Rest/binary>>) ->
    Boolean = binary:replace(Rest, <<";">>, <<>>),
    case clean_binary(Boolean) of
        <<"1">> -> true;
        <<"0">> -> false
    end;
unserialize(<<"i:", Value/binary>>) ->
    binary_to_integer(clean_binary(Value));
unserialize(<<"d:", Value/binary>>) ->
    binary_to_float(clean_binary(Value));
unserialize(<<"N;">>) ->
    null;
unserialize(<<"N">>) ->
    null;
unserialize(<<"s:", Rest/binary>>) ->
    case re:run(Rest, <<"^\\d+:">>) of
        {match, [{_Pos, Length}]} ->
            {_, Bin} = split_binary(Rest, Length),
            Bin1 = binary:replace(Bin, <<"\"">>, <<>>, [global]),
            clean_binary(Bin1)
    end;
unserialize(<<"O:", _Value/binary>>) ->
    %% Not handled as of yet.
    <<>>;
unserialize(<<>>) ->
    <<>>.

%% Private
unserialize(<<>>, Acc) ->
    make_proplist(lists:reverse(Acc), []);
unserialize(<<"a:", _Rest/binary>> = ArrayElement, Acc) ->
    case re:run(ArrayElement, <<"^a:\\d:{[^}]*}+">>) of
        {match, [{_Pos, PatternLength}]} ->
            {Array, Rest1} = split_binary(ArrayElement, PatternLength),
            Values = unserialize(Array),
            Acc1 = [Values | Acc],
            unserialize(Rest1, Acc1)
    end;
unserialize(ArrayElements, Acc) ->
    Rest = re:replace(ArrayElements, <<"^}*">>, <<>>, [{return, binary}]),
    [Element, Rest2] = case Rest of
        <<>> -> [<<>>, <<>>];
        _       -> binary:split(Rest, <<";">>)
    end,
    Value = unserialize(Element),
    Acc1 = case Value of
        <<>> -> Acc;
        _    -> [Value | Acc]
    end,
    case Rest2 of
        <<>> ->
            make_proplist(lists:reverse(Acc1), []);
        _ ->
            unserialize(Rest2, Acc1)
    end.

%% Remove trailing ;
clean_binary(Value) when is_binary(Value) ->
    binary:replace(Value, <<"\;">>, <<>>, [global]).

make_proplist([K, V | T] = List, Acc) when List > 1; is_integer(V); is_binary(V); is_atom(V) ->
    make_proplist(T, [{K, V} | Acc]);
make_proplist(List, Acc) when List == 1 ->
    lists:reverse([List | Acc]);
make_proplist([K, V | T], Acc) when is_list(V) ->
    make_proplist(T, [{K, make_proplist(V, [])} | Acc]);
make_proplist([], Acc) ->
    lists:reverse(Acc).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
unserialize_test() ->
    ?assertEqual(455, unserialize(<<"i:455;">>)),
    ?assertEqual(<<"455">>, unserialize(<<"s:3:455;">>)),
    ?assertEqual(<<"foo">>, unserialize(<<"s:3:foo;">>)),
    ?assertEqual(true, unserialize(<<"b:1;">>)),
    ?assertEqual(false, unserialize(<<"b:0;">>)),
    ?assertEqual([{0, <<"foo">>}], unserialize(<<"a:1:{i:0;s:3:\"foo\";}">>)),
    ?assertEqual([{0, <<"foobar">>}, {1, 1234}, {2, true}],
                 unserialize(<<"a:3:{i:0;s:6:\"foobar\";i:1;i:1234;i:2;b:1;}">>)),
    ?assertEqual([{0, [{0, <<"foobar">>}]}, {1, 420}],
                 unserialize(<<"a:2:{i:0;a:1:{i:0;s:6:\"foobar\";}i:1;i:420;}">>)),
    ?assertEqual([{<<"foo">>, [{0, 242}, {1, null}]}],
                 unserialize(<<"a:1:{s:3:\"foo\";a:2:{i:0;i:242;i:1;N;}}">>)),
    ?assertEqual(
        [{<<"barfoo">>, [{0, <<"foobar">>}, {1, 1234}, {2, false}]}, {0, 11234}, {<<"bar">>, <<"foo">>}],
        unserialize(<<"a:3:{s:6:\"barfoo\";a:3:{i:0;s:6:\"foobar\";i:1;i:1234;i:2;b:0;}",
                      "i:0;i:11234;s:3:\"bar\";s:3:\"foo\";}">>)
    ),
    BigString = <<"a:7:{s:3:\"eid\";i:12345;s:6:\"secret\";s:15:\"abcdefgjijklmno\";s:8:\"testmode\"",
                   ";N;s:15:\"ordercost_range\";a:2:{s:4:\"mode\";s:3:\"all\";s:9:\"intervals\";a:1:",
                   "{i:14;a:2:{s:3:\"min\";N;s:3:\"max\";N;}}}s:21:\"disable_deliv_address\";b:0;s:",
                   "11:\"orderstatus\";N;s:9:\"active_SE\";b:1;}">>,
    Expected = [
        {<<"eid">>, 12345},
        {<<"secret">>, <<"abcdefgjijklmno">>},
        {<<"testmode">>, null},
        {<<"ordercost_range">>, [
            {<<"mode">>, <<"all">>},
            {<<"intervals">>, [
                {14, [
                    {<<"min">>, null},
                    {<<"max">>, null}
                ]}
            ]}
        ]},
        {<<"disable_deliv_address">>, false},
        {<<"orderstatus">>, null},
        {<<"active_SE">>, true}
    ],
    Result = unserialize(BigString),
    ?assertEqual(Expected, Result).

serialize_test() ->
    ?assertEqual(<<"s:6:\"Foobar\";">>, serialize(<<"Foobar">>)),
    ?assertEqual(<<"d:1.00000999999999997669e+01;">>, serialize(10.0001)),
    ?assertEqual(<<"N;">>, serialize(null)),
    ?assertEqual(<<"b:1;">>, serialize(true)),
    ?assertEqual(<<"b:0;">>, serialize(false)),
    BigList = [
        {<<"eid">>, 12345},
        {<<"secret">>, <<"abcdefgjijklmno">>},
        {<<"testmode">>, null},
        {<<"ordercost_range">>, [
            {<<"mode">>, <<"all">>},
            {<<"intervals">>, [
                {14, [
                    {<<"min">>, null},
                    {<<"max">>, null}
                ]}
            ]}
        ]},
        {<<"disable_deliv_address">>, false},
        {<<"orderstatus">>, null},
        {<<"active_SE">>, true}
    ],
    Expected = <<"a:7:{s:3:\"eid\";i:12345;s:6:\"secret\";s:15:\"abcdefgjijklmno\";s:8:\"testmode\"",
                   ";N;s:15:\"ordercost_range\";a:2:{s:4:\"mode\";s:3:\"all\";s:9:\"intervals\";a:1:",
                   "{i:14;a:2:{s:3:\"min\";N;s:3:\"max\";N;}}}s:21:\"disable_deliv_address\";b:0;s:",
                   "11:\"orderstatus\";N;s:9:\"active_SE\";b:1;}">>,
    Result = serialize(BigList),
    ?assertEqual(Expected, Result).

combined_test_() ->
    List = [<<"a">>, 1, 1.0, null, []],
    Proplist = lists:zip(lists:seq(0, length(List) - 1), List),
    Values = [<<"a">>, 1, 1.0, null, [], Proplist],
    [?_assertEqual(Value, unserialize(serialize(Value))) || Value <- Values].

-endif.
