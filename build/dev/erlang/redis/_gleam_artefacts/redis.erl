-module(redis).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([main/0]).

-spec init(any()) -> {nil, gleam@option:option(any())}.
init(_) ->
    {nil, none}.

-spec build_pong_msg() -> gleam@bytes_builder:bytes_builder().
build_pong_msg() ->
    gleam_stdlib:wrap_list(<<"+PONG\r\n"/utf8>>).

-spec loop(any(), glisten:message(any()), ALX, glisten:connection(any())) -> gleam@otp@actor:next(any(), ALX).
loop(Logger, Msg, State, Conn) ->
    {packet, Msg@1} = case Msg of
        {packet, _} -> Msg;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"redis"/utf8>>,
                        function => <<"loop"/utf8>>,
                        line => 32})
    end,
    _assert_subject = gleam@bit_array:to_string(Msg@1),
    Command = case _assert_subject of
        _ -> _assert_subject;
        _assert_fail@1 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail@1,
                        module => <<"redis"/utf8>>,
                        function => <<"loop"/utf8>>,
                        line => 33})
    end,
    Response = build_pong_msg(),
    _assert_subject@1 = glisten:send(Conn, Response),
    {ok, _} = case _assert_subject@1 of
        {ok, _} -> _assert_subject@1;
        _assert_fail@2 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail@2,
                        module => <<"redis"/utf8>>,
                        function => <<"loop"/utf8>>,
                        line => 43})
    end,
    gleam@otp@actor:continue(State).

-spec build_logger() -> flash:logger().
build_logger() ->
    flash:new(info_level, fun flash:json_writer/3).

-spec main() -> nil.
main() ->
    Logger = build_logger(),
    gleam@io:println(<<"Logs from your program will appear here!"/utf8>>),
    _assert_subject = begin
        _pipe = glisten:handler(
            fun init/1,
            fun(Msg, State, Conn) -> loop(Logger, Msg, State, Conn) end
        ),
        glisten:serve(_pipe, 6379)
    end,
    {ok, _} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"redis"/utf8>>,
                        function => <<"main"/utf8>>,
                        line => 18})
    end,
    gleam_erlang_ffi:sleep_forever().
