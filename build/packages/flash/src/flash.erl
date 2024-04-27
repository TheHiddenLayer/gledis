-module(flash).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([parse_level/1, level_to_string/1, new/2, with_attrs/2, with_attr/2, with_group/2, enabled/2, log/3, debug/2, info/2, warn/2, error/2, json_writer/3, text_writer/3]).
-export_type([attr/0, level/0, logger/0]).

-type attr() :: {bool_attr, binary(), boolean()} |
    {float_attr, binary(), float()} |
    {group_attr, binary(), list(attr())} |
    {int_attr, binary(), integer()} |
    {string_attr, binary(), binary()}.

-type level() :: debug_level | info_level | warn_level | error_level.

-type logger() :: {logger,
        level(),
        fun((level(), binary(), list(attr())) -> nil),
        gleam@option:option(logger()),
        binary(),
        list(attr())}.

-spec parse_level(binary()) -> {ok, level()} | {error, nil}.
parse_level(Level) ->
    case gleam@string:lowercase(Level) of
        <<"debug"/utf8>> ->
            {ok, debug_level};

        <<"info"/utf8>> ->
            {ok, info_level};

        <<"warn"/utf8>> ->
            {ok, warn_level};

        <<"error"/utf8>> ->
            {ok, error_level};

        _ ->
            {error, nil}
    end.

-spec level_to_string(level()) -> binary().
level_to_string(Level) ->
    case Level of
        debug_level ->
            <<"debug"/utf8>>;

        info_level ->
            <<"info"/utf8>>;

        warn_level ->
            <<"warn"/utf8>>;

        error_level ->
            <<"error"/utf8>>
    end.

-spec new(level(), fun((level(), binary(), list(attr())) -> nil)) -> logger().
new(Level, Writer) ->
    {logger, Level, Writer, none, <<""/utf8>>, []}.

-spec with_attrs(logger(), list(attr())) -> logger().
with_attrs(Logger, Attrs) ->
    erlang:setelement(
        6,
        Logger,
        gleam@list:append(erlang:element(6, Logger), Attrs)
    ).

-spec with_attr(logger(), attr()) -> logger().
with_attr(Logger, Attr) ->
    with_attrs(Logger, [Attr]).

-spec with_group(logger(), binary()) -> logger().
with_group(Logger, Group) ->
    erlang:setelement(
        6,
        erlang:setelement(
            5,
            erlang:setelement(4, Logger, {some, Logger}),
            Group
        ),
        []
    ).

-spec level_to_int(level()) -> integer().
level_to_int(Level) ->
    case Level of
        debug_level ->
            0;

        info_level ->
            1;

        warn_level ->
            2;

        error_level ->
            3
    end.

-spec enabled(logger(), level()) -> boolean().
enabled(Logger, Level) ->
    level_to_int(Level) >= level_to_int(erlang:element(2, Logger)).

-spec attr_compare(attr(), attr()) -> gleam@order:order().
attr_compare(A, B) ->
    A_is_group = case A of
        {group_attr, _, _} ->
            true;

        _ ->
            false
    end,
    B_is_group = case B of
        {group_attr, _, _} ->
            true;

        _ ->
            false
    end,
    case {A_is_group, B_is_group} of
        {true, true} ->
            gleam@string:compare(erlang:element(2, A), erlang:element(2, B));

        {false, false} ->
            gleam@string:compare(erlang:element(2, A), erlang:element(2, B));

        {true, false} ->
            gt;

        {_, _} ->
            lt
    end.

-spec logger_to_attrs(logger()) -> list(attr()).
logger_to_attrs(Logger) ->
    case erlang:element(4, Logger) of
        {some, Parent} ->
            logger_to_attrs(
                {logger,
                    erlang:element(2, Parent),
                    erlang:element(3, Parent),
                    erlang:element(4, Parent),
                    erlang:element(5, Parent),
                    [{group_attr,
                            erlang:element(5, Logger),
                            erlang:element(6, Logger)} |
                        erlang:element(6, Parent)]}
            );

        none ->
            erlang:element(6, Logger)
    end.

-spec log(logger(), level(), binary()) -> nil.
log(Logger, Level, Message) ->
    case enabled(Logger, Level) of
        true ->
            (erlang:element(3, Logger))(Level, Message, logger_to_attrs(Logger));

        false ->
            nil
    end.

-spec debug(logger(), binary()) -> nil.
debug(Logger, Message) ->
    log(Logger, debug_level, Message).

-spec info(logger(), binary()) -> nil.
info(Logger, Message) ->
    log(Logger, info_level, Message).

-spec warn(logger(), binary()) -> nil.
warn(Logger, Message) ->
    log(Logger, warn_level, Message).

-spec error(logger(), binary()) -> nil.
error(Logger, Message) ->
    log(Logger, error_level, Message).

-spec attr_to_json_value(attr()) -> gleam@json:json().
attr_to_json_value(Attr) ->
    case Attr of
        {bool_attr, _, Value} ->
            gleam@json:bool(Value);

        {float_attr, _, Value@1} ->
            gleam@json:float(Value@1);

        {group_attr, _, Value@2} ->
            _pipe = Value@2,
            _pipe@1 = gleam@list:map(
                _pipe,
                fun(Attr@1) ->
                    {erlang:element(2, Attr@1), attr_to_json_value(Attr@1)}
                end
            ),
            gleam@json:object(_pipe@1);

        {int_attr, _, Value@3} ->
            gleam@json:int(Value@3);

        {string_attr, _, Value@4} ->
            gleam@json:string(Value@4)
    end.

-spec attr_to_text(attr()) -> gleam@string_builder:string_builder().
attr_to_text(Attr) ->
    From_strings = fun gleam@string_builder:from_strings/1,
    case Attr of
        {bool_attr, Key, Value} ->
            From_strings([Key, <<"="/utf8>>, gleam@bool:to_string(Value)]);

        {float_attr, Key@1, Value@1} ->
            From_strings([Key@1, <<"="/utf8>>, gleam@float:to_string(Value@1)]);

        {group_attr, Key@2, Value@2} ->
            _pipe = Value@2,
            _pipe@1 = gleam@list:map(
                _pipe,
                fun(Attr@1) ->
                    Key@3 = gleam@string:join(
                        [Key@2, erlang:element(2, Attr@1)],
                        <<"."/utf8>>
                    ),
                    attr_to_text(case Attr@1 of
                            {bool_attr, _, Value@3} ->
                                {bool_attr, Key@3, Value@3};

                            {float_attr, _, Value@4} ->
                                {float_attr, Key@3, Value@4};

                            {group_attr, _, Value@5} ->
                                {group_attr, Key@3, Value@5};

                            {int_attr, _, Value@6} ->
                                {int_attr, Key@3, Value@6};

                            {string_attr, _, Value@7} ->
                                {string_attr, Key@3, Value@7}
                        end)
                end
            ),
            gleam@string_builder:join(_pipe@1, <<" "/utf8>>);

        {int_attr, Key@4, Value@8} ->
            From_strings([Key@4, <<"="/utf8>>, gleam@int:to_string(Value@8)]);

        {string_attr, Key@5, Value@9} ->
            From_strings([Key@5, <<"="/utf8>>, Value@9])
    end.

-spec unique_by(list(GZU), fun((GZU, GZU) -> boolean())) -> list(GZU).
unique_by(List, Predicate) ->
    case List of
        [] ->
            [];

        [First | Rest] ->
            [First |
                unique_by(
                    gleam@list:filter(
                        Rest,
                        fun(Next) -> Predicate(First, Next) end
                    ),
                    Predicate
                )]
    end.

-spec prepare_attrs(list(attr())) -> list(attr()).
prepare_attrs(Attrs) ->
    _pipe = Attrs,
    _pipe@1 = gleam@list:filter(_pipe, fun(Attr) -> case Attr of
                {group_attr, _, Value} ->
                    Value /= [];

                _ ->
                    true
            end end),
    _pipe@2 = gleam@list:reverse(_pipe@1),
    _pipe@3 = unique_by(
        _pipe@2,
        fun(A, B) -> erlang:element(2, A) /= erlang:element(2, B) end
    ),
    _pipe@4 = gleam@list:sort(_pipe@3, fun attr_compare/2),
    gleam@list:map(_pipe@4, fun(Attr@1) -> case Attr@1 of
                {group_attr, Key, Value@1} ->
                    {group_attr, Key, prepare_attrs(Value@1)};

                _ ->
                    Attr@1
            end end).

-spec json_writer(level(), binary(), list(attr())) -> nil.
json_writer(Level, Message, Attrs) ->
    Attrs@1 = begin
        _pipe = Attrs,
        _pipe@1 = prepare_attrs(_pipe),
        gleam@list:map(
            _pipe@1,
            fun(Attr) -> {erlang:element(2, Attr), attr_to_json_value(Attr)} end
        )
    end,
    _pipe@2 = gleam@json:object(
        [{<<"level"/utf8>>, gleam@json:string(level_to_string(Level))},
            {<<"time"/utf8>>, gleam@json:string(birl:to_iso8601(birl:now()))},
            {<<"message"/utf8>>, gleam@json:string(Message)} |
            Attrs@1]
    ),
    _pipe@3 = gleam@json:to_string(_pipe@2),
    gleam@io:println(_pipe@3).

-spec text_writer(level(), binary(), list(attr())) -> nil.
text_writer(Level, Message, Attrs) ->
    Now = birl:get_time_of_day(birl:now()),
    Message@1 = gleam@string:pad_right(Message, 45, <<" "/utf8>>),
    Level@1 = begin
        _pipe = level_to_string(Level),
        _pipe@1 = gleam@string:uppercase(_pipe),
        gleam@string:pad_right(_pipe@1, 5, <<" "/utf8>>)
    end,
    Time_builder = gleam@string_builder:from_strings(
        [gleam@string:pad_left(
                gleam@int:to_string(erlang:element(2, Now)),
                2,
                <<"0"/utf8>>
            ),
            <<":"/utf8>>,
            gleam@string:pad_left(
                gleam@int:to_string(erlang:element(3, Now)),
                2,
                <<"0"/utf8>>
            ),
            <<":"/utf8>>,
            gleam@string:pad_left(
                gleam@int:to_string(erlang:element(4, Now)),
                2,
                <<"0"/utf8>>
            )]
    ),
    Attrs@1 = begin
        _pipe@2 = Attrs,
        _pipe@3 = prepare_attrs(_pipe@2),
        gleam@list:map(_pipe@3, fun attr_to_text/1)
    end,
    _pipe@4 = gleam@string_builder:join(
        [Time_builder,
            gleam@string_builder:from_string(Level@1),
            gleam@string_builder:from_string(Message@1) |
            Attrs@1],
        <<" "/utf8>>
    ),
    _pipe@5 = gleam@string_builder:to_string(_pipe@4),
    gleam@io:println(_pipe@5).
