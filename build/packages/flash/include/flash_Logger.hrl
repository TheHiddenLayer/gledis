-record(logger, {
    level :: flash:level(),
    writer :: fun((flash:level(), binary(), list(flash:attr())) -> nil),
    parent :: gleam@option:option(flash:logger()),
    group :: binary(),
    attrs :: list(flash:attr())
}).
