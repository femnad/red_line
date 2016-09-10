-module(red_line).

%% API exports
-export([main/1]).

-define(CRITICAL_PERCENT, 13).
-define(NORMAL_SLEEP, 60 * 1000).
-define(WARNING_SLEEP, 5 * 1000).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(_) ->
    check_loop(),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================
notify(Title, Text) ->
    os:cmd(lists:flatten(io_lib:format("notify-send '~s' '~s'", [Title, Text]))).

check_loop() ->
    AcpiResult = os:cmd("acpi"),
    %% SP: space prefixed
    [BatteryAndStatus, SPPercentage, _] = string:tokens(AcpiResult, ",\n"),
    Percentage = string:strip(SPPercentage),
    [_, _, Status] = string:tokens(BatteryAndStatus, " :"),
    {Percent, _} = string:to_integer(Percentage),
    case Status of
        "Charging" ->
            timer:sleep(?NORMAL_SLEEP);
        _ ->
            if Percent =< ?CRITICAL_PERCENT ->
                    notify("Battery Low", Percentage),
                    timer:sleep(?WARNING_SLEEP);
               true ->
                    ok
            end
    end,
    check_loop().
