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

parse_acpi_output(AcpiOutput) ->
    %% SP: space prefixed
    case string:tokens(AcpiOutput, ",\n") of
        [BatteryAndStatus, SPPercentage, _] ->
            Percentage = string:strip(SPPercentage),
            [_, _, Status] = string:tokens(BatteryAndStatus, " :"),
            {Percent, _} = string:to_integer(Percentage),
            {Status, Percent};
        [_, _] ->
            unknown
    end.

handle_battery_status(Status, _) when Status == "Charging" ->
    timer:sleep(?NORMAL_SLEEP);
handle_battery_status(_, Percent) ->
    if Percent =< ?CRITICAL_PERCENT ->
            Percentage = io_lib:format("~i", [Percent]),
            notify("Battery Low", Percentage),
            timer:sleep(?WARNING_SLEEP);
       true ->
            ok
    end.

check_loop() ->
    AcpiResult = os:cmd("acpi"),
    case parse_acpi_output(AcpiResult) of
        unknown ->
            % Hope for the best and try again
            check_loop();
        {Status, Percent} ->
            handle_battery_status(Status, Percent)
    end,
    check_loop().
