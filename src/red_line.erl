-module(red_line).

%% API exports
-export([main/1, acpi_parse_test/0]).

-define(CRITICAL_PERCENT, 13).
-define(FULL_SLEEP, 5 * 60 * 1000).
-define(NORMAL_SLEEP, 60 * 1000).
-define(PID_FILE, "/tmp/red_line.pid").
-define(WARNING_SLEEP, 5 * 1000).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(_) ->
    start(),
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
        [_, Percentage] ->
            case string:strip(Percentage) of
                "100%" ->
                    full;
                _ ->
                    unknown
            end
    end.

handle_battery_status(Status, _) when Status == "Charging" ->
    timer:sleep(?NORMAL_SLEEP);
handle_battery_status(_, Percent) ->
    if Percent =< ?CRITICAL_PERCENT ->
            Percentage = lists:flatten(io_lib:format("~p%", [Percent])),
            notify("Battery Low", Percentage),
            timer:sleep(?WARNING_SLEEP);
       true ->
            ok
    end.

get_acpi_info() ->
    os:cmd("acpi").

check_loop() ->
    AcpiResult = get_acpi_info(),
    case parse_acpi_output(AcpiResult) of
        unknown ->
            % Hope for the best and try again
            check_loop();
        full ->
            notify("Battery Full", "Adapter away!"),
            timer:sleep(?FULL_SLEEP),
            check_loop();
        {Status, Percent} ->
            handle_battery_status(Status, Percent)
    end,
    check_loop().

process_exists_with_saved_pid() ->
    {ok, File} = file:open(?PID_FILE, [read]),
    {ok, Line} = file:read_line(File),
    SavedPid = string:tokens(Line, "\n"),
    PsCommand = lists:flatten(io_lib:format("ps -f --pid ~s", [SavedPid])),
    PsResult = os:cmd(PsCommand),
    case string:tokens(PsResult, "\n") of
        [_Header, _Process] ->
            % If a process with that PID exists it must be red_line, right?
            true;
        [_Header] ->
            false
    end.

save_pid_to_file() ->
    {ok, File} = file:open(?PID_FILE, [write, exclusive]),
    PidLine = lists:flatten(io_lib:format("~s~n", [os:getpid()])),
    file:write(File, PidLine),
    file:close(File).

start_loop() ->
    save_pid_to_file(),
    check_loop().

start() ->
    case file:open(?PID_FILE, read) of
        {error, _} ->
            start_loop();
        {ok, _} ->
            case process_exists_with_saved_pid() of
                true ->
                    io:format("A red_line process is already active~n");
                false ->
                    %% The PID file is stale
                    file:delete(?PID_FILE),
                    start_loop()
            end
    end.

acpi_parse_test() ->
    FullAcpi = "Battery 0: Full, 100%\n",
    full = parse_acpi_output(FullAcpi),
    SomeNonCriticalBattery = "Battery 0: Discharging, 60%, 01:59:37 remaining",
    {"Discharging", 60} = parse_acpi_output(SomeNonCriticalBattery),
    ok.
