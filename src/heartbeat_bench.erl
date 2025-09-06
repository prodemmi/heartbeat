-module(heartbeat_bench).
-export([start/2, simulate_user/1, monitor/0]).

-include("heartbeat.hrl").

start(N, Duration) ->
    rand:seed(exsplus, {erlang:monotonic_time(), erlang:unique_integer(), erlang:phash2(self())}),

    lists:foreach(fun(Id) ->
        spawn(fun() -> simulate_user(Id) end)
    end, lists:seq(1, N)),

    spawn(fun monitor/0),

    timer:sleep(Duration),
    io:format("Benchmark finished~n").

simulate_user(UserId) ->
    heartbeat_store:heal(UserId),
    Random = rand:uniform(10),
    Delay =
        case Random of
            1 -> 15000;
            2 -> 14000;
            3 -> 13000;
            4 -> 12000;
            5 -> 11000;
            _ -> 7000
        end,
    timer:sleep(Delay),
    simulate_user(UserId).

monitor() ->
    Fun = fun() ->
        Procs = erlang:system_info(process_count),
        Mem = erlang:memory(),
        TotalMB = proplists:get_value(total, Mem) div 1024 div 1024,
        ProcessesMB = proplists:get_value(processes, Mem) div 1024 div 1024,
        SystemMB = proplists:get_value(system, Mem) div 1024 div 1024,
        {atomic, HistoriesCount} = mnesia:transaction(fun() ->
            length(mnesia:match_object(#history{user_id = '_',
                                                online_at = '_',
                                                offline_at = '_'}))
        end),
        io:format("~n[Monitor] Processes: ~p, Memory: total ~p MB, processes ~p MB, system ~p MB, Total Histories: ~p~n",
                  [Procs, TotalMB, ProcessesMB, SystemMB, HistoriesCount])
    end,
    repeat(Fun, 5000).

repeat(Fun, Interval) ->
    Fun(),
    timer:sleep(Interval),
    repeat(Fun, Interval).
