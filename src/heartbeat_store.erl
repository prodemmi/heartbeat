-module(heartbeat_store).

-export([init/0, heal/1, checkup/1, heart_rhythm/1]).

-include("heartbeat.hrl").

%%========================================================================
%% init/0: init mnesia
%%========================================================================
init() ->
    Node = node(),
    case mnesia:create_schema([Node]) of
        ok ->
            ok;
        {error, {_, {already_exists, _}}} ->
            ok;
        {'EXIT', {{already_exists, _}, _}} ->
            ok
    end,

    mnesia:start(),

    case mnesia:create_table(history,
                             [{type, bag},
                              {attributes, record_info(fields, history)},
                              {index, [user_id]},
                              {disc_copies, [Node]}])
    of
        {atomic, ok} ->
            ok;
        {aborted, {already_exists, history}} ->
            ok
    end.

%%========================================================================
%% heal/1: updates user status to online
%%========================================================================
heal(UserId) ->
    Timestamp = erlang:system_time(seconds),
    Expire = Timestamp + 7,
    Fun = fun() ->
        Histories =
            mnesia:match_object(#history{user_id = UserId,
                                         online_at = '_',
                                         offline_at = '_'}),

        UpdatedHistory =
            case Histories of
                [] ->
                    #history{user_id = UserId,
                             online_at = Timestamp,
                             offline_at = Expire};

                _ ->
                    Last = lists:last(lists:keysort(#history.offline_at, Histories)),

                    if Last#history.offline_at >= Timestamp ->
                           mnesia:delete_object(Last),
                           Last#history{offline_at = Expire};
                       true ->
                           #history{user_id = UserId,
                                    online_at = Timestamp,
                                    offline_at = Expire}
                    end
            end,
        mnesia:write(UpdatedHistory)
    end,
    mnesia:transaction(Fun).

%%========================================================================
%% checkup/1: gets user status
%%========================================================================
checkup(UserId) ->
    Now = erlang:system_time(seconds),
    Fun = fun() ->
             Histories =
                 mnesia:match_object(#history{user_id = UserId,
                                              online_at = '_',
                                              offline_at = '_'}),
             Status =
                 case Histories of
                     [] -> offline;
                     _ ->
                         Last = lists:last(Histories),
                         if Last#history.offline_at > Now -> online;
                            true -> offline
                         end
                 end,
             #user{id = UserId,
                   status = Status,
                   histories = Histories}
          end,
    {atomic, Result} = mnesia:transaction(Fun),
    Result.

%%========================================================================
%% heart_rhythm/1: gets all user history status
%%========================================================================
heart_rhythm(UserId) when is_integer(UserId) ->
    F = fun() ->
           mnesia:match_object(#history{user_id = UserId,
                                        online_at = '_',
                                        offline_at = '_'})
        end,
    case mnesia:transaction(F) of
        {atomic, []} ->
            {error, not_found};
        {atomic, Records} ->
            SortedRecords = lists:keysort(#history.online_at, Records),
            {ok, SortedRecords};
        {aborted, {no_exists, _}} ->
            lager:warning("Table does not exist during heart_rhythm for user ~p", [UserId]),
            {error, not_found};
        {aborted, Reason} ->
            lager:error("Transaction aborted in heart_rhythm for user ~p: ~p", [UserId, Reason]),
            {error, transaction_failed};
        Other ->
            {error, Other}
    end.
