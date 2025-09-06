-module(heart_rhythm_handler).
-behaviour(cowboy_handler).

-export([init/2]).

-include("heartbeat.hrl").

init(Req, State) ->
    Result = try
        UserIdBin = cowboy_req:binding(user_id, Req),
        case UserIdBin of
            undefined ->
                cowboy_req:reply(400,
                                  #{<<"content-type">> => <<"application/json">>},
                                  jsx:encode(#{error => <<"missing user_id parameter">>}),
                                  Req);
            _ ->
                UserId = case catch binary_to_integer(UserIdBin) of
                    {'EXIT', _} -> throw({invalid_user_id, UserIdBin});
                    ValidUserId when is_integer(ValidUserId) -> ValidUserId
                end,

                case heartbeat_store:heart_rhythm(UserId) of
                    {ok, History} ->
                        HistoryJson = [#{online_at => H#history.online_at,
                                         offline_at => H#history.offline_at}
                                       || H <- History],
                        JSON = jsx:encode(#{id => UserId,
                                            history => HistoryJson}),
                        cowboy_req:reply(200,
                                         #{<<"content-type">> => <<"application/json">>},
                                         JSON,
                                         Req);

                    {error, not_found} ->
                        JSON = jsx:encode(#{id => UserId,
                                            history => []}),
                        cowboy_req:reply(200,
                                         #{<<"content-type">> => <<"application/json">>},
                                         JSON,
                                         Req);
                    Other ->
                        lager:warning("Unexpected return from heartbeat_store:heart_rhythm(~p): ~p", [UserId, Other]),
                        throw({unexpected_store_response, Other})
                end
        end
    catch
        throw:{invalid_user_id, Reason} ->
            lager:error("Invalid user_id in checkup_handler: ~p", [Reason]),
            cowboy_req:reply(400,
                             #{<<"content-type">> => <<"application/json">>},
                             jsx:encode(#{error => <<"invalid user_id format">>}),
                             Req);
        throw:{unexpected_store_response, Response} ->
            lager:error("Unexpected response from store in checkup_handler: ~p", [Response]),
            cowboy_req:reply(500,
                             #{<<"content-type">> => <<"application/json">>},
                             jsx:encode(#{error => <<"internal server error">>}),
                             Req);
        Error:Reason:Stacktrace ->
            lager:error("Error in checkup_handler: ~p:~p~nStacktrace: ~p", [Error, Reason, Stacktrace]),
            cowboy_req:reply(500,
                             #{<<"content-type">> => <<"application/json">>},
                             jsx:encode(#{error => <<"internal server error">>}),
                             Req)
    end,
    {ok, Result, State}.
