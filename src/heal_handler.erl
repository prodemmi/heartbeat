-module(heal_handler).

-behaviour(cowboy_handler).

-export([init/2]).

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
                    {'EXIT', _} ->
                        throw({invalid_user_id, UserIdBin});
                    ValidUserId when is_integer(ValidUserId) ->
                        ValidUserId
                end,

                case heartbeat_store:heal(UserId) of
                    {atomic, ok} ->
                        JSON = jsx:encode(#{status => <<"online">>}),
                        cowboy_req:reply(200, 
                                        #{<<"content-type">> => <<"application/json">>}, 
                                        JSON, 
                                        Req);
                    {aborted, table_not_exists} ->
                        lager:error("Database table does not exist for user ~p", [UserId]),
                        cowboy_req:reply(500,
                                        #{<<"content-type">> => <<"application/json">>},
                                        jsx:encode(#{error => <<"database not initialized">>}),
                                        Req);
                    {aborted, AbortReason} ->
                        lager:error("Transaction aborted in heal_handler for user ~p: ~p", [UserId, AbortReason]),
                        throw({transaction_aborted, AbortReason});
                    Other ->
                        lager:error("Unexpected return from heartbeat_store:heal(~p): ~p", [UserId, Other]),
                        throw({unexpected_store_response, Other})
                end
        end
    catch
        throw:{invalid_user_id, InvalidReason} ->
            lager:error("Invalid user_id in heal_handler: ~p", [InvalidReason]),
            cowboy_req:reply(400,
                            #{<<"content-type">> => <<"application/json">>},
                            jsx:encode(#{error => <<"invalid user_id format">>}),
                            Req);
        throw:{transaction_aborted, TxnReason} ->
            lager:error("Transaction aborted in heal_handler: ~p", [TxnReason]),
            cowboy_req:reply(500,
                            #{<<"content-type">> => <<"application/json">>},
                            jsx:encode(#{error => <<"database transaction failed">>}),
                            Req);
        throw:{unexpected_store_response, Response} ->
            lager:error("Unexpected response from store in heal_handler: ~p", [Response]),
            cowboy_req:reply(500,
                            #{<<"content-type">> => <<"application/json">>},
                            jsx:encode(#{error => <<"internal server error">>}),
                            Req);
        ErrorType:ErrorReason:ErrorStacktrace ->
            lager:error("Error in heal_handler: ~p:~p~nStacktrace: ~p", [ErrorType, ErrorReason, ErrorStacktrace]),
            cowboy_req:reply(500,
                            #{<<"content-type">> => <<"application/json">>},
                            jsx:encode(#{error => <<"internal server error">>}),
                            Req)
    end,
    {ok, Result, State}.