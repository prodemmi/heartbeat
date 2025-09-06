-module(checkup_handler).
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
                    {'EXIT', _} ->
                        throw({invalid_user_id, UserIdBin});
                    ValidUserId when is_integer(ValidUserId) ->
                        ValidUserId
                end,
                
                %% heartbeat_store:checkup/1 returns a #user{} record directly
                User = heartbeat_store:checkup(UserId),
                JSON = jsx:encode(#{id => User#user.id,
                                    status => atom_to_binary(User#user.status, utf8)}),
                cowboy_req:reply(200,
                                #{<<"content-type">> => <<"application/json">>},
                                JSON,
                                Req)
        end
    catch
        throw:{invalid_user_id, InvalidReason} ->
            lager:error("Invalid user_id in checkup_handler: ~p", [InvalidReason]),
            cowboy_req:reply(400,
                            #{<<"content-type">> => <<"application/json">>},
                            jsx:encode(#{error => <<"invalid user_id format">>}),
                            Req);
        ErrorType:ErrorReason:ErrorStacktrace ->
            lager:error("Error in checkup_handler: ~p:~p~nStacktrace: ~p", [ErrorType, ErrorReason, ErrorStacktrace]),
            cowboy_req:reply(500,
                            #{<<"content-type">> => <<"application/json">>},
                            jsx:encode(#{error => <<"internal server error">>}),
                            Req)
    end,
    {ok, Result, State}.