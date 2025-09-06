FROM erlang:27 AS BUILD

WORKDIR /buildroot

COPY src src/
COPY config config/
COPY include include/
COPY rebar.config .
RUN rebar3 as prod release

FROM alpine

COPY --from=BUILD /buildroot/_build/prod/rel/heartbeat /rel

EXPOSE 3519
CMD ["/rel/heartbeat/bin/heartbeat", "console"]