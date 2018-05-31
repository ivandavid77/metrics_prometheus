-module(agent).
-export([init/2, terminate/3]).
-export([initialize_metrics/0, begin_collection/1, end_collection/2, end_collection_with_errors/2]).

initialize_metrics() ->
    prometheus_counter:new([
        {name, ms_failed_service_calls_total},
        {labels, [service]},
        {help, "Conteo de llamadas a servicio fallidas"}
    ]),
    % Usage:
    % prometheus_counter:inc(ms_failed_service_calls_total, [whatsapp]).
    % Related metrics:
    % ratio of exceptions = rate(ms_failed_service_calls_total[1m]) / rate(ms_service_request_milliseconds_count[1m])
    
    % Uso actual de los servicios
    prometheus_gauge:new([
        {name, ms_current_service_usage},
        {labels, [service]},
        {help, "Uso actual de lo servicios"}
    ]),
    % Usage:
    % prometheus_gauge:inc(ms_current_service_usage, [whatsapp])
    % prometheus_gauge:dec(ms_current_service_usage, [whatsapp])

    % Medir la latencia promedio (con Summary)
    prometheus_summary:new([
        {name, ms_service_request_milliseconds},
        {labels, [service]},
        {help, "Total de llamadas a servicio y latencia en milisegundos"}
    ]),
    % Latency:
    % rate(ms_service_request_milliseconds[1m]) / rate(ms_service_request_milliseconds[1m]) 

    % Medir la latencia promedio (con Histogram)
    % prometheus_histogram:new([
    %     {name, ms_service_request_duration_seconds}, 
    %     {labels, [service]},
    %     {buckets, [1, 5, 10, 15, 20]},
    %     {help, "Segundos entre la solicitud del servicio y su respuesta"}
    % ]),
     
    % Usage:
    % Service = whatsapp,
    % Start = erlang:timestamp(),
    % End = erlang:timestamp(),
    % Seconds = timer:now_diff(End, Start) * 0.000001,
    % prometheus_histogram:observe(ms_service_request_duration_seconds, [Service], Seconds)
    %
    % Latency:
    % rate(ms_service_request_duration_seconds_sum[1m]) / rate(ms_service_request_duration_seconds_count[1m]) 
    %
    % Metrics Example:
    % ms_service_request_duration_seconds_bucket{service="whatsapp",le="100"} 0
    % ms_service_request_duration_seconds_bucket{service="whatsapp",le="300"} 1
    % ms_service_request_duration_seconds_bucket{service="whatsapp",le="500"} 3
    % ms_service_request_duration_seconds_bucket{service="whatsapp",le="750"} 4
    % ms_service_request_duration_seconds_bucket{service="whatsapp",le="1000"} 5
    % ms_service_request_duration_seconds_bucket{service="whatsapp",le="+Inf"} 6
    % ms_service_request_duration_seconds_count{service="whatsapp"} 6
    % ms_service_request_duration_seconds_sum{service="whatsapp"} 4350
    % ms_service_request_duration_seconds_bucket{service="facebook",le="100"} 3
    % ms_service_request_duration_seconds_bucket{service="facebook",le="300"} 6
    % ms_service_request_duration_seconds_bucket{service="facebook",le="500"} 7
    % ms_service_request_duration_seconds_bucket{service="facebook",le="750"} 8
    % ms_service_request_duration_seconds_bucket{service="facebook",le="1000"} 9
    % ms_service_request_duration_seconds_bucket{service="facebook",le="+Inf"} 9
    % ms_service_request_duration_seconds_count{service="facebook"} 9
    % ms_service_request_duration_seconds_sum{method="facebook"} 2622
    ok.


begin_collection(Service) ->
    prometheus_gauge:inc(ms_current_service_usage, [Service]),
    erlang:timestamp().

end_collection(Service, BeginTime) ->
    EndTime = erlang:timestamp(),
    end_collection(Service, BeginTime, EndTime).

end_collection(Service, BeginTime, EndTime) ->
    prometheus_gauge:dec(ms_current_service_usage, [Service]),
    MicroSeconds = timer:now_diff(EndTime, BeginTime),
    %io:write(MicroSeconds),
    %prometheus_histogram:observe(ms_service_request_duration_seconds, [Service], Seconds),
    % Al parecer este plugin acepta el valor en microseconds pero lo convierte a milliseconds
    % esto es importnte para establecer la unidad de la metrica en grafana
    prometheus_summary:observe(ms_service_request_milliseconds, [Service], MicroSeconds),
    ok.

end_collection_with_errors(Service, BeginTime) ->
    EndTime = erlang:timestamp(),
    prometheus_counter:inc(ms_failed_service_calls_total, [Service]),
    end_collection(Service, BeginTime, EndTime).

init(Req, State) ->
    Response = cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/plain">>
    }, prometheus_text_format:format(), Req),
    {ok, Response, State}.

terminate(_Reason, _Req, _State) ->
    ok.
