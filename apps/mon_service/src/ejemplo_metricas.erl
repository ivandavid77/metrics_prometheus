-module(ejemplo_metricas).
-export([loop/1, test/0]).
-record(conf, {
    total_iterations,
    sleep_between_iterations,
    services,
    sample_size,
    max_delay_between_samples
}).

invoke_normal(Service, Delay) ->
    BeginTime = agent:begin_collection(Service),
    timer:sleep(Delay),
    agent:end_collection(Service, BeginTime),
    ok.

invoke_failed(Service, Delay) ->
    BeginTime = agent:begin_collection(Service),
    timer:sleep(Delay),
    agent:end_collection_with_errors(Service, BeginTime),
    ok.

invoke(Threshold) when (Threshold >= 1) and (Threshold =< 10)->
    Test = rand:uniform(10),
    if
        Test > Threshold -> fun (S, D) -> invoke_failed(S, D) end;
        true -> fun(S, D) -> invoke_normal(S, D) end
    end.

services(Services) ->
    Len = length(Services),
    Index = rand:uniform(Len),
    lists:nth(Index, Services).

generate_processes(0, _, _) ->
    ok;
generate_processes(SampleSize, Services, DelayLimit) ->
    Service = services(Services),
    Delay = rand:uniform(DelayLimit),
    Fun = invoke(8),
    spawn(fun () -> Fun(Service, Delay) end),
    generate_processes(SampleSize - 1, Services, DelayLimit).

loop(#conf{total_iterations = 0}) ->
    ok;
loop(#conf{total_iterations = TotalIter} = State) ->
    generate_processes(State#conf.sample_size, State#conf.services, State#conf.max_delay_between_samples),
    timer:sleep(State#conf.sleep_between_iterations),
    loop(State#conf{total_iterations = TotalIter - 1}).

test() ->
    State = #conf{
        total_iterations = 10000,
        sleep_between_iterations = 1000,
        services = [whatsapp, facebook, gmail],
        sample_size = 10,
        max_delay_between_samples = 8000
    },
    loop(State).

