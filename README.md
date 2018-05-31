mon_service
====

POC for monitoring

Build
-----

    $ rebar3 compile


POC of using Prometheus library to expose metrics using Cowboy Server, by default it will choose port 8000 to serve the /metrics directory, if you need to generate test data, firtst use rebar2 shell command, then on Erl shell type:

1> ejemplo_metricas:test().

Currently using Counter, Gauge and Summary but not Histogram, I think that I do something wrong or I don't fully understad completely the output of such component (Histogram), so I left apart from this POC, the metrics are defined on agent.erl, this same file is used also by mon_service_sup.erl when wakeup the Cowboy (and related services), this is because agent.erl is the module for attending HTTP request callbacks from Cowboy.

