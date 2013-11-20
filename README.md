# lager_folsom

Lager backend which will count the events happening in folsom. Can be used to get metric overviews of the amount of logging happening in a subsystem so one can correlate data between subsystems. It is also effective at figuring out if certain subsystems are the source of errors in a large code base.

The code is written for Issuu and is in use inside a couple of production systems. It is used to track error rates for the subsystems.

# Use

The module `lager_folsom_backend` is a new lager handler backend. It requires Lager 2.0 and there will probably not be too much backporting done for 1.0. You use the system by injecting a handler:

	{lager_folsom_backend, [debug]}, ⋯
	
in your handler configuration. This will let lager log any event at the `debug` level into folsom. You can then use tools to export folsom counters and obtain nice graphs of what is going on inside the system.



