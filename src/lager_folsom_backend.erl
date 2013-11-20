%% lager_folsom_backend implements a folsom backend counter module for lager
-module(lager_folsom_backend).

-behaviour(gen_event).
-include_lib("lager/include/lager.hrl").

-export([init/1, handle_call/2, handle_event/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {level, id}).

init([Level]) ->
    initialize_metrics(),
    try parse_level(Level) of
      Lvl -> {ok, #state { level = Lvl, id = unique }}
    catch
      _:_ -> {error, bad_log_level}
    end.
    
handle_call(get_loglevel, #state { level = Level } = State) ->
    {ok, Level, State};
handle_call({set_loglevel, Level}, State) ->
    try parse_level(Level) of
      Lvl -> {ok, ok, State#state { level = Lvl }}
    catch
      _:_ -> {ok, {error, bad_log_level}, State}
    end;
handle_call(_Request, State) ->
    {ok, ok, State}.

handle_event({log, Level, {_Date, _Tme}, [_LevelStr, _Location, _Message]},
        #state { level = LogLevel} = State) when Level =< LogLevel ->
    ok = folsom_log(convert_level(Level)),
    {ok, State};
handle_event({log, Message}, #state { level = Level } = State) ->
    case lager_util:is_loggable(Message, Level, State#state.id) of
        true ->
          ok = folsom_log(convert_level(Level)),
          {ok, State};
        false ->
          {ok, State}
    end;
handle_event(_Event, State) ->
    {ok, State}.

handle_info(_Info, State) ->
	{ok, State}.
	
terminate(_Reason, _State) ->
	ok.
	
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% Internal stuff
%% ------------------------------------

initialize_metrics() ->
    [folsom_metrics:new_counter(C)
      || C <-
        ['lager.emergency',
         'lager.alert',
         'lager.critical',
         'lager.error',
         'lager.warning',
         'lager.notice',
         'lager.info',
         'lager.debug'] ],
    ok.
      
parse_level(Level) ->
	try lager_util:config_to_mask(level) of
	  Res -> Res
	catch
	  error:undef ->
	    lager_util:level_to_num(Level)
	end.

folsom_log(Level) ->
    folsom_metrics:notify({Level, {inc, 1}}).

convert_level(?DEBUG) -> 'lager.debug';
convert_level(?INFO) -> 'lager.info';
convert_level(?NOTICE) -> 'lager.notice';
convert_level(?WARNING) -> 'lager.warning';
convert_level(?ERROR) -> 'lager.error';
convert_level(?CRITICAL) -> 'lager.critical';
convert_level(?ALERT) -> 'lager.alert';
convert_level(?EMERGENCY) -> 'lager.emergency'.