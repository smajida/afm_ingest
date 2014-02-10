-module(afm_ingest_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/2,update_detections_now/0,last_updated/0]).
-export([subscribe/1,unsubscribe/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(SatList,TimeoutMin) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [SatList,[],TimeoutMin,unknown], []).

update_detections_now() ->
  gen_server:call(?SERVER,update_detections_now).

subscribe(Pid) ->
  gen_server:call(?SERVER,{subscribe,Pid}).

unsubscribe(Pid) ->
  gen_server:call(?SERVER,{unsubscribe,Pid}).

last_updated() ->
  gen_server:call(?SERVER,last_updated).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
  % kick off the ingest of satellite detections
  ?SERVER ! update_detections_timeout,
  {ok, Args}.

handle_call(Request, _From, State=[Sats,Monitors,TimeoutMins,LastUpdate]) ->
  case Request of
    {subscribe,Pid} ->
      case lists:member(Pid,Monitors) of
        true ->
          {reply,ok,State};
        false ->
          {reply,ok,[Sats,[Pid|Monitors],TimeoutMins,LastUpdate]}
      end;
    {unsubscribe,Pid} ->
      {reply,ok,[Sats,lists:delete(Pid,Monitors),TimeoutMins,LastUpdate]};
    last_updated ->
      {reply, LastUpdate, State};
    update_detections_now ->
      update_detections_int(Sats,Monitors),
      {reply, ok, [Sats,Monitors,TimeoutMins,calendar:local_time()]};
    _ ->
      {reply, invalid_request, State}
  end.

handle_cast(_Msg, State) ->
  {noreply,State}.

handle_info(update_detections_timeout, [Sats,Monitors,TimeoutMins,_LastUpdate]) ->
  update_detections_int(Sats,Monitors),
  timer:send_after(TimeoutMins * 60 * 1000, update_detections_timeout),
  {noreply, [Sats,Monitors,TimeoutMins,calendar:local_time()]};
handle_info(_Info,State) ->
  {noreply,State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------


update_detections_int(Sats,Monitors) ->
  case retrieve_new_detections_int(Sats) of
    [] ->
      ok;
    NewDets ->
      % auto-update all monitors with the newest detections (push interface)
      lists:map(fun (X) -> X ! {afm_new_detections,NewDets} end, Monitors)
  end.


retrieve_new_detections_int(Sats) ->
  FDs = lists:flatten(lists:map(fun afm_ingest_kml:retrieve_detections/1, Sats)),
  New = afm_ingest_utils:find_new_detections(FDs),
  mnesia:transaction(fun() -> lists:foreach(fun mnesia:write/1, FDs) end, [], 3),
  New.

