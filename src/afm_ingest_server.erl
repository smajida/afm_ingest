-module(afm_ingest_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-include("afm_detection.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/2]).
-export([update_detections_now/0,last_updated/0,current_detections/0]).
-export([subscribe/1,unsubscribe/1]).
-export([report_errors/0,clear_errors/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------


-type retr_error() :: [{error,satellite(),region(),calendar:datetime(),any(),any()}].
-export_type([retr_error/0]).

-spec start_link([satellite()],pos_integer()) -> {ok,pid()} | ignore | {error,any()}.
start_link(IngestList,TimeoutMin) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [IngestList,[],TimeoutMin,unknown,gb_sets:new(),[]], []).

-spec update_detections_now() -> ok.
update_detections_now() ->
  ?SERVER ! update_detections_now.

-spec subscribe(pid()) -> ok.
subscribe(Pid) ->
  gen_server:call(?SERVER,{subscribe,Pid}).

-spec unsubscribe(pid()) -> ok.
unsubscribe(Pid) ->
  gen_server:call(?SERVER,{unsubscribe,Pid}).

-spec last_updated() -> calendar:datetime().
last_updated() ->
  gen_server:call(?SERVER,last_updated).

-spec current_detections() -> [#afm_detection{}].
current_detections() ->
  gen_server:call(?SERVER,current_detections).

-spec report_errors() -> [retr_error()].
report_errors() ->
  gen_server:call(?SERVER,report_errors).

-spec clear_errors() -> ok.
clear_errors() ->
  gen_server:call(?SERVER,clear_errors).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
  % kick off the ingest of satellite detections
  ?SERVER ! update_detections_timeout,
  {ok, Args}.

handle_call(Request, _From, State=[IngestList,Monitors,TimeoutMins,LastUpdate,LastFDs,Errors]) ->
  case Request of
    {subscribe,Pid} ->
      case lists:member(Pid,Monitors) of
        true ->
          error_logger:info_msg("afm_ingest_server: pid ~p was already subscribed for updates.~n", [Pid]),
          {reply,ok,State};
        false ->
          error_logger:info_msg("afm_ingest_server: subscribed pid ~p for updates.~n", [Pid]),
          {reply,ok,[IngestList,[Pid|Monitors],TimeoutMins,LastUpdate,LastFDs,Errors]}
      end;
    {unsubscribe,Pid} ->
      error_logger:info_msg("afm_ingest_server: unsubscribed pid ~p for updates.~n", [Pid]),
      {reply,ok,[IngestList,lists:delete(Pid,Monitors),TimeoutMins,LastUpdate,LastFDs,Errors]};
    last_updated ->
      {reply, LastUpdate, State};
    report_errors ->
      {reply, Errors, State};
    clear_errors ->
      {reply, ok, [IngestList,Monitors,TimeoutMins,LastUpdate,LastFDs,[]]};
    current_detections ->
      {reply, gb_sets:to_list(LastFDs), State};
    _ ->
      {reply, invalid_request, State}
  end.

handle_cast(_Msg, State) ->
  {noreply,State}.

handle_info(update_detections_timeout, S=[IngestList,Monitors,TimeoutMins,_LastUpdate,LastFDs,_Errors]) ->
  timer:send_after(TimeoutMins * 60 * 1000, update_detections_timeout),
  update_detections_async(IngestList,Monitors,LastFDs),
  {noreply, S};
handle_info(update_detections_now, S=[IngestList,Monitors,_TimeoutMins,_LastUpdate,LastFDs,_Errors]) ->
  update_detections_async(IngestList,Monitors,LastFDs),
  {noreply, S};
handle_info({detection_results,NewErrors,FDSet}, [IngestList,Monitors,TimeoutMins,_LastUpdate,_LastFDs,Errors]) ->
    error_logger:info_msg("afm_ingest_server: ~p new detection results have arrived with ~p errors.", [gb_sets:size(FDSet),length(NewErrors)]),
    {noreply, [IngestList,Monitors,TimeoutMins,calendar:local_time(),FDSet,NewErrors ++ Errors]};
handle_info(_Info,State) ->
  {noreply,State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-spec update_detections_async([term()],[pid()],gb_set()) -> ok.
update_detections_async(IngestList,Monitors,LastFDs) ->
  spawn(fun() ->
    {NewErrors,FDSet} = update_detections_int(IngestList,Monitors,LastFDs),
    ?SERVER ! {detection_results, NewErrors,FDSet} end),
  ok.


-spec update_detections_int([{satellite(),[region()]}],[pid()],gb_set()) -> {[retr_error()],gb_set()}.
update_detections_int(IngestList,Monitors,FDSet) ->
  Reports = lists:map(fun safe_retrieve_detections/1, IngestList),
  {ErrorsL, FDss} = lists:unzip(Reports),
  FDs = lists:flatten(FDss),
  Errors = lists:flatten(ErrorsL),
  New = case gb_sets:is_empty(FDSet) of
    true ->
      % no previous result set in memory, must use dbase to identify new results
      find_detections_not_in_table(FDs);
    false ->
      % we already have a previous result set in memory, simply compare to current
      % results
      lists:filter(fun (FD) -> not gb_sets:is_member(FD,FDSet) end, FDs)
  end,
  case {New,Errors} of
    {[],[]} ->
      error_logger:info_msg("afm_ingest_server: no new detections found."),
      ok;
    _ ->
      error_logger:info_msg("afm_ingest_server: sending new detections to ~p monitors.", [length(Monitors)]),
      lists:map(fun (X) -> X ! {afm_new_detections,New,Errors} end, Monitors)
  end,
  mnesia:transaction(fun() -> lists:foreach(fun mnesia:write/1, FDs) end, [], 3),
  {Errors,gb_sets:from_list(FDs)}.


-spec safe_retrieve_detections({satellite(),region()}) -> {[retr_error()],[#afm_detection{}]}.
safe_retrieve_detections({Sat,Regions}) ->
  Reports = lists:map(fun (R) ->
      try
        D = afm_ingest_kml:retrieve_detections(Sat,R),
        error_logger:info_msg("afm_ingest_server: acquired ~p satellite detections for sat ~p and regions ~w.~n", [length(D),Sat,Regions]),
        {ok, D}
      catch Cls:Exc ->
        error_logger:error_msg("afm_ingest_server:retrieve_detections(Sat=~p, Region=~p) encountered exception ~p:~p,~nreturning empty list.~n",
                                [Sat,R,Cls,Exc]),
        {{error,Sat,R,calendar:local_time(),Cls,Exc},[]}
      end
  end, Regions),
  {Infos,FDs} = lists:unzip(Reports),
  Errors = lists:filter(fun (ok) -> false; (_) -> true end, Infos),
  {Errors,lists:flatten(FDs)}.


-spec find_detections_not_in_table([#afm_detection{}]) -> [#afm_detection{}].
find_detections_not_in_table(FDs) ->
  {atomic, New} = mnesia:transaction(
    fun() ->
      lists:filter(fun (FD=#afm_detection{timestamp=T}) ->
            not lists:member(FD, mnesia:read(afm_detection,T)) end, FDs) end),
  New.

