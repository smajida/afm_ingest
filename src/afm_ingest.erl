

-module(afm_ingest).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-export([start/0,init_afm_tables/0]).
-export([detections_since/1,detections_since/2,detections_since/3,last_updated/0]).
-export([update_now/0]).
-export([subscribe/0,unsubscribe/0]).
-include("afm_detection.hrl").
-include_lib("stdlib/include/qlc.hrl").


start() ->
  ok = application:start(sasl),
  ok = application:start(inets),
  mnesia:create_schema([node()]),
  ok = application:start(mnesia),
  init_afm_tables(),
  ok = application:start(afm_ingest).


init_afm_tables() ->
  ensure_table_exists(afm_detection,record_info(fields,afm_detection),[]).


last_updated() ->
  afm_ingest_server:last_updated().

detections_since(Since) ->
  case mnesia:transaction(
    fun() ->
        Q = qlc:q([X || X=#afm_detection{locator={T,_,_}} <- mnesia:table(afm_detection), T > Since]),
        qlc:e(Q) end) of
    {atomic, R} ->
      R;
    Error ->
      Error
  end.


detections_since(Since,conus) ->
  detections_since(Since, [25.6,50], [-125.3,-65]).


detections_since(Since, [LatMin,LatMax], [LonMin,LonMax]) ->
case mnesia:transaction(
    fun() ->
        Q = qlc:q([X || X=#afm_detection{locator={T,Lat,Lon}} <- mnesia:table(afm_detection),
                    T > Since, Lat >= LatMin, Lat =< LatMax, Lon >= LonMin, Lon =< LonMax]),
        qlc:e(Q) end) of
    {atomic, R} ->
      R;
    Error ->
      Error
  end.


update_now() ->
  afm_ingest_server:update_detections_now().


subscribe() ->
  afm_ingest_server:subscribe(self()).

unsubscribe() ->
  afm_ingest_server:unsubscribe(self()).

%-----------------------
% Internal functions
%-----------------------


ensure_table_exists(Name,RecFields,NdxFields) ->
  case lists:member(Name,mnesia:system_info(tables)) of
    true ->
      ok;
    false ->
      {atomic,ok} = mnesia:create_table(Name, [{attributes,RecFields}, {disc_copies,[node()]}, {index,NdxFields}])
  end.

