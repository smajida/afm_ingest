
-module(afm_ingest).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-export([start/0,init_afm_tables/0]).
-export([detections_since/1,detections_since/2,detections_since/3,last_updated/0,current_detections/0]).
-export([update_now/0]).
-export([subscribe/0,unsubscribe/0]).
-include("afm_detection.hrl").
-include_lib("stdlib/include/qlc.hrl").

-type satellite() :: viirs|avhrr|modis|goes.
-export_type([satellite/0]).

-spec start() -> ok.
start() ->
  ok = application:start(sasl),
  ok = application:start(inets),
  mnesia:create_schema([node()]),
  ok = application:start(mnesia),
  init_afm_tables(),
  ok = application:start(afm_ingest).

-spec init_afm_tables() -> ok.
init_afm_tables() ->
  ensure_table_exists(afm_detection,record_info(fields,afm_detection),[lat,lon]).

-spec last_updated() -> calendar:datetime().
last_updated() ->
  afm_ingest_server:last_updated().

-spec detections_since(calendar:datetime()) -> [#afm_detection{}] | {error, term()}.
detections_since(Since) ->
  case mnesia:transaction(
    fun() ->
        Q = qlc:q([X || X=#afm_detection{timestamp=T} <- mnesia:table(afm_detection), T > Since]),
        qlc:e(Q) end) of
    {atomic, R} ->
      R;
    Error ->
      Error
  end.

-spec current_detections() -> [#afm_detection{}].
current_detections() ->
  afm_ingest_server:current_detections().

-spec detections_since(calendar:datetime(),term()) -> [#afm_detection{}].
detections_since(Since,conus) ->
  detections_since(Since, [25.6,50], [-125.3,-65]).

-spec detections_since(calendar:datetime(), {number(),number()}, {number(),number()}) -> [#afm_detection{}].
detections_since(Since, {LatMin,LatMax}, {LonMin,LonMax}) ->
case mnesia:transaction(
    fun() ->
        Q = qlc:q([X || X=#afm_detection{timestamp=T,lat=Lat,lon=Lon} <- mnesia:table(afm_detection),
                    T > Since, Lat >= LatMin, Lat =< LatMax, Lon >= LonMin, Lon =< LonMax]),
        qlc:e(Q) end) of
    {atomic, R} ->
      R;
    Error ->
      Error
  end.

-spec update_now() -> ok.
update_now() ->
  afm_ingest_server:update_detections_now().

-spec subscribe() -> ok.
subscribe() ->
  afm_ingest_server:subscribe(self()).

-spec unsubscribe() -> ok.
unsubscribe() ->
  afm_ingest_server:unsubscribe(self()).

%-----------------------
% Internal functions
%-----------------------

-spec ensure_table_exists(atom(),[atom()], [atom()]) -> ok.
ensure_table_exists(Name,RecFields,NdxFields) ->
  case lists:member(Name,mnesia:system_info(tables)) of
    true ->
      ok;
    false ->
      {atomic,ok} = mnesia:create_table(Name, [{attributes,RecFields}, {disc_copies,[node()]}, {index,NdxFields}, {type,bag}]),
      ok
  end.

