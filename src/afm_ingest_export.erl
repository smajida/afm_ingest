-module(afm_ingest_export).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-include("afm_detection.hrl").
-export([to_geojson/1]).



to_geojson(L) ->
  lists:flatten([
    "[\n",
    string:join(lists:map(fun fd_to_geojson/1, L),",\n"),
    "]\n"]).


to_esmf({{Y,M,D},{H,Min,S}}) ->
  lists:flatten(io_lib:format("~4..0B~2..0B~2..0B_~2..0B~2..0B~2..0B", [Y,M,D,H,Min,S])).

-spec fd_to_geojson(#afm_detection{}) -> string().
fd_to_geojson(#afm_detection{timestamp=TS,lat=Lat,lon=Lon,confidence=C,satellite=S}) ->
  [ "{\n",
    "  \"type\": \"Feature\",\n",
    "  \"geometry\": {\n",
    "    \"type\": \"Point\",\n",
    "    \"coordinates\": [",number_to_list(Lon),",",number_to_list(Lat),"]\n",
    "  },\n",
    "  \"properties\" : {\n",
    "    \"timestamp_gmt\": \"", to_esmf(TS), "\",\n",
    "    \"confidence\": ",confidence_to_list(C),",\n",
    "    \"satellite\": ",satellite_to_list(S),"\n",
    "  }\n",
    "}\n"].

number_to_list(N) when is_integer(N) ->
  integer_to_list(N);
number_to_list(F) when is_float(F) ->
  io_lib:format("~p", [F]).

confidence_to_list(N) when is_integer(N) ->
  integer_to_list(N);
confidence_to_list(high) ->
  "\"high\"";
confidence_to_list(medium) ->
  "\"medium\"";
confidence_to_list(cloud) ->
  "\"cloud contamination\"";
confidence_to_list(undefined) ->
  "\"unknown\"";
confidence_to_list(S) when is_list(S) ->
  "\"unknown\"".

satellite_to_list(goes) ->
  "\"GOES\"";
satellite_to_list(avhrr) ->
  "\"AVHRR\"";
satellite_to_list(viirs) ->
  "\"VIIRS\"";
satellite_to_list(modis) ->
  "\"MODIS\"".


