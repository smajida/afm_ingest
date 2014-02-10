

-module(afm_ingest_kml).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-export([parse_kml/1,retrieve_kml/1,retrieve_detections/1]).

-include_lib("eunit/include/eunit.hrl").
-include("afm_detection.hrl").

-spec retrieve_detections(afm_ingest:satellite()) -> [#afm_detection{}].
retrieve_detections(Sat) ->
  {ok,Bin} = retrieve_kml(Sat),
  FDs = parse_kml(Bin),
  lists:map(fun (C) -> C#afm_detection{satellite=Sat} end, FDs).


% Parses the KML extracted from the kmz file on the activefiremaps website.
-spec parse_kml(binary()) -> [#afm_detection{}].
parse_kml(Binary) when is_binary(Binary) ->
  {ok, {wait_for_placemark,FDs},_} = erlsom:parse_sax(Binary, {wait_for_placemark,[]}, fun extract_centroids/2),
  FDs.


% SAX callback function to parse all placemark elements that are centroids or footprints.

extract_centroids({startElement,_,"Placemark",_,_}, {wait_for_placemark,Acc}) ->
  {[],in_placemark,Acc};
extract_centroids({startElement,_,"description",_,_}, {C,in_placemark,Lst}) ->
  {C,in_description,Lst};
extract_centroids({startElement,_,"coordinates",_,_}, {C,in_placemark,Lst}) ->
  {C,in_coordinates,Lst};
extract_centroids({startElement,_,"name",_,_}, {C,in_placemark,Lst}) ->
  {C,in_name,Lst};

extract_centroids({characters,Chars}, {C,in_description,Lst}) ->
  {[{description, Chars}|C],in_placemark,Lst};
extract_centroids({characters,Chars}, {C,in_coordinates,Lst}) ->
  {[{coordinates, Chars}|C],in_placemark,Lst};
extract_centroids({characters,Chars}, {C,in_name,Lst}) ->
  {[{name,Chars}|C],in_placemark,Lst};

extract_centroids({endElement,_,"Placemark",_}, {C,in_placemark,Lst}) ->
  case process_placemark(C) of
    error ->
      % forget the placemark since it does not have the expected form
      {wait_for_placemark,Lst};
    Centroid ->
      % add the placemark, since it was parsed succesfully
      {wait_for_placemark,[Centroid|Lst]}
  end;
extract_centroids(_Ev,Acc) ->
  Acc.


process_placemark(Data) ->
  N = string:strip(proplists:get_value(name,Data)),
  D = proplists:get_value(description,Data),
  C = proplists:get_value(coordinates,Data),

  case N of
    "Fire Detection Footprint" ->
      C1 = update_with_description(#afm_detection{type=footprint},D),
      update_with_coords(C1,C);
    "Fire Detection Centroid" ->
      C1 = update_with_description(#afm_detection{type=centroid},D),
      C1#afm_detection{det_poly=[]};
    _ ->
      error
  end.


extract_confidence(undefined) ->
  undefined;
extract_confidence("Cloud-contaminated fire pixel") ->
  cloud;
extract_confidence([$H,$i,$g,$h|_]) ->
  high;
extract_confidence([$M,$e,$d,$i,$u,$m|_]) ->
  medium;
extract_confidence([$L,$o,$w|_]) ->
  low;
extract_confidence(Text) ->
  case string:to_float(Text) of
    {error, no_float} ->
      case string:to_integer(Text) of
        {error, no_integer} ->
          Text;
        {I,[]} ->
          I
      end;
    {F,[]} ->
      F
  end.


update_with_description(C,Chars) ->
  Toks = lists:map(fun(X) -> re:replace(X,"</?b>","",[global,{return,list}]) end, re:split(Chars, "<br/>")),
  Plist = lists:map(fun (X) -> {Key,[$:|Val]} = lists:splitwith(fun ($:) -> false; (_) -> true end, X),
                                                {string:strip(Key), string:strip(Val)} end, Toks),
  Lat = list_to_number(proplists:get_value("Latitude", Plist)),
  Lon = list_to_number(proplists:get_value("Longitude", Plist)),
  Date = parse_date(proplists:get_value("Detection Date", Plist)),
  Time = parse_time(proplists:get_value("Detection Time", Plist)),
  Sensor = proplists:get_value("Sensor", Plist),
  Recv = proplists:get_value("Receiving Station", Plist),
  Confidence = extract_confidence(proplists:get_value("Confidence", Plist)),
  C#afm_detection{confidence=Confidence,timestamp={Date,Time},lat=Lat,lon=Lon,sensor=Sensor,recv_station=Recv}.


update_with_coords(C,Chars) ->
  Coords3d = lists:map(fun list_to_number/1, string:tokens(Chars,"\r\n \t,")),
  C#afm_detection{det_poly=ground_coords(Coords3d,[])}.

ground_coords([],C) ->
  C;
ground_coords([Lon,Lat,_|Rest], C) ->
  ground_coords(Rest, [{Lon,Lat}|C]).

list_to_number(undefined) ->
  undefined;
list_to_number(L) ->
  case string:to_float(L) of
    {error, no_float} ->
      list_to_integer(L);
    {F,[]} ->
      F
  end.


decode_month("Jan") -> 1;
decode_month("Feb") -> 2;
decode_month("Mar") -> 3;
decode_month("Apr") -> 4;
decode_month("May") -> 5;
decode_month("Jun") -> 6;
decode_month("Jul") -> 7;
decode_month("Aug") -> 8;
decode_month("Sep") -> 9;
decode_month("Oct") -> 10;
decode_month("Nov") -> 11;
decode_month("Dec") -> 12.


parse_date([D1,D2,$ ,M1,M2,M3,$ |YearStr]) ->
  Day = list_to_integer([D1,D2]),
  Mon = decode_month([M1,M2,M3]),
  Year = list_to_integer(YearStr),
  {Year,Mon,Day}.

parse_time([H1,H2,$:,M1,M2|_]) ->
  Hr = list_to_integer([H1,H2]),
  Min = list_to_integer([M1,M2]),
  {Hr,Min,0}.


-spec retrieve_kml(afm_ingest:satellite()) -> {ok,binary()} | {error,any()}.
retrieve_kml(modis) ->
  retrieve_and_unzip("http://activefiremaps.fs.fed.us/data/kml/conus.kmz", "conus.kml");

retrieve_kml(avhrr) ->
  retrieve_and_unzip("http://activefiremaps.fs.fed.us/data_avhrr/kml/conus.kmz", "conus.kml");

retrieve_kml(goes) ->
  retrieve_and_unzip("http://activefiremaps.fs.fed.us/data_goes/kml/conus.kmz", "conus.kml");

retrieve_kml(viirs) ->
  retrieve_and_unzip("http://activefiremaps.fs.fed.us/data_viirs/kml/conus.kmz", "conus.kml").


retrieve_and_unzip(Url, File) ->
  case httpc:request(get, {Url, []}, [], [{body_format,binary}]) of
    {ok, {{_, 200, _}, _, Bdy}} ->
      {ok, Files} = zip:unzip(Bdy, [memory]),
      case proplists:get_value(File, Files) of
        undefined ->
          {error, file_not_found};
        Bin ->
          {ok, Bin}
      end;
    {ok, {{_, Other, _}, _, _}} ->
      {error, Other};
    {error, Reason} ->
      {error, Reason}
  end.


