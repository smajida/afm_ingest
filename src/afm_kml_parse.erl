

-module(afm_kml_parse).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-export([detections/1,test_viirs/0,test_modis/0,test_sat/1]).

-include("afm_detection.hrl").

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
      {wait_for_placemark,Lst};
    Centroid ->
      {wait_for_placemark,[Centroid|Lst]}
  end;
extract_centroids(_Ev,Acc) ->
  Acc.


% Parses the KML extracted from the kmz file on the activefiremaps website.
detections(Binary) when is_binary(Binary) ->
  {ok, {wait_for_placemark,FDs},_} = erlsom:parse_sax(Binary, {wait_for_placemark,[]}, fun extract_centroids/2),
  FDs.




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
  C#afm_detection{confidence=Confidence,lat=Lat,lon=Lon,timestamp={Date,Time},sensor=Sensor,recv_station=Recv}.


update_with_coords(C,Chars) ->
  Toks = lists:map(fun list_to_number/1, string:tokens(Chars,"\r\n \t,")),
  C#afm_detection{det_poly=ground_coords(Toks,[])}.

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

convert_num(TensChar,UnitChar) ->
  (TensChar - $0) * 10 + (UnitChar - $0).

parse_date([D1,D2,$ ,M1,M2,M3,$ |YearStr]) ->
  Day = convert_num(D1,D2),
  Mon = decode_month([M1,M2,M3]),
  Year = list_to_integer(YearStr),
  {Day,Mon,Year}.

parse_time([H1,H2,$:,M1,M2|_]) ->
  Hr = convert_num(H1,H2),
  Min = convert_num(M1,M2),
  {Hr,Min,0}.

% testing code using kml files in examples folder

test_sat(Type) ->
  {ok,B} = file:read_file(lists:flatten(["examples/",Type,"/conus.kml"])),
  D = detections(B),
  io:format("~p~n", [D]).

test_viirs() ->
  test_sat("viirs").


test_modis() ->
  test_sat("modis").



