

-module(afm_kml_retr).
-author("Martin Vejmelka <vejmelka@gmail.com>").
-export([detections_kml/1]).


detections_kml(modis) ->
  retrieve_and_unzip("http://activefiremaps.fs.fed.us/data/kml/conus.kmz", "conus.kml");

detections_kml(avhrr) ->
  retrieve_and_unzip("http://activefiremaps.fs.fed.us/data_avhrr/kml/conus.kmz", "conus.kml");

detections_kml(goes) ->
  retrieve_and_unzip("http://activefiremaps.fs.fed.us/data_goes/kml/conus.kmz", "conus.kml");

detections_kml(viirs) ->
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



