

-module(afm_ingest_utils).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-export([find_new_detections/1]).
-include("afm_detection.hrl").


find_new_detections(FDs) ->
  {atomic, New} = mnesia:transaction(
    fun() ->
      lists:filter(fun (#afm_detection{locator=L}) ->
            case mnesia:read(afm_detection,L) of
              [] ->
                true;
              _ ->
                false
            end
        end, FDs) end,
    [], 1),
  New.



