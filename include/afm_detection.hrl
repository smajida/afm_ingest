
-record(afm_detection,
        {locator,    % {timestamp,lat,lon}
         satellite,
         type,
         confidence,
         det_poly,
         sensor,
         recv_station}).
