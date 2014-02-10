
-record(afm_detection,
        {timestamp,
         lat,
         lon,
         satellite,
         type,
         confidence,
         det_poly,
         sensor,
         recv_station}).
