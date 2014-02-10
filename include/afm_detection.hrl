
-type satellite() :: viirs|avhrr|modis|goes.
-export_type([satellite/0]).

-record(afm_detection,
  {timestamp :: calendar:datetime(),
   lat :: number(),
   lon :: number(),
   satellite :: satellite(),
   type :: centroid | footprint,
   confidence :: number() | atom(),
   det_poly :: [{number(),number()}],
   sensor :: list(),
   recv_station :: list()}).

