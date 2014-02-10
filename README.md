### afm\_ingest

An OTP-compliant Erlang application that periodically ingests satellite fire detections from (http://activefiremaps.fs.fed.us).
This is accomplished by parsing the KML files which provide fire detections for the last 24hrs of four systems:

  * GOES - Geostationary Operational Environmental Satellite
  * VIIRS - Visible Infrared Imaging Radiometer Suite
  * AVHRR - Advanced Very High Resolution Radiometer
  * MODIS - Moderate Resolution Imaging Spectroradiometer

The application starts a ````gen_server```` which periodically downloads the KML files corresponding to the selected satellites.

Note: **The activefiremaps website may change the KML format of these files at any time and in that case this library will stop working until it is updated to reflect the new format.**

## Retrieving detections

The ````afm_ingest```` module provides a complete API to retrieving the detections.  There are two ways to retrieve the detections.

# Push API

The function ````afm_ingest:subscribe()```` registeres the caller for messages ````{afm_new_detections, [#afm_detection{}]}````.
When the caller is no longer interested in updates, ````afm_ingest:unsubscribe()```` deregisteres the calling process from updates.

# Pull API

The function ````afm_ingest:detections_since(Since :: calendar:datetime())```` retrieves all detections that have a timestamp later than ````Since````.  The query area can be restricted to a rectangle by specifying minimum and maximum latitude and longitude using the function ````afm_ingest:detections_since(Since, {MinLat,MaxLat}, {MinLon,MaxLon})````.


## Each detection is stored as a record

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

Where ````satellite()```` is a type defined as follows:

    -type satellite() :: avhrr|viirs|goes|modis.

There are two types of detections, ````centroid```` which provides information only on the center of the fire detection and ````footprint```` which additionally contains a detection polygon stored in the ````det_poly```` field.

The sensor field identifies the exact sensor from which the fire detection originates, for example for the MODIS instrument, this may be Aqua or Terra.

