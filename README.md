### afm\_ingest

An OTP-compliant Erlang application that periodically ingests satellite fire detections from (http://activefiremaps.fs.fed.us).
This is accomplished by parsing the KML files which provide fire detections for the last 24hrs of up to four systems:

  * GOES - Geostationary Operational Environmental Satellite
  * VIIRS - Visible Infrared Imaging Radiometer Suite
  * AVHRR - Advanced Very High Resolution Radiometer
  * MODIS - Moderate Resolution Imaging Spectroradiometer

The application starts a ````gen_server```` which periodically downloads the KML files corresponding to the selected satellites.

The queried instruments as well as the query frequency can be modified in the ````afm_ingest.app```` file.  For example, the line

    {mod, {afm_ingest_app, [[viirs,goes,avhrr,modis],10]}},

in the ````.app```` file indicates the wish to ingest fire detections from all four instruments every 10 minutes.


Note: **The activefiremaps website may change the KML format of these files at any time and in that case this library will stop working until it is updated to reflect the new format.**

## Retrieving detections

The ````afm_ingest```` module provides a complete API to retrieving the detections.  There are two ways to retrieve detections.
One can also select detection records from the mnesia ````afm_detection```` table, into which all ingested fire detections are written.

# Push API

The function ````afm_ingest:subscribe()```` registeres the caller for messages ````{afm_new_detections, [#afm_detection{}]}````.
When the caller is no longer interested in updates, ````afm_ingest:unsubscribe()```` deregisteres the calling process from updates.

# Pull API

The function

    afm_ingest:detections_since(Since :: datetime())
    
retrieves all detections that have a datetime later than ````Since````.  The query area can be restricted to a rectangle by specifying minimum and maximum latitude and longitude using the function

    afm_ingest:detections_since(Since :: datetime(), {MinLat,MaxLat}, {MinLon,MaxLon}).


## The fire detection record

     -record(afm_detection,
      {timestamp :: calendar:datetime(),
       lat :: number(),
       lon :: number(),
       satellite :: avhrr|viirs|goes|modis,
       type :: centroid | footprint,
       confidence :: number() | atom(),
       det_poly :: [{number(),number()}],
       sensor :: list(),
       recv_station :: list()}).

There are two types of detections, ````centroid```` which provides information only on the center of the fire detection and ````footprint```` which additionally contains a detection polygon stored in the ````det_poly```` field.

The sensor field identifies the exact sensor from which the fire detection originates, for example for the MODIS instrument, this field indicates the particular spacecraft (Aqua or Terra).

