# afm\_ingest

An OTP-compliant Erlang application that periodically ingests satellite fire detections from (http://activefiremaps.fs.fed.us).
This is accomplished by parsing the KML files which provide fire detections for the last 24hrs of up to four systems:

  * GOES - Geostationary Operational Environmental Satellite
  * VIIRS - Visible Infrared Imaging Radiometer Suite
  * AVHRR - Advanced Very High Resolution Radiometer
  * MODIS - Moderate Resolution Imaging Spectroradiometer

The type

    satellite() :: goes|viirs|avhrr|modis.

is used to indicate in which satellites/instruments we are interested.

The KMZ files on activefiremaps are available for four regions: CONUS (Contiguous US), Alaska, Hawaii and Canada.  Each of these is assigned an atom

    region() :: conus|alaska|hawaii|canada.

The queried satellites/instruments, regions as well as the query frequency can be modified in the ````afm_ingest.app```` file.  For example, the line

    {mod, {afm_ingest_app, [[{viirs,[canada]},{goes,[conus,alaska]},{avhrr,[conus]},{modis,[conus,hawaii]}],10]}},

in the ````.app```` file indicates the wish to ingest fire detections from all four systems every 10 minutes and VIIRS detections will be retrieved only for the Canada domain, GOES for Contiguous US and Alaska, AVHRR for Contiguous US and MODIS for Contiguous US and Hawaii.

The server responsible for retrieving and parsing the KMZ/KML files will not crash if the files cannot be retrieved or parsed but will send message to the ````error_logger```` event manager.

Note: **The activefiremaps website may change the KML format of these files at any time and in that case this library will stop working until it is updated to reflect the new format.  However, KML seems to be the only format in which the data for all four systems is provided.**

## Error handling

If the ````afm_ingest_server```` encounters any errors when retrieving and parsing the requested KMZ files, it will currently emit errors to the ````error_logger```` but will continue functioning.

All errors that are encountered during retrievel/parsing are stored and can be queried using

    afm_ingest:report_errors()

all errors have the format ````{error,satellite(),region(),atom(),term()}```` where the ````atom()```` is the exception class and ````term()```` is the exception content itself.
When the errors have been processed and acknowledged, they can be removed from the server using

   afm_ingest:clear_errors().


## Retrieving detections

The ````afm_ingest```` module provides a complete API to retrieving the detections.  There are two ways to retrieve detections.

NOTE: All ingested fire detections are written into the mnesia table ````afm_detection````, which can be queried at any time.

### Push API

The function ````afm_ingest:subscribe()```` registeres the caller for messages ````{afm_new_detections, [#afm_detection{}]}```` and ````{afm_errors, ErrorList}````.
See above the the format of the errors in the ````ErrorList````.
When the caller is no longer interested in updates, ````afm_ingest:unsubscribe()```` deregisteres the calling process from updates.

### Pull API

The function

    afm_ingest:detections_since(Since :: datetime())

retrieves all detections that have a datetime later than ````Since````.  The query area can be restricted to a rectangle by specifying minimum and maximum latitude and longitude using the function

    afm_ingest:detections_since(Since :: datetime(), {MinLat,MaxLat}, {MinLon,MaxLon}).

### Out of band refresh

Invocation of 

    afm_ingest:update_now()

will force the system to immediatly retrieve and process KMZ files according to application configuration.

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

There are two types of detections

  * ````centroid```` which provides information only on the center of the fire detection and
  * ````footprint```` which additionally contains a detection polygon stored in the ````det_poly```` field.

The sensor field identifies the exact sensor from which the fire detection originates, for example for the MODIS instrument, this field indicates the particular spacecraft (Aqua or Terra).

