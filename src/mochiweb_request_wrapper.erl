-module(mochiweb_request_wrapper).
-compile(export_all).

get(Req, Field) ->
    Req:get(Field).

ok(Req, {ContentType, Body}) ->
    Req:ok({ContentType, Body}).
