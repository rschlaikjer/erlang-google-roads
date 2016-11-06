# google_roads

## Erlang API client for the [google roads API](https://developers.google.com/maps/documentation/roads/snap)

Currently only imeplements the snap to roads api.

    ApiKey = <<"Your api key">>.
    {ok, Client} = google_roads:new_client(ApiKey).
    Points = google_roads:snap_points(Client, [#location{latitude=-35.27801, longitude=149.12958}]).
    Points.
    [#snapped_point{location = #location{latitude = -35.27800489993019,
                                         longitude = 149.129531998742},
                    original_index = 0,
                    place_id = <<"ChIJr_xl0GdNFmsRsUtUbW7qABM">>}]

