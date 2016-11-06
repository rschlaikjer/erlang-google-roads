-module(google_roads).

-export([
    new_client/1,
    snap_points/2, snap_points/3
]).

new_client(ApiKey) ->
    supervisor:start_child(google_roads_sup, [ApiKey]).

snap_points(Client, Points) ->
    snap_points(Client, Points, false).
snap_points(Client, Points, Interpolate) ->
    google_roads_client:snap_points(Client, Points, Interpolate).
