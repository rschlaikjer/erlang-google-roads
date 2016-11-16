-module(google_roads_client).
-compile([{parse_transform, lager_transform}]).
-include_lib("google_roads/include/google_roads.hrl").

-define(GOOGLE_ROADS_DOMAIN, "roads.googleapis.com").
-define(GOOGLE_ROADS_PATH, "/v1/snapToRoads").

%% Actual gmaps API
-export([snap_points/3]).

%% Supervisor init method
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% API client state
-record(state, {
    api_key :: binary(),
    open_requests :: map(),
    gun ::any()
}).

-record(request, {
    from,
    callback,
    response_data
}).

%% Callbacks
start_link(ApiKey) when is_binary(ApiKey) ->
    gen_server:start_link(?MODULE, [ApiKey], []).

init([ApiKey]) ->
    {ok, Gun} = init_gun(),
    {ok, #state{
            api_key=ApiKey,
            gun=Gun,
            open_requests=maps:new()
    }}.

handle_call({snap_points, Points, Interpolate}, From, State) ->
    NewState = handle_snap_points(From, State, Points, Interpolate),
    {noreply, NewState};
handle_call(Request, From, State) ->
    lager:info("Call ~p From ~p", [Request, From]),
    {reply, ignored, State}.

handle_cast(Msg, State) ->
    lager:info("Cast ~p", [Msg]),
    {noreply, State}.

handle_info({gun_response, ConnPid, StreamRef, Fin, Status, Headers}, State) ->
    NewState = handle_gun_response(State, ConnPid, StreamRef, Fin, Status, Headers),
    {noreply, NewState};
handle_info({gun_data, ConnPid, StreamRef, Fin, Data}, State) ->
    NewState = handle_gun_data(State, ConnPid, StreamRef, Fin, Data),
    {noreply, NewState};
handle_info({gun_up, _, _}, State) ->
    {noreply, State};
handle_info({gun_down, _, _, _, _, _}, State) ->
    {noreply, State};
handle_info(Info, State) ->
    lager:info("Info ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(OldVsn, State, _Extra) ->
    lager:info("~p updated from vsn ~p", [?MODULE, OldVsn]),
    {ok, State}.

%% API
snap_points(Client, Points, Interpolate) ->
    gen_server:call(Client, {snap_points, Points, Interpolate}).

%% Implementation

init_gun() ->
    gun:open(?GOOGLE_ROADS_DOMAIN, 443, #{protocols => [http]}).

handle_gun_response(State, _ConnPid, StreamRef, fin, _Status, _Headers) ->
    case maps:find(StreamRef, State#state.open_requests) of
        error ->
            lager:error("Got Gun callback for unknown stream ref ~p~n", [StreamRef]),
            State;
        {ok, Request} ->
            lager:info("Stream ~p terminated, responding error~n", [StreamRef]),
            gen_server:reply(Request#request.from, error),
            State#state{
              open_requests=maps:remove(StreamRef, State#state.open_requests)
            }
    end;
handle_gun_response(State, _ConnPid, StreamRef, nofin, _Status, _Headers) ->
    case maps:find(StreamRef, State#state.open_requests) of
        error ->
            lager:error("Got Gun callback for unknown stream ref ~p~n", [StreamRef]),
            State;
        {ok, Request} ->
            Blank = <<"">>,
            State#state{
                open_requests=maps:put(
                    StreamRef,
                    Request#request{response_data=Blank},
                    State#state.open_requests
                )
            }
    end.

handle_gun_data(State, _ConnPid, StreamRef, fin, Data) ->
    case maps:find(StreamRef, State#state.open_requests) of
        error ->
            lager:error("Got Gun callback for unknown stream ref ~p~n", [StreamRef]),
            State;
        {ok, Request} ->
            CurData = Request#request.response_data,
            FinalData = <<CurData/binary, Data/binary>>,
            Callback = Request#request.callback,
            Response = Callback(FinalData),
            gen_server:reply(Request#request.from, Response),
            State#state{
              open_requests=maps:remove(StreamRef, State#state.open_requests)
            }
    end;
handle_gun_data(State, _ConnPid, StreamRef, nofin, Data) ->
    case maps:find(StreamRef, State#state.open_requests) of
        error ->
            lager:error("Got Gun callback for unknown stream ref ~p~n", [StreamRef]),
            error;
        {ok, Request} ->
            CurData = Request#request.response_data,
            NewData = <<CurData/binary, Data/binary>>,
            State#state{
                open_requests=maps:put(
                    StreamRef,
                    Request#request{response_data=NewData},
                    State#state.open_requests
                )
            }
    end.


handle_snap_points(From, State=#state{}, Points, Interpolate) ->
    Fragment = generate_fragment(State#state.api_key, Points, Interpolate),
    Headers = [
        {<<"accept">>, "application/json"}
    ],
    StreamRef = gun:get(State#state.gun, Fragment, Headers),
    Request = #request{from=From, callback=fun parse_snapped_points/1},
    NewMap = maps:put(StreamRef, Request, State#state.open_requests),
    State#state{open_requests=NewMap}.

parse_snapped_points(Data) ->
    try jsx:decode(Data) of
        Response ->
            case proplists:get_value(<<"snappedPoints">>, Response) of
                undefined ->
                    extract_api_error(Response);
                SnappedPoints ->
                    {ok, lists:map(
                        fun proplist_to_snapped_point/1,
                        SnappedPoints
                    )}
            end
    catch
        error:badarg ->
            {error, bad_json_response}
    end.

proplist_to_snapped_point(Proplist) ->
    LocationProplist = proplists:get_value(<<"location">>, Proplist),
    Location = #location{
        latitude = proplists:get_value(<<"latitude">>, LocationProplist),
        longitude = proplists:get_value(<<"longitude">>, LocationProplist)
    },
    OriginalIndex = proplists:get_value(<<"originalIndex">>, Proplist),
    PlaceId = proplists:get_value(<<"placeId">>, Proplist),
    #snapped_point{
        location=Location,
        original_index=OriginalIndex,
        place_id=PlaceId
    }.

extract_api_error(Response) ->
    case proplists:get_value(<<"error">>, Response) of
        undefined ->
            {error, unknown_error};
        ErrorData ->
            case proplists:get_value(<<"message">>, ErrorData) of
                undefined ->
                    {error, unknown_error};
                ErrorMessage ->
                    {error, ErrorMessage}
            end
    end.

generate_fragment(ApiKey, Points, Interpolate) ->
    PointStrings = [
        io_lib:format("~f,~f", [Lat, Lon])
        || #location{latitude=Lat, longitude=Lon} <- Points
    ],
    PointList = string:join(PointStrings, "|"),
    io_lib:format("~s?key=~s&interpolate=~s&path=~s", [
        ?GOOGLE_ROADS_PATH,
        ApiKey,
        Interpolate,
        PointList
    ]).
