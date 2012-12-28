-module(gui).
-export([start/2]).
-define(width, 200).
-define(height, 200).
-include_lib("wx/include/wx.hrl").

start(Id, Master) ->
    spawn_link(fun() -> init(Id, Master) end).

init(Id, Master) ->
    Frame = make_frame(Id),
    loop(Frame, Master).

make_frame(Id) ->
                                                %Id is the window title
    Server = wx:new(), %Server will be the parent for the Frame
    Frame = wxFrame:new(Server, -1, Id, [{size,{?width, ?height}}]),
    wxFrame:setBackgroundColour(Frame, ?wxBLACK),
    wxFrame:show(Frame),
                                                %monitor closing window event
    wxFrame:connect(Frame, close_window),
    Frame.

loop(Frame, Master)->
    receive
                                                %check if the window was closed by the user
        #wx{event=#wxClose{}} ->
            wxWindow:destroy(Frame),
            Master ! stop,
            ok;
        {color, Color} ->
            color(Frame, Color),
            loop(Frame, Master);
        stop ->
            ok;
        Error ->
            io:format("gui: strange message ~w ~n", [Error]),
            loop(Frame, Master)
    end.

color(Frame, Color) ->
    wxFrame:setBackgroundColour(Frame, Color),
    wxFrame:refresh(Frame).
