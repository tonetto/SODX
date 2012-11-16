-module(gui).
-export([start/4]).
-include_lib("wx/include/wx.hrl").
-define(WindowSize, {550, 600}).
-define(PanelSize, {250, 400}).
-define(OuterSizerMinWidth, 250).
-define(OuterSizerMaxHeight, 600). % maximum sizer size
-define(InSizerMinWidth, 200).
-define(InSizerMinHeight, 50).
-define(PropTitle, "Proposers").
-define(PropText1, "Round:").
-define(PropText2, "Last:").
-define(AccTitle, "Acceptors").
-define(AccText1, "Round Voted: {}").
-define(AccText2, "Cur. Promise: {}").

start(Acceptors, Proposers, AccPanelHeight, PropPanelHeight) ->
    State = make_window(Acceptors, Proposers, AccPanelHeight, PropPanelHeight),
    gui(State).

make_window(Acceptors, Proposers, AccPanelHeight, PropPanelHeight) ->
    Server = wx:new(),
    Env = wx:get_env(),
    Frame = wxFrame:new(Server, -1, "Paxos Algorithm", [{size,?WindowSize}]),
    wxFrame:connect(Frame, close_window),
    Panel = wxPanel:new(Frame),
                                                % create Sizers
    OuterSizer = wxBoxSizer:new(?wxVERTICAL),
    MainSizer = wxBoxSizer:new(?wxHORIZONTAL),
    ProposerSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel,
                                         [{label, "Proposers"}]),
    AcceptorSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel,
                                         [{label, "Acceptors"}]),
                                                % set Sizer’s min width/height
    case AccPanelHeight > ?OuterSizerMaxHeight of
        true ->
            OuterAccSizerHeight = ?OuterSizerMaxHeight;
        false ->
            OuterAccSizerHeight = AccPanelHeight
    end,
    case PropPanelHeight > ?OuterSizerMaxHeight of
        true ->
            OuterPropSizerHeight = ?OuterSizerMaxHeight;
        false ->
            OuterPropSizerHeight = PropPanelHeight
    end,
    wxSizer:setMinSize(AcceptorSizer, ?OuterSizerMinWidth, OuterAccSizerHeight),
    wxSizer:setMinSize(ProposerSizer, ?OuterSizerMinWidth, OuterPropSizerHeight),
                                                % create Acceptors and Proposers Panels
    AccIds = create_acceptors(Acceptors, [], Panel, AcceptorSizer, Env),
    PropIds = create_proposers(Proposers, [], Panel, ProposerSizer, Env),
                                                % add spacers
    wxSizer:addSpacer(MainSizer, 10), %spacer
    wxSizer:addSpacer(ProposerSizer, 20),
    wxSizer:addSpacer(AcceptorSizer, 20),
                                                % add ProposerSizer into MainSizer
    wxSizer:add(MainSizer, ProposerSizer,[]),
    wxSizer:addSpacer(MainSizer, 20),
                                                % add AcceptorSizer into MainSizer
    wxSizer:add(MainSizer, AcceptorSizer,[]),
    wxSizer:addSpacer(MainSizer, 20),
    wxSizer:addSpacer(OuterSizer, 20),

                                                % add MainSizer into OuterSizer
    wxSizer:add(OuterSizer, MainSizer, []),
    %% Now ’set’ OuterSizer into the Panel
    wxPanel:setSizer(Panel, OuterSizer),
    wxFrame:show(Frame),
    {Frame, AccIds, PropIds}.

gui(State) ->
    {Frame, AccIds, PropIds} = State,
    receive
        % request State
        {reqState, From} ->
            io:format("[Gui] State requested ~n"),
            From ! {reqState, {AccIds, PropIds}},
            gui(State);
        % a connection gets the close_window signal
        % and sends this message to the server
        #wx{event=#wxClose{}} ->
            io:format("~p Closing window ~n",[self()]), %optional, goes to shell
                                                % now we use the reference to Frame
            wxWindow:destroy(Frame),
            ok; % we exit the loop
        stop ->
            wxWindow:destroy(Frame),
            ok; % we exit the loop
        Msg ->
                                                %Everything else ends up here
            io:format("loop default triggered: Got ~n ~p ~n", [Msg]),
            gui(State)
    end.

% create acceptors
create_acceptors(AcceptorList, AcceptorIds, Panel, AcceptorSizer, Env) ->
    case AcceptorList of
        [] ->
            AcceptorIds;
        [AccName|Rest] ->
            Id = spawn(fun() -> initAcceptor(AccName, Panel, ?wxBLACK,
                                             AcceptorSizer, Env) end),
            create_acceptors(Rest, [Id|AcceptorIds], Panel, AcceptorSizer, Env)
    end.

% create proposers
create_proposers(ProposerList, ProposerIds, Panel, ProposerSizer, Env) ->
    case ProposerList of
        [] ->
            ProposerIds;
        [PropName|Rest] ->
            Id = spawn(fun() -> initProposer(PropName, Panel, ?wxBLACK,
                                             ProposerSizer, Env) end),
            create_proposers(Rest, [Id|ProposerIds], Panel, ProposerSizer, Env)
    end.

% initialize an acceptor
initAcceptor(AccTitle, InPanel, BgColour, AccSizer, AccEnv) ->
    wx:set_env(AccEnv),
    AcceptorSizerIn = wxStaticBoxSizer:new(?wxVERTICAL, InPanel,
                                           [{label, AccTitle}]),
    %set Sizer’s min width/height
    wxSizer:setMinSize(AcceptorSizerIn, ?InSizerMinWidth, ?InSizerMinHeight),
    AcceptorPanel = wxPanel:new(InPanel, [{size, ?PanelSize}]),
    {L1, L2} = setPanel(AcceptorPanel, BgColour, ?AccText1, ?AccText2),
    wxSizer:add(AcceptorSizerIn, AcceptorPanel, []),
    wxSizer:add(AccSizer, AcceptorSizerIn),
    wxWindow:fit(InPanel),
    wxWindow:fit(AcceptorPanel),
    acceptor(AcceptorPanel, AcceptorSizerIn, BgColour, L1, L2).

% acceptor loop waiting updates
acceptor(AccPanel, AccSizerIn, BgColour, L1, L2) ->
    receive
        % update panel
        {updateAcc, Round, Promise, Colour} ->
            updatePanel(AccPanel, L1, L2, Round, Promise, Colour),
            wxWindow:fit(AccPanel),
            acceptor(AccPanel, AccSizerIn, BgColour, L1, L2)
    end.

% initialize a proposer
initProposer(PropTitle, InPanel, BgColour, PropSizer, PropEnv) ->
    wx:set_env(PropEnv),
    ProposerSizerIn = wxStaticBoxSizer:new(?wxVERTICAL, InPanel,
                                           [{label, PropTitle}]),
    % set Sizer’s min width/height
    wxSizer:setMinSize(ProposerSizerIn, ?InSizerMinWidth, ?InSizerMinHeight),
    ProposerPanel = wxPanel:new(InPanel, [{size, ?PanelSize}]),
    {L1, L2} = setPanel(ProposerPanel, BgColour, ?PropText1, ?PropText2),
    wxSizer:add(ProposerSizerIn, ProposerPanel, []),
    wxSizer:add(PropSizer, ProposerSizerIn,[]),
    wxWindow:fit(InPanel),
    proposer(ProposerPanel, ProposerSizerIn, BgColour, L1, L2).

% proposer loop waiting for updates
proposer(PropPanel, PropSizerIn, BgColour, L1, L2) ->
    receive
        % update panel
        {updateProp, Round, Proposal, Colour} ->
            updatePanel(PropPanel, L1, L2, Round, Proposal, Colour),
            wxWindow:fit(PropPanel),
            proposer(PropPanel, PropSizerIn, BgColour, L1, L2)
    end.

% set a Panel
setPanel(InPanel, BgColour, Label1, Label2) ->
    wxPanel:setBackgroundColour(InPanel, BgColour),
    Round = wxStaticText:new(InPanel, 1, Label1,[{pos, {5, 5}}]),
    wxStaticText:setForegroundColour(Round, ?wxWHITE),
    Proposal = wxStaticText:new(InPanel, 1, Label2, [{pos, {5, 20}}]),
    wxStaticText:setForegroundColour(Proposal, ?wxWHITE),
    {Round, Proposal}.

updatePanel(PropPanel, Label1, Label2, Round, Proposal, Colour) ->
    wxPanel:setBackgroundColour(PropPanel, Colour),
    wxStaticText:setLabel(Label1, Round),
    wxStaticText:setLabel(Label2, Proposal),
    wxPanel:refresh(PropPanel).
