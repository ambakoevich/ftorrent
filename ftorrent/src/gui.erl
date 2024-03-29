%% @author: Ionut Trancioveanu, Zarif Jawad, Paulius Vysniauskas.
%% @copyright Framework Studio
%% @version: v0.1
%% @doc Created: 1-Oct-2011, Creating GUI of software.

-module(gui).
-export([start/1,new_window/0,loop/5]).
-import(gui_info,[about/2,help/2,error_message/2,error_message2/2]).
-import(gui_util,[create_list_file/2, create_list_ctrl/2,check/1,file_image/2,limit_filename/3]).
-include_lib("wx/include/wx.hrl").

%% @doc Creating the start function.
start(Manager)->
    State = new_window(),
    {_,_,CurrentTime} = erlang:now(),
    loop(State,Manager,1,0,CurrentTime).

%% @doc Creating a new window/frame and a panel. 

new_window()->
    Server = wx:new(),
    Frame  = wxFrame:new(Server,-1,"F-torrent",[{pos,{300,150}},{size,{650,500}}]),
    Icon = wxIcon:new("bg.ico",[{type,?wxBITMAP_TYPE_ICO}]),
    wxFrame:setIcon(Frame,Icon),
    Panel  = wxPanel:new(Frame),
    wxPanel:setBackgroundColour(Panel,{204,204,204}),

%% @doc Creating the Widgets which will be showed on the frame/panel. 
    
    BitmapStart  = wxBitmap:new("Start.png",    [{type,?wxBITMAP_TYPE_PNG}]),
    BitmapCancel = wxBitmap:new("Cancel.png",   [{type,?wxBITMAP_TYPE_PNG}]),
    BitmapOpen   = wxBitmap:new("Openfile.png", [{type,?wxBITMAP_TYPE_PNG}]),
    Logo         = wxBitmap:new("Logo.png",     [{type,?wxBITMAP_TYPE_PNG}]),
    BitmapAbout  = wxBitmap:new("about.png",    [{type,?wxBITMAP_TYPE_PNG}]),
    BitmapHelp   = wxBitmap:new("help.png",     [{type,?wxBITMAP_TYPE_PNG}]),
    DownloadText = wxStaticText:new  (Panel, 21,"Download status",[]),
    StaticBitmap = wxStaticBitmap:new(Panel,  14,       Logo),
    DownldStatus = wxStaticText:new  (Panel, 71,   " N/A",[]),
    FileName     = wxStaticText:new  (Panel, 31,"Name: ", []),
    NameNA       = wxStaticText:new  (Panel, 41,"    N/A",[]),
    FileSize     = wxStaticText:new  (Panel, 51,"Size: ", []),
    FileSizeNA   = wxStaticText:new  (Panel, 61,"    N/A",[]),
    ButtonOpen   = wxBitmapButton:new(Panel,  1 , BitmapOpen),
    ButtonStart  = wxBitmapButton:new(Panel,  2 ,BitmapStart),
    ButtonCancel = wxBitmapButton:new(Panel,  3,BitmapCancel),
    ButtonAbout  = wxBitmapButton:new(Panel,  4 ,BitmapAbout),
    ButtonHelp   = wxBitmapButton:new(Panel,  5 , BitmapHelp),

%% @doc Creating listeners for the buttons.
    Range = 100,
    Value = 0,
    wxBitmapButton:connect(ButtonOpen,  command_button_clicked),
    wxBitmapButton:connect(ButtonStart, command_button_clicked),
    wxBitmapButton:connect(ButtonCancel,command_button_clicked),
    wxBitmapButton:setToolTip(ButtonOpen,  "Add torrent file "),
    wxBitmapButton:setToolTip(ButtonStart,     "Start torrent"),
    wxBitmapButton:setToolTip(ButtonCancel,   "Cancel torrent"),
    Gauge = wxGauge:new(Panel,1,Range,[{size,{260,-1}},{style,?wxGA_HORIZONTAL}]),
    wxGauge:setValue(Gauge, Value),
    Font1 = wxFont:new(15, ?wxMODERN, ?wxNORMAL, ?wxBOLD),
    Font2 = wxFont:new(10, ?wxNORMAL, ?wxNORMAL, ?wxBOLD),
    wxStaticText:setFont(DownloadText,Font2),
    wxStaticText:setFont(FileName,Font1),
    wxStaticText:setFont(FileSize,Font1),
   
%% @doc Creating Notebook in which the Info about torrent file will be shown.

    Notebook = wxNotebook:new(Panel, 1, [{style, ?wxBK_DEFAULT}]),
    %% wxImageList is for displaying icons in the tab field
    IL = wxImageList:new(16,16),
    wxImageList:add(IL, wxArtProvider:getBitmap("wxART_TICK_MARK",       [{size, {16,16}}])),
    wxImageList:add(IL, wxArtProvider:getBitmap("wxART_INFORMATION",     [{size, {16,16}}])),
    wxImageList:add(IL, wxArtProvider:getBitmap("wxART_LIST_VIEW",       [{size, {16,16}}])),
    wxImageList:add(IL, wxArtProvider:getBitmap("wxART_FILE_OPEN",       [{size, {16,16}}])),
    wxImageList:add(IL, wxArtProvider:getBitmap("wxART_EXECUTABLE_FILE", [{size, {16,16}}])),
    wxNotebook:assignImageList(Notebook, IL),
    %% First tab field ("General").
    Win1 = wxListCtrl:new(Notebook, [{style, ?wxLC_REPORT}]),
    wxListCtrl:insertColumn(Win1, 0, "File",
	      [{format, ?wxLIST_FORMAT_LEFT}, {width, 400}]),
    wxListCtrl:insertColumn(Win1, 1, "Size",
	      [{format, ?wxLIST_FORMAT_LEFT}, {width, 100}]),
    wxListCtrl:insertColumn(Win1, 2, "Pieces",
	      [{format, ?wxLIST_FORMAT_LEFT}, {width, 105}]),
    wxListCtrl:setBackgroundColour(Win1, {128,128,128}),
    wxListCtrl:setTextColour(Win1, ?wxWHITE),
    wxNotebook:addPage(Notebook, Win1, "General", []),
    wxNotebook:setPageImage(Notebook, 0, 1),
    %% Second tab field ("Tracker").
    Win2 = wxListCtrl:new(Notebook, [{style, ?wxLC_REPORT}]),
    wxListCtrl:insertColumn(Win2, 0, "Tracker list",
	      [{format, ?wxLIST_FORMAT_LEFT}, {width, 605}]),
    wxListCtrl:setBackgroundColour(Win2, {128,128,128}),
    wxListCtrl:setTextColour(Win2, ?wxWHITE),
    wxNotebook:addPage(Notebook, Win2, "Tracker", []),
    wxNotebook:setPageImage(Notebook, 1,4),
    %% Third tab field ("Peers").
    Win3 = wxListCtrl:new(Notebook, [{style, ?wxLC_REPORT}]),
    wxListCtrl:insertColumn(Win3, 0, "IP List",
	      [{format, ?wxLIST_FORMAT_LEFT}, {width, 605}]),
    wxNotebook:addPage(Notebook, Win3, "Peer List", []),
    wxListCtrl:setBackgroundColour(Win3, {128,128,128}),
    wxListCtrl:setTextColour(Win3, ?wxWHITE),
    wxNotebook:setPageImage(Notebook, 2,2),
    %% Fourth tab field ("File").
    Win4 = wxListCtrl:new(Notebook, [{style,?wxLC_REPORT}]),
    wxListCtrl:insertColumn(Win4, 0, "Path",
	      [{format, ?wxLIST_FORMAT_LEFT}, {width, 400}]),
    wxListCtrl:insertColumn(Win4, 1, "Size",
	      [{format, ?wxLIST_FORMAT_LEFT}, {width, 100}]),
    wxListCtrl:insertColumn(Win4, 2, "Pieces",
	      [{format, ?wxLIST_FORMAT_LEFT}, {width, 105}]),
    wxListCtrl:setBackgroundColour(Win4, {128,128,128}),
    wxListCtrl:setTextColour(Win4, ?wxWHITE),
    wxNotebook:addPage(Notebook, Win4, "File", []),
    wxNotebook:setPageImage(Notebook, 3, 3),

%% @doc Create Sizers.
    
    MainSizer     = wxBoxSizer:new(?wxHORIZONTAL),
    InputSizer    = wxBoxSizer:new(?wxHORIZONTAL),
    InputSizer1   = wxBoxSizer:new  (?wxVERTICAL),
    TextSizer     = wxBoxSizer:new  (?wxVERTICAL),
    TextSizer1    = wxBoxSizer:new(?wxHORIZONTAL),
    ButtonSizer   = wxBoxSizer:new  (?wxVERTICAL),
    ButtonSizer1  = wxBoxSizer:new(?wxHORIZONTAL),
    SpaceSizer    = wxBoxSizer:new(?wxHORIZONTAL),
    OuterSizer    = wxBoxSizer:new  (?wxVERTICAL),
    
%% @doc Adding the Widgets, using the Variable, to Sizers and Spacers.
     
    wxSizer:add(MainSizer, InputSizer,   []),
    wxSizer:add(ButtonSizer1,ButtonSizer,[]),
    wxSizer:add(TextSizer1,TextSizer,    []),
    wxSizer:add(InputSizer,SpaceSizer ,  []),
    wxSizer:add(InputSizer,ButtonSizer1 ,[]),
    wxSizer:add(InputSizer,TextSizer1,   []),
    wxSizer:add(InputSizer,InputSizer1,  []),
    wxSizer:addSpacer(SpaceSizer ,  40),
    wxSizer:addSpacer(ButtonSizer1, 50),
    wxSizer:addSpacer(ButtonSizer,  40),
    wxSizer:add(ButtonSizer,ButtonOpen , []),
    wxSizer:addSpacer(ButtonSizer,   5),
    wxSizer:addSpacer(ButtonSizer,  30),
    wxSizer:add(ButtonSizer,ButtonStart, []),
    wxSizer:addSpacer(ButtonSizer,   5),
    wxSizer:add(ButtonSizer,ButtonCancel,[]),
    wxSizer:addSpacer(ButtonSizer,  40),
    wxSizer:addSpacer(InputSizer , -50),
    wxSizer:add(InputSizer,ButtonAbout , []),
    wxSizer:addSpacer(InputSizer ,   5),
    wxSizer:add(InputSizer,ButtonHelp  , []),
    wxSizer:addSpacer(TextSizer1 ,  30),
    wxSizer:addSpacer(TextSizer  ,  40),
    wxSizer:add(TextSizer, FileName    , []),
    wxSizer:addSpacer(TextSizer  ,  20),
    wxSizer:add(TextSizer, NameNA      , []),
    wxSizer:addSpacer(TextSizer  ,  20),
    wxSizer:add(TextSizer, FileSize    , []),
    wxSizer:addSpacer(TextSizer  ,  20),
    wxSizer:add(TextSizer, FileSizeNA  , []),
    wxSizer:addSpacer(TextSizer  ,  40),
    wxSizer:add(TextSizer, DownloadText, []),
    wxSizer:addSpacer(TextSizer  ,  10),
    wxSizer:add(TextSizer, DownldStatus, []),
    wxSizer:addSpacer(TextSizer  ,  40),
    wxSizer:addSpacer(InputSizer ,  30),
    wxSizer:addSpacer(InputSizer1,  40),
    wxSizer:add(InputSizer1,StaticBitmap,[]),
    wxSizer:addSpacer(InputSizer1,  20),
    wxSizer:add(InputSizer1,Gauge      , []),
    wxSizer:addSpacer(OuterSizer ,  10),
    wxSizer:addSpacer(MainSizer  ,  40),
    wxSizer:add(OuterSizer,MainSizer   , []),
    wxSizer:add(OuterSizer,Notebook,[{proportion, 1}, {flag, ?wxEXPAND}]),
    wxNotebook:connect(Notebook, command_notebook_page_changed, [{skip, true}]),

%% @doc Setting the OuterSizer into the Panel and show the Frame.
    
    wxPanel:setSizer(Panel,OuterSizer),
    wxFrame:createStatusBar(Frame),
    wxFrame:setStatusText(Frame, "                Download:  N/A  kB/s"),
    wxFrame:fit(Frame),
    wxFrame:show(Frame),

%% @doc Create the listeners.

     wxFrame:connect(Frame, close_window),
     wxPanel:connect(Panel, command_button_clicked),

%% @doc Returned value from the State. 

    {Frame, NameNA, FileSizeNA, DownldStatus,  Win1, Win2, Win3,Win4, StaticBitmap,Gauge}.

%% @doc Create a loop which receives messages and respond to them.

loop(State,Manager,Piece_total,Status,PreviousTime)->
    {Frame, NameNA, FileSizeNA, DownldStatus,  Win1, Win2, Win3,Win4, StaticBitmap,Gauge}= State,
    
    receive      %% Receiving the close message which is sent to server.
	%% Exit the loop,Close the window.
	#wx{event=#wxClose{}} ->   
	    Manager ! {stop},
	    wxFrame:destroy(Frame),  
            ok;  
	%% File Dialog Open File Button.
        #wx{id= 1, event=#wxCommand{type=command_button_clicked}} ->
	    case Status of      
		0 ->
		    FD=wxFileDialog:new(Frame,[{message,"   Select torrent file to open  "}]),
		    case wxFileDialog:showModal(FD) of
			?wxID_OK ->      %% Open is clicked show the Dialog.
			    Filename = wxFileDialog:getFilename(FD),

			    Fpath = wxFileDialog:getDirectory(FD),
			    %%io:format("DIRrtt...~p~n", [Fpath]),
			    Fpath_converted = filter:replace_values(Fpath, [$\\], $/),

			    Manager ! {start_manager, Filename, self(), Fpath_converted},
			    %%self() ! {change_icon},
			    %%file_image(Panel, StaticBitmap, FileName),
			    wxFileDialog:destroy(FD);
			_ ->             %% Cancel is clicked close Dialog.
			    wxFileDialog:destroy(FD),
			    cancel
	    end,
		    io:format("First case status 0~n"),
		    loop(State, Manager,Piece_total,1,PreviousTime);
		1 ->
		    io:format("First case status 1~n"),
		    error_message2(1,  Frame),
		    loop(State, Manager,Piece_total,Status,PreviousTime);
		2 ->
		    io:format("First case status 2~n"),
		    error_message2(1,  Frame),
		    loop(State, Manager,Piece_total,Status,PreviousTime)
		    end;
	%% Start Button
	#wx{id= 2, event=#wxCommand{type=command_button_clicked}} -> 
	    case Status of    
		1 ->
		    io:format("Case for start ,Process not yet started~n"),
		    Manager ! {connect, self()},   
		    loop(State, Manager,Piece_total,2,PreviousTime);
		2 ->
		     io:format("Case for  start ,Process already started~n"),
		     loop(State, Manager,Piece_total,Status,PreviousTime);
		0 ->
		     io:format("Case for start ,Process already started~n"),
		     error_message(2,  Frame),
		     loop(State, Manager,Piece_total,Status,PreviousTime)
	    end;
	%% Cancel Button
	#wx{id= 3, event=#wxCommand{type=command_button_clicked}} ->
	    %% wxListControl:clearAll(Win1),
	    io:format("Manger trying to be stopped"),
	    exit(Manager,"watvere"),
	    Managernew = spawn(manager, start, []),
	    loop(State, Managernew,0,0,PreviousTime);  
	%% Event when button About is clicked.
	#wx{id= 4, event=#wxCommand{type=command_button_clicked}} ->
	    about(4, Frame),  
	    loop(State, Manager,Piece_total,Status,PreviousTime);
	%% Event when button Help is clicked.
	#wx{id= 5, event=#wxCommand{type=command_button_clicked}} ->
	    help(5, Frame),   
	    loop(State, Manager,Piece_total,Status,PreviousTime);
	%% Torrent information.
	{table, Torrent_info} ->	    
	    wxStaticText:setLabel(NameNA, limit_filename(db:read("FileName"),[],20)),
	    wxStaticText:setLabel(FileSizeNA, integer_to_list(db:read("length") div 1048576) ++ " MB"), 
	    create_list_file(Win1, Win4),
	    create_list_ctrl(Win2,[db:read("announce")]),
	    file_image(StaticBitmap, db:read("FileName")),
	    loop(State, Manager,Piece_total,Status,PreviousTime);
	{peer_list, Peer_list} ->
	    create_list_ctrl(Win3, Peer_list),
	    loop(State, Manager,Piece_total,Status,PreviousTime);

 %% @doc Receiving messages from piece manager after every peice 
 %% is downloaded and sets the gauge accordingly
	{piece_downloaded} ->
	        {_,_,CurrentTime} = erlang:now(),
	     Time_diff = ((CurrentTime - PreviousTime) / 100000),
	     Speed = abs(round((db:read("pieceSize") / 1024) / Time_diff)),
	     wxFrame:setStatusText(Frame, "                Download:  " ++ integer_to_list(Speed) ++ "  kB/s"),
	    Value =  round((Piece_total / db:read("NoOfPieces")) * 100),
	    case Value > 99 of 
		true  ->  wxGauge:setValue(Gauge,100),
	    wxStaticText:setLabel(DownldStatus, integer_to_list(100) ++ "%");
	        false ->
	    wxGauge:setValue(Gauge,Value),
	    wxStaticText:setLabel(DownldStatus, integer_to_list(Value) ++ "%")
			 end,
	    loop(State, Manager,Piece_total + 1,Status,PreviousTime)
    end.


