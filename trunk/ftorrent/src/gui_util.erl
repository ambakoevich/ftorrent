%% @author Ionut Trancioveanu , Paulius Vysniauskas
%% @version v0.1
%% @doc Created: 15-Oct-2011, aditional functions for gui.

-module(gui_util).
-export([create_list_ctrl/2,create_list_file/2,
	 extend_file_list/3,check/1,file_image/2,limit_filename/3]).
-include_lib("wx/include/wx.hrl").

%% @doc Create a list for Peers list,Tracker tab,in Notebook.
create_list_ctrl(ListCtrl, List) ->
    Fun =
	fun(H) ->
		wxListCtrl:insertItem(ListCtrl, 0, H)
	end,
    lists:reverse(List),
    lists:map(Fun,List),
    ListCtrl.

%% @doc Create a list for General,File tab,inNotebook.
create_list_file(General_tab, File_tab) ->
    Fun =
	fun({{Length, [Name1|Name2]}, Row}) ->
		wxListCtrl:insertItem(File_tab, Row, ""),
		case Name2 of
		    [] -> 
			wxListCtrl:setItem(File_tab, Row, 0, Name1);
		    _ -> wxListCtrl:setItem(File_tab, Row, 0, Name2)
		end,
		%%wxListCtrl:setItem(File_tab, Row, 0, Name),
		wxListCtrl:setItem(File_tab, Row, 1, integer_to_list(Length div 1048576) ++ " MB"),
		wxListCtrl:setItem(File_tab, Row, 2, integer_to_list(Length div db:read("pieceSize") + 1))
	end,
    Path = db:read("path"),
    case Path of
	na -> 
	    wxListCtrl:insertItem(General_tab, 0, ""),
	    wxListCtrl:setItem(General_tab, 0, 0, db:read("FileName")),
	    wxListCtrl:setItem(General_tab, 0, 1, integer_to_list(db:read("length") div 1048576)),
	    wxListCtrl:setItem(General_tab, 0, 2, integer_to_list(db:read("NoOfPieces")));
	List -> 
	    wxListCtrl:insertItem(General_tab, 0, ""),
	    wxListCtrl:setItem(General_tab, 0, 0, db:read("FileName")),
	    
	    io:format("PATH ? ~p~n", [Path]),
	    NewPath = lists:reverse(extend_file_list(Path, 0, [])),
	    io:format("NEW PATH ~p~n", [NewPath]),

	    lists:map(Fun, NewPath)
    end,
    ok.

%% @doc Function which extends the multifile list.
extend_file_list([], N, Acc) ->
    Acc;

extend_file_list([H|T], N, Acc) ->
    List = [{H, N}|Acc],
    %%{H, N} ++ [Acc],
    extend_file_list(T, N + 1, List).

%% @doc Checking the extension of the file for identification.
check(File) ->
       case filename:extension(File) of
	".mp3" ->
	    mp3;
	".png" ->
	    png;
	".avi" ->
	    avi;
	_ ->
	    other
		end.

%% @doc Creating Images.
file_image(StaticBitmap, FileName) ->
    Type = check(FileName),
    Image = wxBitmap:new("image.png",   [{type,?wxBITMAP_TYPE_PNG}]),
    Audio = wxBitmap:new("audio.png",   [{type,?wxBITMAP_TYPE_PNG}]),
    Video = wxBitmap:new("video.png",   [{type,?wxBITMAP_TYPE_PNG}]),
    case Type of 
	mp3  ->
	    wxStaticBitmap:setBitmap(StaticBitmap, Audio);
        avi  ->
	    wxStaticBitmap:setBitmap(StaticBitmap, Video);
	png  ->
	    wxStaticBitmap:setBitmap(StaticBitmap, Image);
         _   ->
	    ok   
 end.

%% @doc Limit for the Name of the file function.
limit_filename([], Acc, _) ->
    lists:reverse(Acc);
limit_filename(_, Acc, 0) ->
    lists:reverse(Acc);
limit_filename([H|T], Acc, Limit) when Limit > 0->
    %% NewAcc = Acc ++ [H],    
    limit_filename(T, [H|Acc], Limit - 1).

