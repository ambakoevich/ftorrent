-module(gui_util).
-author("Ionut Trancioveanu").
-compile(export_all).
-include_lib("wx/include/wx.hrl").



create_list_ctrl(ListCtrl, List) ->
    Fun =
	fun(H) ->
		wxListCtrl:insertItem(ListCtrl, 0, H)
	end,
    lists:reverse(List),
    lists:map(Fun,List),
    ListCtrl.

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
file_image(StaticBitmap, FileName) ->
    Type = check(FileName),
    Image = wxBitmap:new("image.jpg",   [{type,?wxBITMAP_TYPE_JPEG}]),
    Audio = wxBitmap:new("audio.jpg",   [{type,?wxBITMAP_TYPE_JPEG}]),
    Video = wxBitmap:new("video.jpg",   [{type,?wxBITMAP_TYPE_JPEG}]),
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

limit_filename([H|T],Acc,Limit) when Limit > 0->
    NewAcc = Acc ++ [H],
    limit_filename(T,NewAcc, Limit - 1);
limit_filename(A,Acc,0) ->
    Acc.
