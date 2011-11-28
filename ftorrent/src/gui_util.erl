%%GUI UTIL
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
