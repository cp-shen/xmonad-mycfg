--

module XMonad.MyCfg.Workspaces
  ( myWsList,
    ws1, ws2, ws3, ws4, ws5,
    ws6, ws7, ws8, ws9, ws10,
    wsCode,
    wsClock,
    wsDownloads,
    vsVideo,
    wsTerminal,
    wsWebpages,
  )
where

ws1 = "代码"
ws2 = "终端"
ws3 = "网页"
ws4 = "影音"
ws5 = "下载"
ws6 = "其他"
ws7 = "7"
ws8 = "8"
ws9 = "9"
ws10 = "10"

wsCode = ws1
wsTerminal = ws2
wsClock = ws2
wsWebpages = ws3
vsVideo = ws4
wsDownloads = ws5

myWsList =
  [ws1, ws2, ws3, ws4, ws5, ws6]
