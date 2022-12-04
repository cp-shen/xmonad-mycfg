--

module XMonad.MyCfg.Workspaces
  ( myWsList,
    ws1, ws2, ws3, ws4, ws5,
    ws6, ws7, ws8, ws9, ws10,
    wsCode,
    wsDownloads,
    wsEntertain,
    wsTerminal,
    wsWebpages,
  )
where

ws1 = "终端"
ws2 = "网页"
ws3 = "代码"
ws4 = "计时"
ws5 = "娱乐"
ws6 = "下载"
ws7 = "其他"
ws8 = "8"
ws9 = "9"
ws10 = "10"

wsTerminal = ws1
wsWebpages = ws2
wsCode = ws3
wsEntertain = ws5
wsDownloads = ws6

myWsList =
  [ ws1, ws2, ws3, ws4, ws5, ws6, ws7]
