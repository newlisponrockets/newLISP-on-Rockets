; partial file rockets-leftpanel.lsp
;
; displays the optional left hand column for the main page

; the actual code is shared for both left and right panels, we just have to specify which one
(setq panel-type RocketsConfig:LeftPanel)
(display-partial "rockets-sidepanels")

