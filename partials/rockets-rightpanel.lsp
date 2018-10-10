; partial file rockets-rightpanel.lsp
;
; displays the optional right hand column for the main page

; the actual code is shared for both left and right panels, we just have to specify which one
(setq panel-type RocketsConfig:RightPanel)
(display-partial "rockets-sidepanels")