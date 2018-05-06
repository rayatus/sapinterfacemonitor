*&---------------------------------------------------------------------*
*&  Include           ZINFMONITOR_GUI_APPL_SCR
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK a WITH FRAME TITLE text-t01.
  select-OPTIONS: s_intid  for ZINTFMONITOR010-INTFID,
                  s_PRCDt  for ZINTFMONITOR020-PROCDATE,
                  s_prctm  for ZINTFMONITOR020-PROCTIME,
                  s_PRCBY  FOR ZINTFMONITOR020-procby,
                  s_PRCTP  for ZINTFMONITOR020-PROCENDTYPE.
SELECTION-SCREEN end   of BLOCK a.
