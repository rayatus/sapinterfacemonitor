*&---------------------------------------------------------------------*
*& Report zintfmonitor_appl_001
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zintfmonitor_appl_001.

*======================================================================
TABLES:
*======================================================================
  zintfmonitor010,
  zintfmonitor020.

*======================================================================
DATA:
*======================================================================
  gs_filter_by TYPE zcl_intfmonitor_gui_appl=>mtyp_s_filter_by,
  go_appl      TYPE REF TO zcl_intfmonitor_gui_appl,
  go_container TYPE REF TO cl_gui_custom_container.

SELECTION-SCREEN BEGIN OF BLOCK a WITH FRAME.
SELECT-OPTIONS: s_id      FOR zintfmonitor010-intfid,
                s_date    FOR zintfmonitor020-procdate,
                s_time    FOR zintfmonitor020-proctime,
                s_by      FOR zintfmonitor020-procby,
                s_type    FOR zintfmonitor020-procendtype.
SELECTION-SCREEN END   OF BLOCK a.

*======================================================================
START-OF-SELECTION.
*======================================================================

  CALL SCREEN 9001.

  INCLUDE zintfmonitor_appl_001_statuo01.
  INCLUDE zintfmonitor_appl_001_statuf01.
  INCLUDE zintfmonitor_appl_001_user_i01.
