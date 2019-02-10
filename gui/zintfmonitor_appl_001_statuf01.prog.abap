*----------------------------------------------------------------------*
***INCLUDE ZINTFMONITOR_APPL_001_STATUF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form STATUS_9001
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM status_9001 .

  SET PF-STATUS 'S001'.
  SET TITLEBAR 'T01'.


  IF go_appl IS NOT BOUND.
    go_appl = NEW zcl_intfmonitor_gui_appl( ).

    gs_filter_by-intfid       = s_id[].
    gs_filter_by-procdate     = s_date[].
    gs_filter_by-proctime     = s_time[].
    gs_filter_by-procby       = s_by[].
    gs_filter_by-procendtype  = s_type[].

    CREATE OBJECT go_container
      EXPORTING
        container_name = 'CONTAINER'
        repid          = sy-repid   " Screen to Which this Container is Linked
        dynnr          = sy-dynnr   " Report To Which this Container is Linked
      EXCEPTIONS
        OTHERS         = 6.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.


    go_appl->search( is_filters = gs_filter_by ).
    go_appl->display( go_container ).
  ENDIF.

ENDFORM.

FORM user_command_9001.
  LEAVE TO SCREEN 0.
ENDFORM.
