"! <p class="shorttext synchronized" lang="en">SAP Interface Monitor Application</p>
CLASS zcl_intfmonitor_gui_appl DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.


    TYPES:
      "! <p class="shorttext synchronized" lang="en">Filter by</p>
      BEGIN OF mtyp_s_filter_by,
        intfid      TYPE RANGE OF zintfmonitor010-intfid,
        procdate    TYPE RANGE OF zintfmonitor020-procdate,
        proctime    TYPE RANGE OF zintfmonitor020-proctime,
        procby      TYPE RANGE OF zintfmonitor020-procby,
        procendtype TYPE RANGE OF zintfmonitor020-procendtype,
      END   OF mtyp_s_filter_by .

    "! <p class="shorttext synchronized" lang="en">Displays Monitor application on screen</p>
    "!
    "! @parameter io_container | <p class="shorttext synchronized" lang="en">Abstract Container for GUI Controls</p>
    METHODS display
      IMPORTING
        !io_container TYPE REF TO cl_gui_container .
    "! <p class="shorttext synchronized" lang="en">Searches for interface executions</p>
    "!
    "! @parameter is_filters | <p class="shorttext synchronized" lang="en">Filter by</p>
    METHODS search
      IMPORTING
        !is_filters TYPE mtyp_s_filter_by
      RAISING
        zcx_intfmonitor .
  PROTECTED SECTION.

  PRIVATE SECTION.


    "! <p class="shorttext synchronized" lang="en">Filter by</p>
    DATA ms_filter_by TYPE mtyp_s_filter_by .
    "! <p class="shorttext synchronized" lang="en">Shows a list with info about interface executions</p>
    DATA mo_list TYPE REF TO zcl_intfmonitor_gui_list .
    "! <p class="shorttext synchronized" lang="en">Splitter Control</p>
    DATA mo_splitter_vertical TYPE REF TO cl_gui_splitter_container .
    "! <p class="shorttext synchronized" lang="en">Splitter Control</p>
    DATA mo_splitter_horizontal TYPE REF TO cl_gui_splitter_container .
    "! <p class="shorttext synchronized" lang="en">Main control for all application</p>
    DATA mo_container_root TYPE REF TO cl_gui_container .
    "! <p class="shorttext synchronized" lang="en">Control for summary data</p>
    DATA mo_container_summary TYPE REF TO cl_gui_container .
    "! <p class="shorttext synchronized" lang="en">Control for list data</p>
    DATA mo_container_list TYPE REF TO cl_gui_container .
    "! <p class="shorttext synchronized" lang="en">Control for detail data</p>
    DATA mo_container_detail TYPE REF TO cl_gui_container .
    "! <p class="shorttext synchronized" lang="en">SAP Monitor Interface App - Show Details</p>
    DATA mo_detail TYPE REF TO zcl_intfmonitor_gui_detail .
    "! <p class="shorttext synchronized" lang="en">SAP Monitor Interface App - Show summary</p>
    DATA mo_summary TYPE REF TO zcl_intfmonitor_gui_summary .
    TYPE-POOLS abap .
    "! <p class="shorttext synchronized" lang="en">Detail is shown</p>
    DATA mf_detail_is_shown TYPE abap_bool VALUE abap_false. "#EC NOTEXT .

    "! <p class="shorttext synchronized" lang="en">Initializes application data</p>
    "!
    "! @parameter it_list | <p class="shorttext synchronized" lang="en">List of interface executions</p>
    METHODS initialize
      IMPORTING
        !it_list TYPE ztt_zintfmonitor030
      RAISING
        zcx_intfmonitor .
    CLASS zcl_zintfmonitor020_read DEFINITION LOAD .
    "! <p class="shorttext synchronized" lang="en">Executes search</p>
    "!
    "! @parameter is_filter_by | <p class="shorttext synchronized" lang="en">All fields in ranges</p>
    "! @parameter et_list      | <p class="shorttext synchronized" lang="en">List of interface executions</p>
    METHODS do_search
      IMPORTING
        !is_filter_by TYPE zcl_zintfmonitor020_read=>mtyp_all_fields
      EXPORTING
        !et_list      TYPE ztt_zintfmonitor030 .
    "! <p class="shorttext synchronized" lang="en">Builds screen</p>
    METHODS build_screen .
    "! <p class="shorttext synchronized" lang="en">Handles ListInterfaceSelected() event</p>
    METHODS on_list_interface_selected
        FOR EVENT interface_selected OF zcl_intfmonitor_gui_list .
ENDCLASS.



CLASS zcl_intfmonitor_gui_appl IMPLEMENTATION.


  METHOD build_screen.
    DATA: lo_dummy_container TYPE REF TO cl_gui_container.

* Split main container into 2 horizontal containers. Upper container
* will be also splitted vertically into two additional containers where left one
* will be used for displaying summary data while rigth one will be will display a list
* Finally, bottom container will display the details.

    CREATE OBJECT mo_splitter_horizontal
      EXPORTING
        parent  = mo_container_root
        rows    = 2
        columns = 1
      EXCEPTIONS
        OTHERS  = 0.

    mo_splitter_horizontal->set_row_height( id = 1 height = 100 ).
    mo_splitter_horizontal->set_row_sash( id    = 1
                                          type  = cl_gui_splitter_container=>type_movable
                                          value = cl_gui_splitter_container=>false ).

    mo_splitter_horizontal->set_row_sash( id    = 1
                                          type  = cl_gui_splitter_container=>type_sashvisible
                                          value = cl_gui_splitter_container=>false ).

    lo_dummy_container  = mo_splitter_horizontal->get_container( row = 1 column = 1 ).
    mo_container_detail = mo_splitter_horizontal->get_container( row = 2 column = 1 ).

    CREATE OBJECT mo_splitter_vertical
      EXPORTING
        parent  = lo_dummy_container
        rows    = 1
        columns = 2
      EXCEPTIONS
        OTHERS  = 0.

    mo_container_summary  = mo_splitter_vertical->get_container( row = 1 column = 1 ).
    mo_container_list     = mo_splitter_vertical->get_container( row = 1 column = 2 ).

    mo_splitter_vertical->set_column_width( id = 1 width = 20 ).

  ENDMETHOD.


  METHOD display.

    mo_container_root = io_container.

* Build screen sections
    build_screen( ).

* Show data on screen
    mo_summary->display( mo_container_summary ).
    mo_list->display( mo_container_list ).
    mo_detail->display( mo_container_detail ).

* Handle events
    SET HANDLER mo_list->on_summary_interface_selected FOR mo_summary.
    SET HANDLER mo_detail->on_list_interface_selected  FOR mo_list.
    SET HANDLER me->on_list_interface_selected         FOR mo_list.
  ENDMETHOD.


  METHOD do_search.
    DATA: lt_list      TYPE ztt_zintfmonitor020.

    FIELD-SYMBOLS: <ls_list>       LIKE LINE OF lt_list,
                   <ls_found_list> LIKE LINE OF et_list.

    TRY.
        zcl_zintfmonitor020_read=>get_list_by( EXPORTING is_filter_by = is_filter_by
                                               IMPORTING et_list      = lt_list ).

        LOOP AT lt_list ASSIGNING <ls_list>.
          INSERT INITIAL LINE INTO TABLE et_list ASSIGNING <ls_found_list>.
          <ls_found_list> = zcf_intfmonitor=>find_by_guid( <ls_list>-guid ).
        ENDLOOP.

      CATCH cx_db2_not_found .
*   Do nothing
    ENDTRY.

  ENDMETHOD.


  METHOD initialize.

    CLEAR: mo_list,
           mo_detail,
           mo_summary.

    CREATE OBJECT mo_list.
    CREATE OBJECT mo_summary.
    CREATE OBJECT mo_detail.

    mo_list->set_list( it_list ).
    mo_detail->set_list( it_list ).
    mo_summary->set_list( it_list ).

  ENDMETHOD.


  METHOD on_list_interface_selected.

    IF mf_detail_is_shown = abap_false.
      mo_splitter_horizontal->set_row_sash( id    = 1
                                            type  = cl_gui_splitter_container=>type_movable
                                            value = cl_gui_splitter_container=>true ).

      mo_splitter_horizontal->set_row_sash( id    = 1
                                            type  = cl_gui_splitter_container=>type_sashvisible
                                            value = cl_gui_splitter_container=>true ).

      mo_splitter_horizontal->set_row_height( id = 1 height = 50 ).
      mo_splitter_horizontal->set_row_height( id = 2 height = 50 ).
      mf_detail_is_shown = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD search.
    DATA: ls_filter_by TYPE zcl_zintfmonitor020_read=>mtyp_all_fields,
          lt_list      TYPE ztt_zintfmonitor030.

    ls_filter_by-intfid        = is_filters-intfid.
    ls_filter_by-procdate      = is_filters-procdate.
    ls_filter_by-proctime      = is_filters-proctime.
    ls_filter_by-procby        = is_filters-procby.
    ls_filter_by-procendtype   = is_filters-procendtype.

    do_search( EXPORTING is_filter_by = ls_filter_by
               IMPORTING et_list      = lt_list ).

    initialize( lt_list ).

  ENDMETHOD.
ENDCLASS.
