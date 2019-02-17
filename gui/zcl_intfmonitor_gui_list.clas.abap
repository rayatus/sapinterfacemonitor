"! <p class="shorttext synchronized" lang="en">Shows a list with info about interface executions</p>
CLASS zcl_intfmonitor_gui_list DEFINITION
  PUBLIC
  INHERITING FROM zcl_intfmonitor_gui_appl_comp
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.


    "! <p class="shorttext synchronized" lang="en">Interface execution has been selected</p>
    "!
    "! @parameter io_interface | <p class="shorttext synchronized" lang="en">SAP Interface Monitor</p>
    EVENTS interface_selected
      EXPORTING
        VALUE(io_interface) TYPE REF TO zif_intfmonitor .

    "! <p class="shorttext synchronized" lang="en">Handles SummaryInterfaceSelected( ) event</p>
    "! @parameter it_intfmonitor | Selected interfaces
    METHODS on_summary_interface_selected
          FOR EVENT interface_selected OF zcl_intfmonitor_gui_summary
      IMPORTING
          it_intfmonitor .
  PROTECTED SECTION.


    "! <p class="shorttext synchronized" lang="en">Sets Grid data to be displayed</p>
    "!
    "! @parameter id_filter_by_intfid | <p class="shorttext synchronized" lang="en">Interface Id</p>
    METHODS set_grid_data
      IMPORTING
        id_filter_by_intfid TYPE zzeintfid OPTIONAL PREFERRED PARAMETER id_filter_by_intfid.

    METHODS prepare_data
        REDEFINITION .
    METHODS _display
        REDEFINITION .
  PRIVATE SECTION.


    TYPES:
      BEGIN OF mtyp_s_alv_data,
        guid TYPE zintfmonitor020-guid.
        INCLUDE TYPE zeintfmonitor_detail_x.
    TYPES: rows TYPE zzealvrow,
           END   OF mtyp_s_alv_data .
    TYPES:
      mtyp_t_alv_data TYPE STANDARD TABLE OF mtyp_s_alv_data .

    "! <p class="shorttext synchronized" lang="en">Basis Class for Simple Tables</p>
    DATA mo_grid TYPE REF TO cl_salv_table .
    "! <p class="shorttext synchronized" lang="en">Data to be displayed in Grid</p>
    DATA mt_grid_data TYPE mtyp_t_alv_data .
    "! <p class="shorttext synchronized" lang="en">ALV Control: Title bar text</p>
    DATA md_title TYPE lvc_title .

    "! <p class="shorttext synchronized" lang="en">Handles Grid doubleClick() event</p>
    "! @parameter row | Row selected
    "! @parameter column | Column selected
    METHODS on_grid_double_click
          FOR EVENT double_click OF cl_salv_events_table
      IMPORTING
          row
          column .
    "! <p class="shorttext synchronized" lang="en">Handes Grid LinkClick() event</p>
    "! @parameter row | Row selected
    "! @parameter column | Column selected
    METHODS on_grid_link_click
          FOR EVENT link_click OF cl_salv_events_table
      IMPORTING
          row
          column .
    "! <p class="shorttext synchronized" lang="en">Raise InterfaceSelected() event</p>
    "!
    "! @parameter id_guid | <p class="shorttext synchronized" lang="en">Process Guid</p>
    METHODS raise_interface_selected
      IMPORTING
        id_guid TYPE zintfmonitor020-guid OPTIONAL
          PREFERRED PARAMETER id_guid .

    "! <p class="shorttext synchronized" lang="en">Prepare column disposition</p>
    "!
    METHODS set_grid_columns.
    METHODS set_grid_layout.

ENDCLASS.



CLASS zcl_intfmonitor_gui_list IMPLEMENTATION.


  METHOD on_grid_double_click.
    DATA: ls_grid_data LIKE LINE OF mt_grid_data.

    READ TABLE mt_grid_data INDEX row INTO ls_grid_data.
    IF sy-subrc IS INITIAL.
      raise_interface_selected( ls_grid_data-guid ).
    ENDIF.

  ENDMETHOD.


  METHOD on_grid_link_click.
    DATA: ls_grid_data LIKE LINE OF mt_grid_data.

    READ TABLE mt_grid_data INDEX row INTO ls_grid_data.
    IF sy-subrc IS INITIAL.
      raise_interface_selected( ls_grid_data-guid ).
    ENDIF.
  ENDMETHOD.


  METHOD on_summary_interface_selected.

    IF it_intfmonitor <> mt_list.

      mt_list = it_intfmonitor.

      TRY.
          prepare_data( ).
        CATCH zcx_intfmonitor.
          "Do noting.
      ENDTRY.
      mo_grid->refresh( ).

    ENDIF.
  ENDMETHOD.


  METHOD prepare_data.

    set_grid_data( ).
    set_grid_columns( ).

  ENDMETHOD.


  METHOD raise_interface_selected.

    DATA: lo_interface TYPE REF TO zif_intfmonitor.

    READ TABLE mt_list WITH KEY table_line->md_guid = id_guid INTO lo_interface.
    IF sy-subrc IS INITIAL.
      RAISE EVENT interface_selected EXPORTING io_interface = lo_interface.
    ENDIF.

  ENDMETHOD.


  METHOD set_grid_data.
    DATA: ls_detail_x             TYPE zeintfmonitor_detail_x.

    FIELD-SYMBOLS: <ls_list>      LIKE LINE OF mt_list,
                   <ls_grid_data> LIKE LINE OF mt_grid_data.

    CLEAR mt_grid_data.
    LOOP AT mt_list ASSIGNING <ls_list>.
      ls_detail_x = <ls_list>->get_detail_x( ).

      INSERT INITIAL LINE INTO TABLE mt_grid_data ASSIGNING <ls_grid_data>.

      MOVE-CORRESPONDING ls_detail_x TO <ls_grid_data>.
      <ls_grid_data>-guid   = <ls_list>->md_guid.
      <ls_grid_data>-rows   = 1.
    ENDLOOP.
    SORT mt_grid_data.

    IF mo_grid IS BOUND.
      TRY.
          mo_grid->set_data( CHANGING t_table = mt_grid_data ).
        CATCH cx_salv_no_new_data_allowed.
          "do nothing
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD _display.

    DATA: lo_functions TYPE REF TO cl_salv_functions_list,
          lo_settings  TYPE REF TO cl_salv_display_settings,
          lt_cols      TYPE salv_t_column_ref,

          lo_events    TYPE REF TO cl_salv_events_table.

    TRY.
        IF NOT mo_grid IS BOUND.

          cl_salv_table=>factory( EXPORTING r_container    = mo_container
                                  IMPORTING r_salv_table   = mo_grid
                                  CHANGING  t_table        = mt_grid_data ).

        ENDIF.

        lo_settings = mo_grid->get_display_settings( ).
        lo_settings->set_striped_pattern( value = abap_true ).
        lo_settings->set_list_header( md_title ).
        lo_settings->set_list_header_size( cl_salv_display_settings=>c_header_size_small ).

*     Prepare buttons
        lo_functions = mo_grid->get_functions( ).
        lo_functions->set_all( ).

*     Prepare columns
        set_grid_columns( ).

*     Prepare layout
        set_grid_layout( ).


*     Handle Events
        lo_events = mo_grid->get_event( ).
        SET HANDLER on_grid_double_click FOR lo_events.
        SET HANDLER on_grid_link_click FOR lo_events.

        mo_grid->display( ).

      CATCH cx_salv_msg .
        "Do nothing
    ENDTRY.
  ENDMETHOD.

  METHOD set_grid_columns.
    IF mo_grid IS BOUND.

      DATA(lo_columns) = mo_grid->get_columns( ).
      lo_columns->set_optimize( ).

      DATA(lt_cols) = lo_columns->get( ).
      LOOP AT lt_cols INTO DATA(ls_col).
        CASE ls_col-columnname.
          WHEN 'PROCDATE'.                                  "#EC NOTEXT
            CAST cl_salv_column_list( ls_col-r_column )->set_cell_type( if_salv_c_cell_type=>hotspot ).
            lo_columns->set_column_position( columnname = ls_col-columnname position   = 1 ).
          WHEN 'PROCTIME'.                                  "#EC NOTEXT
            CAST cl_salv_column_list( ls_col-r_column )->set_cell_type( if_salv_c_cell_type=>hotspot ).
            lo_columns->set_column_position( columnname = ls_col-columnname position   = 2 ).
          WHEN 'INTFID'.                                    "#EC NOTEXT
            lo_columns->set_column_position( columnname = ls_col-columnname position   = 3 ).
          WHEN 'XINTFID'.                                   "#EC NOTEXT
            lo_columns->set_column_position( columnname = ls_col-columnname position   = 4 ).
          WHEN 'PROCBY'.                                    "#EC NOTEXT
            lo_columns->set_column_position( columnname = ls_col-columnname position   = 5 ).
          WHEN 'XPROCBY'.                                   "#EC NOTEXT
            lo_columns->set_column_position( columnname = ls_col-columnname position   = 6 ).
          WHEN 'PROCENDTYPE'.                               "#EC NOTEXT
            ls_col-r_column->set_visible(  if_salv_c_bool_sap=>false ).
          WHEN 'XPROCENDTYPE'.                              "#EC NOTEXT
            lo_columns->set_column_position( columnname = ls_col-columnname position   = 7 ).
          WHEN 'LOGNUMBER'.                                 "#EC NOTEXT
            lo_columns->set_column_position( columnname = ls_col-columnname position   = 8 ).
          WHEN 'ROWS'.                                      "#EC NOTEXT
            TRY.
                lo_columns->set_count_column( ls_col-columnname ).
              CATCH cx_salv_data_error.
                "Do nothing
            ENDTRY.
            lo_columns->set_column_position( columnname = ls_col-columnname position   = 9 ).
          WHEN 'GUID'.                                      "#EC NOTEXT
            ls_col-r_column->set_visible( if_salv_c_bool_sap=>false ).

        ENDCASE.
      ENDLOOP.

    ENDIF.
  ENDMETHOD.


  METHOD set_grid_layout.
    DATA: ld_layout_key     TYPE salv_s_layout_key,
          ls_default_layout TYPE salv_s_layout.

    DATA(lo_layout)      = mo_grid->get_layout( ).

    ld_layout_key-report = sy-repid.
    ld_layout_key-handle = 'LIST'.                          "#EC NOTEXT
    lo_layout->set_key( ld_layout_key ).
    lo_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
    ls_default_layout    = lo_layout->get_default_layout( ).
    lo_layout->set_initial_layout( ls_default_layout-layout ).
    lo_layout->set_default( abap_true ). "allow user to set layouts as default

  ENDMETHOD.

ENDCLASS.
