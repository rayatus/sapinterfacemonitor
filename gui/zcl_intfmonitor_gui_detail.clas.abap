"! <p class="shorttext synchronized" lang="en">SAP Monitor Interface App - Show Details</p>
CLASS zcl_intfmonitor_gui_detail DEFINITION
  PUBLIC
  INHERITING FROM zcl_intfmonitor_gui_appl_comp
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
*"* public components of class ZCL_INTFMONITOR_GUI_DETAIL
*"* do not include other source files here!!!
    TYPE-POOLS abap .

    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    METHODS constructor .
    "! <p class="shorttext synchronized" lang="en">Handles ListInterfaceSelected() event</p>
    METHODS on_list_interface_selected
          FOR EVENT interface_selected OF zcl_intfmonitor_gui_list
      IMPORTING
          !io_interface .

    METHODS set_list
        REDEFINITION .
  PROTECTED SECTION.
*"* protected components of class ZCL_INTFMONITOR_GUI_DETAIL
*"* do not include other source files here!!!

    METHODS prepare_data
        REDEFINITION .
    METHODS _display
        REDEFINITION .
  PRIVATE SECTION.
*"* private components of class ZCL_INTFMONITOR_GUI_DETAIL
*"* do not include other source files here!!!

    TYPES mtyp_t_tree_data TYPE zcl_intfmonitor_util=>mtyp_t_intf_param .

    "! <p class="shorttext synchronized" lang="en">Dynamic Document for displaying title</p>
    DATA mo_dd_document TYPE REF TO cl_dd_document .
    "! <p class="shorttext synchronized" lang="en">Main container default size</p>
    CONSTANTS mc_container_height TYPE i VALUE 100.         "#EC NOTEXT
    "! <p class="shorttext synchronized" lang="en">Processing parameter</p>
    CONSTANTS mc_processing TYPE zzeintfdatatype VALUE 'P'. "#EC NOTEXT
    "! <p class="shorttext synchronized" lang="en">Returning parameter</p>
    CONSTANTS mc_returning TYPE zzeintfdatatype VALUE 'R'.  "#EC NOTEXT
    "! <p class="shorttext synchronized" lang="en">Main container current size</p>
    DATA md_container_height TYPE i VALUE 0.              "#EC NOTEXT .
    "! <p class="shorttext synchronized" lang="en">Grid Container</p>
    DATA mo_container_grid TYPE REF TO cl_gui_container .
    "! <p class="shorttext synchronized" lang="en">Container for title</p>
    DATA mo_container_title TYPE REF TO cl_gui_container .
    "! <p class="shorttext synchronized" lang="en">Tree Container</p>
    DATA mo_container_tree TYPE REF TO cl_gui_container .
    "! <p class="shorttext synchronized" lang="en">Grid</p>
    DATA mo_grid TYPE REF TO cl_salv_table .
    "! <p class="shorttext synchronized" lang="en">Splitter Control</p>
    DATA mo_splitter_horizontal TYPE REF TO cl_gui_splitter_container .
    "! <p class="shorttext synchronized" lang="en">Splitter Control</p>
    DATA mo_splitter_vertical TYPE REF TO cl_gui_splitter_container .
    "! <p class="shorttext synchronized" lang="en">Tree</p>
    DATA mo_tree TYPE REF TO cl_salv_tree .
    "! <p class="shorttext synchronized" lang="en">Data to be displayed in grid</p>
    DATA mr_grid_data TYPE REF TO data .
    "! <p class="shorttext synchronized" lang="en">Empty Tree Data</p>
    DATA mt_data TYPE mtyp_t_tree_data .
    "! <p class="shorttext synchronized" lang="en">Tree Data</p>
    DATA mt_tree_data TYPE mtyp_t_tree_data .

    EVENTS parameter_selected
      EXPORTING
        VALUE(id_param) TYPE zzeintfdatapar .

    "! <p class="shorttext synchronized" lang="en">Builds grid</p>
    METHODS build_grid
      CHANGING
        !it_data TYPE ANY TABLE .
    "! <p class="shorttext synchronized" lang="en">Builds screen</p>
    METHODS build_screen .
    "! <p class="shorttext synchronized" lang="en">Builds Title</p>
    METHODS build_title .
    "! <p class="shorttext synchronized" lang="en">Builds Tree</p>
    METHODS build_tree .
    "! <p class="shorttext synchronized" lang="en">Builds Tree Section</p>
    "!
    "! @parameter id_datatype | <p class="shorttext synchronized" lang="en">Data type - processing or returning data</p>
    METHODS build_tree_section
      IMPORTING
        id_datatype TYPE zzeintfdatatype .
    "! <p class="shorttext synchronized" lang="en">Handles ListInterfaceSelected() event</p>
    "!
    "! @parameter io_interface | <p class="shorttext synchronized" lang="en">SAP Interface Monitor</p>
    METHODS display_interface_data
      IMPORTING
                io_interface TYPE REF TO zif_intfmonitor
      RAISING   zcx_intfmonitor.
    "! <p class="shorttext synchronized" lang="en">Handles ParameterSelected() event</p>
    "!
    "! @parameter id_param | <p class="shorttext synchronized" lang="en">Parameter</p>
    METHODS display_param_data
      IMPORTING id_param TYPE zzeintfdatapar
      RAISING   zcx_intfmonitor.
    "! <p class="shorttext synchronized" lang="en">Handles ContainerResize( ) Event</p>
    METHODS on_container_resize
        FOR EVENT size_control OF cl_gui_container .
    "! <p class="shorttext synchronized" lang="en">Handles ParameterSelected() event</p>
    METHODS on_parameter_selected
          FOR EVENT parameter_selected OF zcl_intfmonitor_gui_detail
      IMPORTING
          id_param .
    "! <p class="shorttext synchronized" lang="en">Handles TreeDoubleClick() event</p>
    METHODS on_tree_double_click
          FOR EVENT double_click OF cl_salv_events_tree
      IMPORTING
          node_key
          columnname .
    "! <p class="shorttext synchronized" lang="en">Handles TreeLinkClick() event</p>
    METHODS on_tree_link_click
          FOR EVENT link_click OF cl_salv_events_tree
      IMPORTING
          columnname
          node_key .
    "! <p class="shorttext synchronized" lang="en">Raises parameter selected event</p>
    "!
    "! @parameter io_node | <p class="shorttext synchronized" lang="en">Single Node Object of Tree Structure</p>
    METHODS raise_parameter_selected
      IMPORTING
        io_node TYPE REF TO cl_salv_node .
ENDCLASS.



CLASS zcl_intfmonitor_gui_detail IMPLEMENTATION.


  METHOD build_grid.
    TYPES: BEGIN OF ltyp_s_col_texts,
             medium TYPE scrtext_m,
             long   TYPE scrtext_l,
             short  TYPE scrtext_s,
           END   OF ltyp_s_col_texts.

    DATA: lo_cols  TYPE REF TO cl_salv_columns_table,
          lf_new   TYPE xfeld,
          lt_cols  TYPE salv_t_column_ref,
          ls_col   LIKE LINE OF lt_cols,
          ls_text  TYPE ltyp_s_col_texts,
          lo_funcs TYPE REF TO cl_salv_functions_list.

    IF NOT mo_grid IS BOUND.
      TRY.
          lf_new = abap_true.
          cl_salv_table=>factory( EXPORTING r_container    = mo_container_grid
                                  IMPORTING r_salv_table   = mo_grid
                                  CHANGING  t_table        = it_data ).

        CATCH cx_salv_msg .
*       Do nothing
      ENDTRY.
    ELSE.
      lf_new = abap_false.
      TRY.
          mo_grid->set_data( CHANGING t_table = it_data ).
        CATCH cx_salv_no_new_data_allowed.
          "Do nothing
      ENDTRY.
    ENDIF.

* Prepare Grid buttons
    lo_funcs = mo_grid->get_functions( ).
    lo_funcs->set_all( ).

* Prepare columns
    lo_cols = mo_grid->get_columns( ).
    lo_cols->set_optimize( value = abap_true ).

    lt_cols = lo_cols->get( ).
    LOOP AT lt_cols INTO ls_col.
      ls_text-medium = ls_col-r_column->get_medium_text( ).
      ls_text-long   = ls_col-r_column->get_long_text( ).
      ls_text-short  = ls_col-r_column->get_short_text( ).
      IF ls_text IS INITIAL.
        ls_text-medium = ls_col-columnname.
        ls_text-short  = ls_col-columnname.
        ls_text-long   = ls_col-columnname.
        ls_col-r_column->set_medium_text( ls_text-medium ).
        ls_col-r_column->set_long_text( ls_text-long ).
        ls_col-r_column->set_short_text( ls_text-short ).
      ENDIF.

    ENDLOOP.

    IF lf_new = abap_true.
      mo_grid->display( ).
    ELSE.
      mo_grid->refresh( ).
    ENDIF.
  ENDMETHOD.


  METHOD build_screen.
    DATA: lo_dummy_container TYPE REF TO cl_gui_container.

* Split main container into 2 additional containers. Left one will
* display a tree where diferent nodes will represent all different
* inbound/outbound attributes while right one will display
* corresponding content of each attribute.


    CREATE OBJECT mo_splitter_horizontal
      EXPORTING
        parent  = mo_container
        rows    = 2
        columns = 1
      EXCEPTIONS
        OTHERS  = 999.

    mo_splitter_horizontal->set_row_height( id  = 1 height = 10 ).

    mo_splitter_horizontal->set_row_sash( id    = 1
                                          type  = cl_gui_splitter_container=>type_movable
                                          value = cl_gui_splitter_container=>false ).

    mo_splitter_horizontal->set_row_sash( id    = 1
                                          type  = cl_gui_splitter_container=>type_sashvisible
                                          value = cl_gui_splitter_container=>false ).


    mo_container_title = mo_splitter_horizontal->get_container( row = 1 column = 1 ).
    lo_dummy_container = mo_splitter_horizontal->get_container( row = 2 column = 1 ).

    CREATE OBJECT mo_splitter_vertical
      EXPORTING
        parent  = lo_dummy_container
        rows    = 1
        columns = 2
      EXCEPTIONS
        OTHERS  = 999.

    mo_splitter_vertical->set_column_width( id = 1 width = 20 ).

    mo_container_tree  = mo_splitter_vertical->get_container( row = 1 column = 1 ).
    mo_container_grid  = mo_splitter_vertical->get_container( row = 1 column = 2 ).

  ENDMETHOD.


  METHOD build_title.
    DATA: lf_refresh         TYPE xfeld,
          lo_table           TYPE REF TO cl_dd_table_area,
          ld_text            TYPE sdydo_text_element,
          lo_intfmonitor     TYPE REF TO zif_intfmonitor,
          ls_detail_x        TYPE zeintfmonitor_detail_x,
          ld_string_procdate TYPE c LENGTH 10,
          ld_string_proctime TYPE c LENGTH 8.

    IF NOT mo_dd_document IS BOUND.
      lf_refresh = abap_true.
    ENDIF.

* Prepare text to be displayed
    READ TABLE mt_list INDEX 1 INTO lo_intfmonitor.
    ls_detail_x = lo_intfmonitor->get_detail_x( ).

    WRITE ls_detail_x-procdate TO ld_string_procdate.
    WRITE ls_detail_x-proctime TO ld_string_proctime.

    CONCATENATE ls_detail_x-intfid
                ls_detail_x-xintfid
                INTO ld_text SEPARATED BY '-'.

    CONCATENATE ld_text
                ':'
                INTO ld_text.

    CONCATENATE ld_text
                ld_string_procdate
                ld_string_proctime
                INTO ld_text SEPARATED BY space.


* Build title
    CREATE OBJECT mo_dd_document.
    mo_dd_document->add_text( text = ld_text ).

    mo_dd_document->merge_document( ).
    IF lf_refresh IS INITIAL.
      mo_dd_document->display_document( parent = mo_container_title ).
    ELSE.
      mo_dd_document->display_document( reuse_control = abap_true reuse_registration = abap_true ).
    ENDIF.

  ENDMETHOD.


  METHOD build_tree.
    DATA: lo_cols     TYPE REF TO  cl_salv_columns_tree,
          lo_col      TYPE REF TO  cl_salv_column_tree,
          lt_cols     TYPE         salv_t_column_ref,
          ls_col      LIKE LINE OF lt_cols,
          ld_icon     TYPE         salv_de_tree_image,
          lo_funcs    TYPE REF TO  cl_salv_functions_tree,
          lo_events   TYPE REF TO  cl_salv_events_tree,
          lo_settings TYPE REF TO  cl_salv_tree_settings,
          lo_nodes    TYPE REF TO  cl_salv_nodes,
          lf_new      TYPE         xfeld.

    IF mo_tree IS BOUND.
*   It's not necessary to build once again Tree, just delete its content
*   and insert new data
      lo_nodes = mo_tree->get_nodes( ).
      TRY.
          lo_nodes->delete_all( ).
        CATCH cx_salv_error.
          "Do nothing
      ENDTRY.
    ELSE.
*   It's necessary to create new tree
      TRY.
          lf_new = abap_true.
          cl_salv_tree=>factory( EXPORTING r_container = mo_container_tree
                                 IMPORTING r_salv_tree = mo_tree
                                 CHANGING  t_table     = mt_data ).

          lo_settings = mo_tree->get_tree_settings( ).
          lo_settings->set_hierarchy_header( 'Parameters'(001) ).
          lo_settings->set_hierarchy_tooltip( 'Parameters'(001) ).
          lo_settings->set_hierarchy_size( 30 ).
          lo_settings->set_header( 'Parameters'(001) ).

          CALL FUNCTION 'ICON_CREATE'
            EXPORTING
              name                  = icon_oo_method
              text                  = 'Parameters'(001)
              info                  = 'Parameters'(001)
              add_stdinf            = ' '
            IMPORTING
              result                = ld_icon
            EXCEPTIONS
              icon_not_found        = 1
              outputfield_too_short = 2.

          lo_settings->set_hierarchy_icon( ld_icon ).

          lo_cols = mo_tree->get_columns( ).
          lt_cols = lo_cols->get( ).
          LOOP AT lt_cols INTO ls_col.
            CASE ls_col-columnname.
              WHEN 'XPARAM'.
                ls_col-r_column->set_optimized( ).
              WHEN OTHERS.
                ls_col-r_column->set_technical( ).
            ENDCASE.
          ENDLOOP.

          lo_funcs = mo_tree->get_functions( ).
          lo_funcs->set_all( value = abap_true ).

          lo_events = mo_tree->get_event( ).
          SET HANDLER on_tree_double_click FOR lo_events.
          SET HANDLER on_tree_link_click   FOR lo_events.

        CATCH cx_salv_error .

      ENDTRY.
    ENDIF.

* Insert current data into Tree
    build_tree_section( mc_processing ).
    build_tree_section( mc_returning ).

    IF lf_new = abap_true.
      mo_tree->display( ).
    ENDIF.

  ENDMETHOD.


  METHOD build_tree_section.
    DATA: lo_nodes     TYPE REF TO cl_salv_nodes,
          ld_dom_text  TYPE dd07v-ddtext,
          lo_node      TYPE REF TO cl_salv_node,
          ld_node_key  TYPE salv_de_node_key,
          ld_text      TYPE lvc_value,
          ls_tree_data LIKE LINE OF mt_tree_data,
          lo_item      TYPE REF TO cl_salv_item.

    lo_nodes = mo_tree->get_nodes( ).

    READ TABLE mt_tree_data WITH KEY datatype = id_datatype TRANSPORTING NO FIELDS.
    IF sy-subrc IS INITIAL.
      ld_text = ld_dom_text = zcl_intfmonitor_util=>get_domain_text( id_domname = 'ZZDINTFDATATYPE' id_value = id_datatype ).

      CLEAR: ld_node_key.
      TRY.
          lo_node  = lo_nodes->add_node( related_node = ld_node_key
                                         relationship = cl_gui_column_tree=>relat_last_child
                                         folder       = abap_true
                                         expander     = abap_true
                                         text         = ld_text ).

          lo_node->expand( ).

          ld_node_key = lo_node->get_key( ).

          LOOP AT mt_tree_data INTO ls_tree_data WHERE datatype = id_datatype.
            ld_text = ls_tree_data-param.
            lo_node = lo_nodes->add_node( related_node = ld_node_key
                                          relationship = cl_gui_column_tree=>relat_last_child
                                          text         = ld_text
                                          data_row     = ls_tree_data ).
            lo_item = lo_node->get_hierarchy_item( ).
            lo_item->set_type( if_salv_c_item_type=>link ).
          ENDLOOP.
        CATCH cx_salv_msg.
          "Do nothing
      ENDTRY.
    ENDIF.


  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).
    SET HANDLER me->on_parameter_selected FOR me.

  ENDMETHOD.


  METHOD display_interface_data.
    DATA: ls_tree_data LIKE LINE OF mt_tree_data.

    CLEAR mt_list[].
    INSERT io_interface INTO TABLE mt_list.

    prepare_data( ).
    build_tree( ).
    build_title( ).

* Simulate one param has been chosen in order to build Grid with param data
    READ TABLE mt_tree_data WITH KEY datatype = mc_processing INTO ls_tree_data.
    IF NOT sy-subrc IS INITIAL.
      READ TABLE mt_tree_data WITH KEY datatype = mc_returning INTO ls_tree_data.
      IF NOT sy-subrc IS INITIAL.
        READ TABLE mt_tree_data INDEX 1 INTO ls_tree_data.
      ENDIF.
    ENDIF.
    display_param_data( ls_tree_data-param  ).

  ENDMETHOD.


  METHOD display_param_data.
    DATA: ls_list         LIKE LINE OF mt_list,
          ls_tree_data    LIKE LINE OF mt_tree_data,
          lo_type_descr   TYPE REF TO cl_abap_typedescr,
          lo_struct_descr TYPE REF TO cl_abap_structdescr,
          lo_table_descr  TYPE REF TO cl_abap_tabledescr,
          lo_abap_descr   TYPE REF TO cl_abap_datadescr,
          lt_components   TYPE abap_component_tab,
          lr_data         TYPE REF TO data,
          lr_row          TYPE REF TO data,
          ld_counter      TYPE i.

    FIELD-SYMBOLS: <lr_data>      TYPE any,
                   <lt_data>      TYPE ANY TABLE,
                   <ls_data>      TYPE any,
                   <ls_component> LIKE LINE OF lt_components.

* Preconditions
    READ TABLE mt_list INDEX 1 INTO ls_list.
    ASSERT sy-subrc IS INITIAL.

    READ TABLE mt_tree_data WITH KEY param = id_param INTO ls_tree_data.
    ASSERT sy-subrc IS INITIAL.

* Retrieve parameter data from interface execution
    ls_list->get_parameter( EXPORTING id_param = id_param
                            IMPORTING er_data  = lr_data ).

* Determine stored type and create internal table
    lo_type_descr =  cl_abap_typedescr=>describe_by_data_ref( lr_data ).
    lo_abap_descr ?= lo_type_descr.

    CASE lo_type_descr->type_kind.
      WHEN cl_abap_typedescr=>typekind_table.
*     It's already an internal table. Just display it
        ASSIGN lr_data->* TO <lt_data>.

      WHEN OTHERS.
*     We need to create an internal table
        ASSIGN lr_data->* TO <lr_data>.

        CASE lo_type_descr->kind.
          WHEN cl_abap_typedescr=>kind_table.
            lo_table_descr ?= lo_abap_descr.

          WHEN cl_abap_typedescr=>kind_struct.
            lo_struct_descr ?= lo_abap_descr.
            lo_table_descr  = cl_abap_tabledescr=>create( p_line_type = lo_struct_descr ).

          WHEN OTHERS.
            INSERT INITIAL LINE INTO TABLE lt_components ASSIGNING <ls_component>.
            <ls_component>-name = id_param.
            <ls_component>-type = lo_abap_descr.

            lo_struct_descr = cl_abap_structdescr=>create( p_components = lt_components ).
            lo_table_descr  = cl_abap_tabledescr=>create( p_line_type   = lo_struct_descr ).
        ENDCASE.

*     We need to create an internal table in order to add to it stored data
        CREATE DATA lr_row TYPE HANDLE lo_struct_descr.
        ASSIGN lr_row->* TO <ls_data>.

        CREATE DATA mr_grid_data TYPE HANDLE lo_table_descr.
        ASSIGN mr_grid_data->* TO <lt_data>.

        <ls_data> = <lr_data>.

        INSERT <ls_data> INTO TABLE <lt_data>.
    ENDCASE.

* Build grid for displaying internal table
    build_grid( CHANGING it_data = <lt_data> ).

  ENDMETHOD.


  METHOD on_container_resize.
    DATA: ld_height TYPE i.

    mo_container->get_height( IMPORTING height = ld_height ).

    ld_height = ld_height - 10.

    mo_container_title->set_height( 10 ).

    mo_container_tree->set_height( ld_height ).
    mo_container_grid->set_height( ld_height ).

  ENDMETHOD.


  METHOD on_list_interface_selected.

    TRY.
        display_interface_data( io_interface ).
      CATCH zcx_intfmonitor.
        "Do nothing
    ENDTRY.

  ENDMETHOD.


  METHOD on_parameter_selected.

    TRY.
        display_param_data( id_param = id_param   ).
      CATCH zcx_intfmonitor.
        "Do nothing
    ENDTRY.

  ENDMETHOD.


  METHOD on_tree_double_click.
    DATA: lo_nodes TYPE REF TO cl_salv_nodes,
          lo_node  TYPE REF TO cl_salv_node.

    TRY .
        lo_nodes = mo_tree->get_nodes( ).
        lo_node  = lo_nodes->get_node( node_key ).

        IF lo_node IS BOUND.
          raise_parameter_selected( lo_node ).
        ENDIF.
      CATCH cx_salv_msg.
*     Do nothing
    ENDTRY.

  ENDMETHOD.


  METHOD on_tree_link_click.
    DATA: lo_nodes TYPE REF TO cl_salv_nodes,
          lo_node  TYPE REF TO cl_salv_node.

    TRY .
        lo_nodes = mo_tree->get_nodes( ).
        lo_node  = lo_nodes->get_node( node_key ).

        IF lo_node IS BOUND.
          raise_parameter_selected( lo_node ).
        ENDIF.
      CATCH cx_salv_msg.
*     Do nothing
    ENDTRY.
  ENDMETHOD.


  METHOD prepare_data.
    DATA:          lt_definition TYPE zcl_intfmonitor_util=>mtyp_t_intf_param.

    FIELD-SYMBOLS: <ls_list>     LIKE LINE OF mt_list.

    ASSERT lines( mt_list ) = 1.

    CLEAR mt_tree_data[].

    LOOP AT mt_list ASSIGNING <ls_list>.
      zcl_intfmonitor_util=>get_interface_param_definition( EXPORTING io_interface  = <ls_list>
                                                            IMPORTING et_definition = lt_definition ).
      INSERT LINES OF lt_definition INTO TABLE mt_tree_data.
    ENDLOOP.

    SORT mt_tree_data.

  ENDMETHOD.


  METHOD raise_parameter_selected.
    DATA: ld_data TYPE REF TO data,
          ls_list LIKE LINE OF mt_list.

    FIELD-SYMBOLS: <ls_tree_data> LIKE LINE OF mt_data.

    ld_data = io_node->get_data_row( ).
    ASSIGN ld_data->* TO <ls_tree_data>.

    READ TABLE mt_list INDEX 1 INTO ls_list.
    RAISE EVENT parameter_selected EXPORTING id_param     = <ls_tree_data>-param.


  ENDMETHOD.


  METHOD set_list.

* Do Nothing - All data will be obtained from event InterfaceSelected

  ENDMETHOD.


  METHOD _display.

    SET HANDLER me->on_container_resize FOR mo_container.
    build_screen( ).

  ENDMETHOD.
ENDCLASS.
