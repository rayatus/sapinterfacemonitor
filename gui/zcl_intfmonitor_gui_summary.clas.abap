"! <p class="shorttext synchronized" lang="en">SAP Monitor Interface App - Show summary</p>
CLASS zcl_intfmonitor_gui_summary DEFINITION
  PUBLIC
  INHERITING FROM zcl_intfmonitor_gui_appl_comp
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPE-POOLS icon .

    "! <p class="shorttext synchronized" lang="en">inbound interfaces</p>
    CONSTANTS mc_inbound TYPE zzeintfinout VALUE '1'.       "#EC NOTEXT
    "! <p class="shorttext synchronized" lang="en">outbound interfaces</p>
    CONSTANTS mc_outbound TYPE zzeintfinout VALUE '2'.      "#EC NOTEXT
    "! <p class="shorttext synchronized" lang="en">Undefined</p>
    CONSTANTS mc_inbout_undefined TYPE zzeintfinout VALUE '0'. "#EC NOTEXT

    "! <p class="shorttext synchronized" lang="en">Interface execution has been selected</p>
    "!
    "! @parameter it_intfmonitor | <p class="shorttext synchronized" lang="en">Interface Executions</p>
    EVENTS interface_selected
      EXPORTING
        VALUE(it_intfmonitor) TYPE ztt_zintfmonitor030 .
  PROTECTED SECTION.


    METHODS prepare_data
        REDEFINITION .
    METHODS _display
        REDEFINITION .
  PRIVATE SECTION.


    TYPES:
      BEGIN OF mtyp_s_tree_data,
        inbout  TYPE zeintfmonitor010_detail_x-inbout,
        xinbout TYPE zeintfmonitor010_detail_x-xinbout,
        intfid  TYPE zeintfmonitor010_detail_x-intfid,
        xintfid TYPE zeintfmonitor010_detail_x-xintfid,
      END   OF mtyp_s_tree_data .
    TYPES:
      mtyp_t_tree_data TYPE STANDARD TABLE OF mtyp_s_tree_data .

    DATA mo_tree TYPE REF TO cl_salv_tree .
    DATA mt_tree_data TYPE mtyp_t_tree_data .
    DATA mt_data TYPE mtyp_t_tree_data .

    "! <p class="shorttext synchronized" lang="en">Builds Tree</p>
    METHODS build_tree .
    "! <p class="shorttext synchronized" lang="en">Builds Tree section</p>
    "!
    "! @parameter id_inbout | <p class="shorttext synchronized" lang="en">Inbound or Outbound interface</p>
    "! @parameter id_icon   | <p class="shorttext synchronized" lang="en">Image for Tree Hierarchy</p>
    METHODS build_tree_section
      IMPORTING
        !id_inbout TYPE zzeintfinout
        !id_icon   TYPE salv_de_tree_image OPTIONAL .
    "! <p class="shorttext synchronized" lang="en">Handles TreeDoubleClick() event</p>
    METHODS on_tree_double_click
          FOR EVENT double_click OF cl_salv_events_tree
      IMPORTING
          !node_key
          !columnname .
    "! <p class="shorttext synchronized" lang="en">Handles TreeLinkClick() event</p>
    METHODS on_tree_link_click
          FOR EVENT link_click OF cl_salv_events_tree
      IMPORTING
          !columnname
          !node_key .
    "! <p class="shorttext synchronized" lang="en">Raises InterfaceSelected() event</p>
    "!
    "! @parameter io_node | <p class="shorttext synchronized" lang="en">Single Node Object of Tree Structure</p>
    METHODS raise_interface_selected
      IMPORTING
        !io_node TYPE REF TO cl_salv_node .
ENDCLASS.



CLASS zcl_intfmonitor_gui_summary IMPLEMENTATION.


  METHOD build_tree.
    build_tree_section( id_inbout = mc_inbound          id_icon = 'ICON_ARROW_RIGHT' ).
    build_tree_section( id_inbout = mc_outbound         id_icon = 'ICON_ARROW_LEFT').
    build_tree_section( id_inbout = mc_inbout_undefined id_icon = 'ICON_DUMMY' ).
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

    READ TABLE mt_tree_data WITH KEY inbout = id_inbout TRANSPORTING NO FIELDS.
    IF sy-subrc IS INITIAL.
      ld_text = ld_dom_text = zcl_intfmonitor_util=>get_domain_text( id_domname = 'ZZDINTFINOUT' id_value = id_inbout ).

      CLEAR: ld_node_key.

      ls_tree_data-inbout  = id_inbout.
      ls_tree_data-xinbout = ld_text.
      TRY.
          lo_node  = lo_nodes->add_node( related_node   = ld_node_key
                                         relationship   = cl_gui_column_tree=>relat_last_child
                                         folder         = abap_true
                                         collapsed_icon = id_icon
                                         expanded_icon  = id_icon
                                         text           = ld_text
                                         data_row       = ls_tree_data ).

          lo_node->expand( ).
          ld_node_key = lo_node->get_key( ).

          LOOP AT mt_tree_data INTO ls_tree_data WHERE inbout = id_inbout.
            ld_text = ls_tree_data-intfid.
            lo_node = lo_nodes->add_node( related_node = ld_node_key
                                          relationship = cl_gui_column_tree=>relat_last_child
                                          text         = ld_text
                                          data_row     = ls_tree_data ).
            lo_item = lo_node->get_hierarchy_item( ).
            lo_item->set_type( if_salv_c_item_type=>link ).
          ENDLOOP.
        CATCH cx_salv_msg.
          "handle exception
      ENDTRY.
    ENDIF.
  ENDMETHOD.


  METHOD on_tree_double_click.
    DATA: lo_nodes TYPE REF TO cl_salv_nodes,
          lo_node  TYPE REF TO cl_salv_node.

    TRY .
        lo_nodes = mo_tree->get_nodes( ).
        lo_node  = lo_nodes->get_node( node_key ).

        IF lo_node IS BOUND.
          raise_interface_selected( lo_node ).
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
          raise_interface_selected( lo_node ).
        ENDIF.
      CATCH cx_salv_msg.
*     Do nothing
    ENDTRY.

  ENDMETHOD.


  METHOD prepare_data.
    DATA: ls_detail_x  TYPE zeintfmonitor010_detail_x,
          ls_tree_data LIKE LINE OF mt_tree_data.

    FIELD-SYMBOLS: <ls_list> LIKE LINE OF mt_list.

    LOOP AT mt_list ASSIGNING <ls_list>.
      TRY.
          ls_detail_x = zcl_zintfmonitor010_read=>get_detail_x( id_intfid = <ls_list>->ms_detail-intfid   ).
          MOVE-CORRESPONDING ls_detail_x TO ls_tree_data.
          INSERT ls_tree_data INTO TABLE mt_tree_data.
        CATCH cx_db2_not_found .
*       Do nothing
      ENDTRY.
    ENDLOOP.

    SORT mt_tree_data.
    DELETE ADJACENT DUPLICATES FROM mt_tree_data.


  ENDMETHOD.


  METHOD raise_interface_selected.
    DATA: ld_data        TYPE REF TO data,
          lr_inbout      TYPE RANGE OF mtyp_s_tree_data-inbout,
          lr_intfid      TYPE RANGE OF mtyp_s_tree_data-intfid,
          ls_inbout      LIKE LINE OF lr_inbout,
          ls_intfid      LIKE LINE OF lr_intfid,
          ls_tree_data   LIKE LINE OF mt_tree_data,
          ls_list        LIKE LINE OF mt_list,
          lt_intfmonitor TYPE ztt_zintfmonitor030.

    FIELD-SYMBOLS: <ls_tree_data> LIKE LINE OF mt_data.

    ld_data = io_node->get_data_row( ).
    ASSIGN ld_data->* TO <ls_tree_data>.

    IF NOT <ls_tree_data>-inbout IS INITIAL.
      ls_inbout-sign   = 'I'.                               "#EC NOTEXT
      ls_inbout-option = 'EQ'.                              "#EC NOTEXT
      ls_inbout-low    = <ls_tree_data>-inbout.
      INSERT ls_inbout INTO TABLE lr_inbout.
    ENDIF.

    IF NOT <ls_tree_data>-intfid IS INITIAL.
      ls_intfid-sign   = 'I'.                               "#EC NOTEXT
      ls_intfid-option = 'EQ'.                              "#EC NOTEXT
      ls_intfid-low    = <ls_tree_data>-intfid.
      INSERT ls_intfid INTO TABLE lr_intfid.
    ENDIF.


    LOOP AT mt_list INTO ls_list WHERE table_line->ms_detail-intfid IN lr_intfid.
      READ TABLE mt_tree_data INTO ls_tree_data WITH KEY intfid = ls_list->ms_detail-intfid.
      IF sy-subrc IS INITIAL AND ls_tree_data-inbout IN lr_inbout.
        INSERT ls_list INTO TABLE lt_intfmonitor[].
      ENDIF.
    ENDLOOP.


    RAISE EVENT interface_selected EXPORTING it_intfmonitor = lt_intfmonitor[].

  ENDMETHOD.


  METHOD _display.
    DATA: lo_cols     TYPE REF TO cl_salv_columns_tree,
          lo_col      TYPE REF TO cl_salv_column_tree,
          lt_cols     TYPE salv_t_column_ref,
          ls_col      LIKE LINE OF lt_cols,
          ld_icon     TYPE salv_de_tree_image,
          lo_funcs    TYPE REF TO cl_salv_functions_tree,
          lo_events   TYPE REF TO cl_salv_events_tree,
          lo_settings TYPE REF TO cl_salv_tree_settings.

    TRY.
        cl_salv_tree=>factory(
          EXPORTING
            r_container = mo_container
          IMPORTING
            r_salv_tree = mo_tree
          CHANGING
            t_table     = mt_data ).

        lo_settings = mo_tree->get_tree_settings( ).
        lo_settings->set_hierarchy_header( 'Summary'(001) ).
        lo_settings->set_hierarchy_tooltip( 'Summary'(001) ).
        lo_settings->set_hierarchy_size( 30 ).
        lo_settings->set_header( 'Summary'(001) ).

        CALL FUNCTION 'ICON_CREATE'
          EXPORTING
            name                  = icon_overview
            text                  = 'Summary'(001)
            info                  = 'Summary'(001)
            add_stdinf            = ' '
          IMPORTING
            result                = ld_icon
          EXCEPTIONS
            icon_not_found        = 1
            outputfield_too_short = 2.


        lo_settings->set_hierarchy_icon( ld_icon ).

        build_tree( ).

        lo_cols = mo_tree->get_columns( ).
        lt_cols = lo_cols->get( ).
        LOOP AT lt_cols INTO ls_col.
          CASE ls_col-columnname.
            WHEN 'XINTFID'.                                 "#EC NOTEXT
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


        mo_tree->display( ).

      CATCH cx_salv_error .
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
