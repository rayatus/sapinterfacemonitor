"! <p class="shorttext synchronized" lang="en">SAP Interface Monitor - RFC</p>
CLASS zcl_intfmonitor_rfc DEFINITION
  PUBLIC
  CREATE PUBLIC

  GLOBAL FRIENDS zcf_intfmonitor .

  PUBLIC SECTION.
*"* public components of class ZCL_INTFMONITOR_RFC
*"* do not include other source files here!!!

    INTERFACES zif_intfmonitor .

    ALIASES md_guid
      FOR zif_intfmonitor~md_guid .
    ALIASES ms_detail
      FOR zif_intfmonitor~ms_detail .
    ALIASES add_parameter
      FOR zif_intfmonitor~add_parameter .
    ALIASES get_detail
      FOR zif_intfmonitor~get_detail .
    ALIASES get_detail_x
      FOR zif_intfmonitor~get_detail_x .
    ALIASES get_parameter
      FOR zif_intfmonitor~get_parameter .
    ALIASES read
      FOR zif_intfmonitor~read .
    ALIASES store
      FOR zif_intfmonitor~store .

    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    METHODS constructor .
  PROTECTED SECTION.
*"* protected components of class ZCL_INTFMONITOR_RFC
*"* do not include other source files here!!!

    TYPES:
      "! <p class="shorttext synchronized" lang="en">Interface Parameter</p>
      BEGIN OF mtyp_s_intfparameter,
        param     TYPE zzeintfdatapar,
        paramtype TYPE zzeintfdatapartype,
        data      TYPE REF TO data,
      END   OF mtyp_s_intfparameter .
    "! <p class="shorttext synchronized" lang="en">Interface Parameters</p>
    TYPES mtyp_t_intfparameters TYPE SORTED TABLE OF mtyp_s_intfparameter WITH UNIQUE KEY param.
  PRIVATE SECTION.
*"* private components of class ZCL_INTFMONITOR_RFC
*"* do not include other source files here!!!

    "! <p class="shorttext synchronized" lang="en">SAP Interface Monitor - Data container</p>
    DATA mo_data_container TYPE REF TO zcl_intfmonitor_data_container .
ENDCLASS.



CLASS zcl_intfmonitor_rfc IMPLEMENTATION.


  METHOD constructor.

    CREATE OBJECT mo_data_container.

  ENDMETHOD.


  METHOD zif_intfmonitor~add_parameter.

    DATA: ld_string TYPE string.

    ld_string = id_param.
    mo_data_container->add( EXPORTING  id_id   = ld_string
                                       id_data = ix_val
                            EXCEPTIONS OTHERS  = 999 ).

  ENDMETHOD.


  METHOD zif_intfmonitor~get_detail.

    rs_detail = ms_detail.

  ENDMETHOD.


  METHOD zif_intfmonitor~get_detail_x.
    DATA: ls_address TYPE bapiaddr3,
          lt_return  TYPE bapirettab,
          ls_intf_x  TYPE zeintfmonitor010_detail_x.

    rs_detail_x-detail = get_detail( ).

* Retrieve Interface name
    TRY.
        ls_intf_x = zcl_zintfmonitor010_read=>get_detail_x( rs_detail_x-detail-intfid  ).
        rs_detail_x-xintfid = ls_intf_x-xintfid.
      CATCH cx_db2_not_found .
*   Do nothing
    ENDTRY.


* Retrieve User Fullname
    CALL FUNCTION 'BAPI_USER_GET_DETAIL'
      EXPORTING
        username = rs_detail_x-detail-procby
      IMPORTING
        address  = ls_address
      TABLES
        return   = lt_return.
    rs_detail_x-xprocby = ls_address-fullname.


  ENDMETHOD.


  METHOD zif_intfmonitor~get_parameter.
    DATA: ld_string TYPE string.

    ld_string = id_param.

    IF er_data IS REQUESTED.
      mo_data_container->get( EXPORTING id_id   = ld_string
                              IMPORTING er_data = er_data
                              EXCEPTIONS OTHERS = 999 ).
    ENDIF.

    IF ex_val IS REQUESTED.
      mo_data_container->get( EXPORTING id_id   = ld_string
                              IMPORTING ed_data = ex_val
                              EXCEPTIONS OTHERS = 999 ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_intfmonitor~initialize.

    me->md_guid   = id_guid.
    me->ms_detail = is_detail.

  ENDMETHOD.


  METHOD zif_intfmonitor~read.
    DATA: ls_head           TYPE zintfmonitor020,
          lt_item           TYPE ztt_zintfmonitor021,
          lt_item_param     TYPE ztt_zintfmonitor021,
          ld_previous_param TYPE zzeintfdatapar,
          lt_intfparameters TYPE ztt_zintfmonitor012,
          lo_type_descr     TYPE REF TO cl_abap_typedescr,
          lo_abap_descr     TYPE REF TO cl_abap_datadescr,
          lo_elem_descr     TYPE REF TO cl_abap_elemdescr,
          lo_struct_descr   TYPE REF TO cl_abap_structdescr,
          lo_table_descr    TYPE REF TO cl_abap_tabledescr,
          ld_lines          TYPE i,
          ld_id             TYPE string,
          ld_ref_data       TYPE REF TO data,
          ld_ref_table_data TYPE REF TO data.

    FIELD-SYMBOLS: <ls_item>          LIKE LINE OF lt_item,
                   <ls_intfparameter> LIKE LINE OF lt_intfparameters,
                   <ls_data_row>      TYPE any,
                   <ls_data_table>    TYPE table.

    TRY.
*     Read execution data
        ls_head   = zcl_zintfmonitor020_read=>get_detail( id_guid = md_guid ).

        md_guid   = ls_head-guid.
        ms_detail = ls_head-detail.

*     Read Interface data values
        zcl_zintfmonitor021_read=>get_list( EXPORTING id_guid = md_guid
                                            IMPORTING et_list = lt_item ).

*     Read Customized Interface Parameters
        zcl_zintfmonitor012_read=>get_list( EXPORTING id_intfid = ms_detail-intfid
                                            IMPORTING et_list   = lt_intfparameters ).

      CATCH cx_db2_not_found .
*     Do nothing
    ENDTRY.

*--------------------------------------------------------------------*
* Map DB Structures to DataContainer
*--------------------------------------------------------------------*
    LOOP AT lt_intfparameters ASSIGNING <ls_intfparameter>.
      ld_id           = <ls_intfparameter>-param.

      lt_item_param[] = lt_item[].
      DELETE lt_item_param WHERE param <> <ls_intfparameter>-param.
      DESCRIBE TABLE lt_item_param  LINES ld_lines.

      lo_type_descr = cl_abap_typedescr=>describe_by_name( <ls_intfparameter>-paramtype ).


      IF ld_lines = 1.


*     Create dynamically data to store it in DataContainer
        lo_abap_descr ?= lo_type_descr.
        CREATE DATA ld_ref_data TYPE HANDLE lo_abap_descr.
        ASSIGN ld_ref_data->* TO <ls_data_row>.

        READ TABLE lt_item_param INDEX 1 ASSIGNING <ls_item>.
        cl_abap_container_utilities=>read_container_c( EXPORTING  im_container           = <ls_item>-dataval
                                                       IMPORTING  ex_value               = <ls_data_row>
                                                       EXCEPTIONS illegal_parameter_type = 1 ).

        mo_data_container->add( id_id   = ld_id
                                id_data = <ls_data_row> ).

      ELSEIF ld_lines > 0.
*     If more than one entry for the same parameter -> create table
        lo_table_descr ?= lo_type_descr.
*      lo_table_descr = cl_abap_tabledescr=>create( p_line_type = lo_abap_descr ).
        CREATE DATA ld_ref_table_data TYPE HANDLE lo_table_descr.
        ASSIGN ld_ref_table_data->* TO <ls_data_table>.

        LOOP AT lt_item_param ASSIGNING <ls_item>.
          INSERT INITIAL LINE INTO TABLE <ls_data_table> ASSIGNING <ls_data_row>.
          cl_abap_container_utilities=>read_container_c( EXPORTING  im_container           = <ls_item>-dataval
                                                         IMPORTING  ex_value               = <ls_data_row>
                                                         EXCEPTIONS illegal_parameter_type = 1 ).

        ENDLOOP.
        mo_data_container->add( id_id   = ld_id
                               id_data = <ls_data_table> ).

      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_intfmonitor~store.

    DATA: ls_head           TYPE zintfmonitor020,
          lt_item           TYPE ztt_zintfmonitor021,
          ld_previous_param TYPE zzeintfdatapar,
          lt_intfparameters TYPE ztt_zintfmonitor012,
          lo_abap_descr     TYPE REF TO cl_abap_typedescr,
          ld_id             TYPE string,
          ld_item_counter   TYPE zintfmonitor021-item.

    FIELD-SYMBOLS: <ls_item>          LIKE LINE OF lt_item,
                   <ls_intfparameter> LIKE LINE OF lt_intfparameters,
                   <ls_data_row>      TYPE any,
                   <lt_data_table>    TYPE ANY TABLE,
                   <ls_data>          LIKE LINE OF mo_data_container->mt_data.

*=======================================================================
* Adds parameter 1 to table parameter 2.
*=======================================================================
    DEFINE mac_add.
      ADD 1 TO ld_item_counter.
      INSERT INITIAL LINE INTO TABLE &2 ASSIGNING <ls_item>.
      <ls_item>-guid  = md_guid.
      <ls_item>-item  = ld_item_counter.
      <ls_item>-param = <ls_intfparameter>-param.

      cl_abap_container_utilities=>fill_container_c( EXPORTING  im_value               = &1
                                                     IMPORTING  ex_container           = <ls_item>-dataval
                                                     EXCEPTIONS illegal_parameter_type = 1 ).
    END-OF-DEFINITION.

    TRY .
*     Read Customized Interface Parameters
        zcl_zintfmonitor012_read=>get_list( EXPORTING id_intfid = ms_detail-intfid
                                            IMPORTING et_list   = lt_intfparameters ).
      CATCH cx_db2_not_found.
*   Do nothing
    ENDTRY.


*--------------------------------------------------------------------*
* Map DataContainer to DB Structures
*--------------------------------------------------------------------*
    LOOP AT lt_intfparameters ASSIGNING <ls_intfparameter>.
      ld_id           = <ls_intfparameter>-param.
      READ TABLE mo_data_container->mt_data WITH KEY id = ld_id ASSIGNING <ls_data>.

      lo_abap_descr = cl_abap_typedescr=>describe_by_data_ref( <ls_data>-data_ref ).
      IF lo_abap_descr->type_kind = cl_abap_typedescr=>typekind_table.

        ASSIGN <ls_data>-data_ref->* TO <lt_data_table>.
        LOOP AT <lt_data_table> ASSIGNING <ls_data_row>.
          mac_add <ls_data_row> lt_item[].
        ENDLOOP.

      ELSE.

        ASSIGN <ls_data>-data_ref->* TO <ls_data_row>.
        mac_add <ls_data_row> lt_item[].

      ENDIF.
    ENDLOOP.

    TRY.
*     Store execution data
        ls_head-guid   = md_guid.
        ls_head-detail = ms_detail.
        zcl_zintfmonitor020_read=>save_detail( ls_head ).

*     Store Interface data values
        zcl_zintfmonitor021_read=>save_list( it_list = lt_item ).

      CATCH cx_db2_not_found .
*     Do nothing
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
