"! <p class="shorttext synchronized" lang="en">SAP Interface Monitor - RFC</p>
CLASS zcl_intfmonitor_rfc DEFINITION
  PUBLIC
  CREATE PUBLIC
  GLOBAL FRIENDS zcf_intfmonitor.

  PUBLIC SECTION.


    INTERFACES zif_intfmonitor.

    ALIASES md_guid
      FOR zif_intfmonitor~md_guid.
    ALIASES ms_detail
      FOR zif_intfmonitor~ms_detail.
    ALIASES add_parameter
      FOR zif_intfmonitor~add_parameter.
    ALIASES get_detail
      FOR zif_intfmonitor~get_detail.
    ALIASES get_detail_x
      FOR zif_intfmonitor~get_detail_x.
    ALIASES get_parameter
      FOR zif_intfmonitor~get_parameter.
    ALIASES read
      FOR zif_intfmonitor~read.
    ALIASES store
      FOR zif_intfmonitor~store.

    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    METHODS constructor.

    "! <p class="shorttext synchronized" lang="en">Sets ending as ok/ko/warning..</p>
    "!
    "! @parameter id_procendtype | <p class="shorttext synchronized" lang="en">How it has ended?</p>
    METHODS set_procendtype
      IMPORTING id_procendtype TYPE zintfmonitor020-procendtype DEFAULT zcl_zintfmonitor020_read=>mc_procendtype-undefined.

    "! <p class="shorttext synchronized" lang="en">Sets lognumber</p>
    "!
    "! @parameter id_lognumber | <p class="shorttext synchronized" lang="en">Lognumber id</p>
    METHODS set_lognumber
      IMPORTING
        id_lognumber TYPE zintfmonitor020-lognumber.

  PROTECTED SECTION.


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

    "! <p class="shorttext synchronized" lang="en">SAP Interface Monitor - Data container</p>
    DATA mo_data_container TYPE REF TO zcl_intfmonitor_data_container .

    "! <p class="shorttext synchronized" lang="en">Maps data container execution parameters to parameter list</p>
    "!
    "! @parameter ct_parameters | Parameters list values
    METHODS _map_datacontainer_to_params
      CHANGING ct_parameters TYPE ztt_zintfmonitor021.

    "! <p class="shorttext synchronized" lang="en">Maps execution parameter list values to data container</p>
    "!
    "! @parameter it_parameters | Parameters list values
    METHODS _map_params_to_datacontainer
      IMPORTING it_parameters TYPE ztt_zintfmonitor021.

    "! <p class="shorttext synchronized" lang="en">Forces interface's parameters config</p>
    "! Ensures that all interface parameters have been already configured, if not creates
    "! a dummy configuration.
    "! @raising zcx_intfmonitor | Unable to set up dummy parameter configuration
    METHODS _ensure_param_config_done
      RAISING zcx_intfmonitor.

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

    " Retrieve Interface name
    TRY.
        ls_intf_x = zcl_zintfmonitor010_read=>get_detail_x( rs_detail_x-detail-intfid  ).
        rs_detail_x-xintfid = ls_intf_x-xintfid.
      CATCH zcx_intfmonitor.
        rs_detail_x-xintfid = |< { rs_detail_x-intfid } >|.
    ENDTRY.

    " Retrieve end status
    rs_detail_x-xprocendtype = zcl_intfmonitor_util=>get_domain_text( id_domname = 'ZZDPROCENDTYPE'
                                                                      id_value   = rs_detail_x-procendtype ).

    " Retrieve User Fullname
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

    CLEAR sy-subrc.

    ld_string = id_param.

    IF er_data IS SUPPLIED AND sy-subrc IS INITIAL.
      mo_data_container->get( EXPORTING id_id   = ld_string
                              IMPORTING er_data = er_data
                              EXCEPTIONS OTHERS = 999 ).
    ENDIF.

    IF ex_val IS SUPPLIED AND sy-subrc IS INITIAL.
      mo_data_container->get( EXPORTING id_id   = ld_string
                              IMPORTING ed_data = ex_val
                              EXCEPTIONS OTHERS = 999 ).
    ENDIF.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_intfmonitor.
    ENDIF.

  ENDMETHOD.


  METHOD zif_intfmonitor~initialize.

    me->md_guid   = id_guid.
    me->ms_detail = is_detail.

  ENDMETHOD.


  METHOD zif_intfmonitor~read.


    " Read execution data
    DATA(ls_head)   = zcl_zintfmonitor020_read=>get_details( id_guid = md_guid ).

    md_guid   = ls_head-guid.
    ms_detail = ls_head-detail.

    " Read Interface data values
    TRY.
        zcl_zintfmonitor021_read=>get_list( EXPORTING id_guid = md_guid
                                            IMPORTING et_list = DATA(lt_item) ).
        _map_params_to_datacontainer( it_parameters = lt_item ).
      CATCH  zcx_intfmonitor.
        "No parameters -> Do nothing
    ENDTRY.



  ENDMETHOD.


  METHOD zif_intfmonitor~store.

    DATA: ls_head TYPE zintfmonitor020,
          lt_item TYPE ztt_zintfmonitor021.

    "Check if any interface parameters is not yet configured
    _ensure_param_config_done( ).

    "Store execution data
    ls_head-guid   = md_guid.
    ls_head-detail = ms_detail.

    zcl_zintfmonitor020_read=>save_details( ls_head ).

    "Store Interface data values
    _map_datacontainer_to_params( CHANGING ct_parameters = lt_item ).
    zcl_zintfmonitor021_read=>save_list( it_list = lt_item ).

  ENDMETHOD.


  METHOD _ensure_param_config_done.
    DATA: ls_param_custo TYPE zintfmonitor012.

    LOOP AT mo_data_container->mt_data INTO DATA(ls_data).

      TRY.
          zcl_zintfmonitor012_read=>get_details( id_intfid = ms_detail-intfid
                                                 id_param  = CONV #( ls_data-id ) ).
        CATCH zcx_intfmonitor.
          " Parameter has not yet configured, lets configure it!
          ls_param_custo-intfid    = ms_detail-intfid.
          ls_param_custo-param     = ls_data-id.

          IF ls_data-type IS INITIAL.
            IF ls_data-data_ref IS NOT INITIAL.
              DATA(lo_abap_descr) = cl_abap_typedescr=>describe_by_data_ref( ls_data-data_ref ).
              IF lo_abap_descr->is_ddic_type( ) = abap_true.
                ls_data-type = lo_abap_descr->get_relative_name( ).
              ELSE.
                " If it was not possible to derive TYPE lets assing default type.
                ls_data-type = 'STRING'.
              ENDIF.
            ELSE.
              "Objects not supported
              RAISE EXCEPTION TYPE zcx_intfmonitor.
            ENDIF.
          ENDIF.

          ls_param_custo-paramtype = ls_data-type.
          ls_param_custo-datatype  = zcl_zintfmonitor012_read=>mc_datatype-undefined.

          TRY.
              zcl_zintfmonitor012_read=>save_details( EXPORTING is_details        = ls_param_custo
                                                                if_add_to_buffer  = abap_true ).
            CATCH zcx_intfmonitor INTO DATA(lo_exception).
              RAISE EXCEPTION lo_exception.
          ENDTRY.

      ENDTRY.
    ENDLOOP.
  ENDMETHOD.


  METHOD _map_datacontainer_to_params.
    DATA: ld_id           TYPE string,
          ld_item_counter TYPE zintfmonitor021-item.

    FIELD-SYMBOLS: <lt_data_table> TYPE ANY TABLE,
                   <ls_item>       LIKE LINE OF ct_parameters.

    DEFINE mac_add.
      ADD 1 TO ld_item_counter.
      INSERT INITIAL LINE INTO TABLE &2 ASSIGNING <ls_item>.
      <ls_item>-guid  = md_guid.
      <ls_item>-item  = ld_item_counter.
      <ls_item>-param = <ls_custo_intfparameter>-param.

      cl_abap_container_utilities=>fill_container_c( EXPORTING  im_value     = &1
                                                     IMPORTING  ex_container = <ls_item>-dataval
                                                     EXCEPTIONS OTHERS       = 0 ).
    END-OF-DEFINITION.



    "Read Customized Interface Parameters
    TRY.
        zcl_zintfmonitor012_read=>get_list( EXPORTING id_intfid = ms_detail-intfid
                                            IMPORTING et_list   = DATA(lt_custo_intfparameters) ).
      CATCH zcx_intfmonitor.
        "Do nothing
    ENDTRY.

    LOOP AT lt_custo_intfparameters ASSIGNING FIELD-SYMBOL(<ls_custo_intfparameter>).
      ld_id           = <ls_custo_intfparameter>-param.
      READ TABLE mo_data_container->mt_data WITH KEY id = ld_id ASSIGNING FIELD-SYMBOL(<ls_data>).

      DATA(lo_abap_descr) = cl_abap_typedescr=>describe_by_data_ref( <ls_data>-data_ref ).
      IF lo_abap_descr->type_kind = cl_abap_typedescr=>typekind_table.

        ASSIGN <ls_data>-data_ref->* TO <lt_data_table>.
        LOOP AT <lt_data_table> ASSIGNING FIELD-SYMBOL(<ls_data_row>).
          mac_add <ls_data_row> ct_parameters[].
        ENDLOOP.

      ELSE.

        ASSIGN <ls_data>-data_ref->* TO <ls_data_row>.
        mac_add <ls_data_row> ct_parameters[].

      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD _map_params_to_datacontainer.

    DATA: lt_item_param     LIKE it_parameters,
          lo_abap_descr     TYPE REF TO cl_abap_datadescr,
          lo_table_descr    TYPE REF TO cl_abap_tabledescr,
          ld_lines          TYPE i,
          ld_id             TYPE string,
          ld_ref_data       TYPE REF TO data,
          ld_ref_table_data TYPE REF TO data.

    FIELD-SYMBOLS: <ls_data_row>   TYPE any,
                   <lt_data_table> TYPE ANY TABLE.

    "Read Customized Interface Parameters
    TRY.
        zcl_zintfmonitor012_read=>get_list( EXPORTING id_intfid = ms_detail-intfid
                                            IMPORTING et_list   = DATA(lt_custo_intfparameters) ).
      CATCH zcx_intfmonitor.
        "Do nothing
    ENDTRY.

    LOOP AT lt_custo_intfparameters ASSIGNING FIELD-SYMBOL(<ls_custo_intfparameter>).
      ld_id           = <ls_custo_intfparameter>-param.

      lt_item_param[] = it_parameters[].
      DELETE lt_item_param WHERE param <> <ls_custo_intfparameter>-param.
      DESCRIBE TABLE lt_item_param  LINES ld_lines.

      DATA(lo_type_descr) = cl_abap_typedescr=>describe_by_name( <ls_custo_intfparameter>-paramtype ).


      IF ld_lines = 1.
        " Create dynamically data to store it in DataContainer
        lo_abap_descr ?= lo_type_descr.
        CREATE DATA ld_ref_data TYPE HANDLE lo_abap_descr.
        ASSIGN ld_ref_data->* TO <ls_data_row>.

        READ TABLE lt_item_param INDEX 1 ASSIGNING FIELD-SYMBOL(<ls_item>).
        cl_abap_container_utilities=>read_container_c( EXPORTING  im_container           = <ls_item>-dataval
                                                       IMPORTING  ex_value               = <ls_data_row>
                                                       EXCEPTIONS illegal_parameter_type = 1 ).

        mo_data_container->add( id_id   = ld_id
                                id_data = <ls_data_row> ).

      ELSEIF ld_lines > 0.
        " If more than one entry for the same parameter -> create table
        lo_table_descr ?= lo_type_descr.
        CREATE DATA ld_ref_table_data TYPE HANDLE lo_table_descr.
        ASSIGN ld_ref_table_data->* TO <lt_data_table>.

        LOOP AT lt_item_param ASSIGNING <ls_item>.
          INSERT INITIAL LINE INTO TABLE <lt_data_table> ASSIGNING <ls_data_row>.
          cl_abap_container_utilities=>read_container_c( EXPORTING  im_container           = <ls_item>-dataval
                                                         IMPORTING  ex_value               = <ls_data_row>
                                                         EXCEPTIONS illegal_parameter_type = 1 ).

        ENDLOOP.
        mo_data_container->add( id_id   = ld_id
                               id_data = <lt_data_table> ).

      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD set_procendtype.

    ASSERT id_procendtype = zcl_zintfmonitor020_read=>mc_procendtype-error
        OR id_procendtype = zcl_zintfmonitor020_read=>mc_procendtype-success
        OR id_procendtype = zcl_zintfmonitor020_read=>mc_procendtype-warning
        OR id_procendtype = zcl_zintfmonitor020_read=>mc_procendtype-undefined.

    ms_detail-procendtype = id_procendtype.

  ENDMETHOD.

  METHOD set_lognumber.

    ASSERT ms_detail-lognumber IS INITIAL.
    ms_detail-lognumber = id_lognumber.

  ENDMETHOD.

ENDCLASS.
