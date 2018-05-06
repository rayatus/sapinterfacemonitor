*----------------------------------------------------------------------*
*       CLASS ZCL_INTFMONITOR_UTIL DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
"! <p class="shorttext synchronized" lang="en">SAP Interface Monitor - Utilities</p>
CLASS zcl_intfmonitor_util DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.
*"* public components of class ZCL_INTFMONITOR_UTIL
*"* do not include other source files here!!!

    TYPES:
      BEGIN OF mtyp_s_intf_param,
        datatype  TYPE zzeintfdatatype,
        param	    TYPE zzeintfdatapar,
        xparam    TYPE zzexintfdatapar,
        xdatatype TYPE ddtext,
        paramtype TYPE zzeintfdatapartype,
      END   OF mtyp_s_intf_param .
    TYPES:
      mtyp_t_intf_param TYPE STANDARD TABLE OF mtyp_s_intf_param WITH NON-UNIQUE KEY datatype param.

    "! <p class="shorttext synchronized" lang="en">Gets Domain Value text</p>
    "!
    "! @parameter id_domname | <p class="shorttext synchronized" lang="en">Domain name</p>
    "! @parameter id_value   | <p class="shorttext synchronized" lang="en">Values for Domains: Single Value / Upper Limit</p>
    "! @parameter rd_text    | <p class="shorttext synchronized" lang="en">Short Text for Fixed Values</p>
    CLASS-METHODS get_domain_text
      IMPORTING
        !id_domname    TYPE dd07v-domname
        !id_value      TYPE any
      RETURNING
        VALUE(rd_text) TYPE dd07v-ddtext .
    "! <p class="shorttext synchronized" lang="en">Gets Interface parameter definition</p>
    "!
    "! @parameter io_interface | <p class="shorttext synchronized" lang="en">SAP Interface Monitor</p>
    CLASS-METHODS get_interface_param_definition
      IMPORTING
        !io_interface  TYPE REF TO zif_intfmonitor
      EXPORTING
        !et_definition TYPE mtyp_t_intf_param .
  PROTECTED SECTION.
*"* protected components of class ZCL_INTFMONITOR_UTIL
*"* do not include other source files here!!!
  PRIVATE SECTION.
*"* private components of class ZCL_INTFMONITOR_UTIL
*"* do not include other source files here!!!
ENDCLASS.



CLASS zcl_intfmonitor_util IMPLEMENTATION.


  METHOD get_domain_text.
    DATA: ld_value TYPE dd07v-domvalue_l.

    ld_value = id_value.
    CALL FUNCTION 'SXMS_GET_DOMAIN_TEXT'
      EXPORTING
        domname       = id_domname
        value         = ld_value
      IMPORTING
        text          = rd_text
      EXCEPTIONS
        illegal_input = 1.

  ENDMETHOD.


  METHOD get_interface_param_definition.
    DATA: lt_list    TYPE ztt_zintfmonitor012,
          ls_details TYPE zintfmonitor013.

    FIELD-SYMBOLS: <ls_list>       LIKE LINE OF lt_list,
                   <ls_definition> LIKE LINE OF et_definition.

    ASSERT io_interface IS BOUND.

    CLEAR et_definition[].

    TRY.
        zcl_zintfmonitor012_read=>get_list( EXPORTING id_intfid = io_interface->ms_detail-intfid
                                            IMPORTING et_list   = lt_list ).

      CATCH cx_db2_not_found .
*     Do nothing
    ENDTRY.

    LOOP AT lt_list ASSIGNING <ls_list>.
      INSERT INITIAL LINE INTO TABLE et_definition ASSIGNING <ls_definition>.
      <ls_definition>-param     = <ls_list>-param.
      <ls_definition>-datatype  = <ls_list>-datatype.
      <ls_definition>-paramtype = <ls_list>-paramtype.
      <ls_definition>-xdatatype = get_domain_text( id_domname = 'ZZDINTFDATATYPE'
                                                   id_value   = <ls_definition>-datatype ).

      TRY.
          ls_details = zcl_zintfmonitor013_read=>get_details( id_intfid = io_interface->ms_detail-intfid
                                                              id_spras  = sy-langu
                                                              id_param  = <ls_list>-param ).
          <ls_definition>-xparam = ls_details-xparam.
        CATCH cx_db2_not_found .
*     Do nothing
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
