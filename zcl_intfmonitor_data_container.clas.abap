"! <p class="shorttext synchronized" lang="en">SAP Interface Monitor - Data container</p>
CLASS zcl_intfmonitor_data_container DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
*"* public components of class ZCL_INTFMONITOR_DATA_CONTAINER
*"* do not include other source files here!!!

    TYPES:
      "! <p class="shorttext synchronized" lang="en">Data</p>
      BEGIN OF mtyp_s_data,
        id       TYPE string,
        data_ref TYPE REF TO data,
        obj_ref  TYPE REF TO object,
        type     TYPE typename,
      END   OF mtyp_s_data .
    TYPES:
      "! <p class="shorttext synchronized" lang="en">DataContainer</p>
      mtyp_t_data TYPE STANDARD TABLE OF mtyp_s_data WITH KEY id .

    "! <p class="shorttext synchronized" lang="en">DataContainer</p>
    DATA mt_data TYPE mtyp_t_data READ-ONLY .

    "! <p class="shorttext synchronized" lang="en">Adds data to DataContainer</p>
    "!
    "! @exception error | <p class="shorttext synchronized" lang="en">Error</p>
    METHODS add
      IMPORTING
        !id_id   TYPE string
        !id_data TYPE any
      EXCEPTIONS
        error .
    "! <p class="shorttext synchronized" lang="en">Gets data from DataContainer</p>
    "!
    "! @parameter id_id     | <p class="shorttext synchronized" lang="en">Parameter Id</p>
    "! @parameter ed_data   | <p class="shorttext synchronized" lang="en">Parameter valu</p>
    "! @parameter ed_type   | <p class="shorttext synchronized" lang="en">Name of Dictionary Type</p>
    "! @parameter er_data   | <p class="shorttext synchronized" lang="en">Reference to parameter value</p>
    "! @exception not_found | <p class="shorttext synchronized" lang="en">Not Found</p>
    "! @exception error     | <p class="shorttext synchronized" lang="en">Error</p>
    METHODS get
      IMPORTING
        !id_id   TYPE string
      EXPORTING
        !ed_data TYPE any
        !ed_type TYPE typename
        !er_data TYPE REF TO data
      EXCEPTIONS
        not_found
        error .
  PROTECTED SECTION.
*"* protected components of class ZCL_INTFMONITOR_DATA_CONTAINER
*"* do not include other source files here!!!
  PRIVATE SECTION.
*"* private components of class ZCL_INTFMONITOR_DATA_CONTAINER
*"* do not include other source files here!!!
ENDCLASS.



CLASS zcl_intfmonitor_data_container IMPLEMENTATION.


  METHOD add.
    DATA: lo_typedescr TYPE REF TO cl_abap_typedescr,
          lo_datadescr TYPE REF TO cl_abap_datadescr,
          ls_data      LIKE LINE OF mt_data.

    FIELD-SYMBOLS: <ld_any>  TYPE any.

* Is there any duplicate record?
    DELETE TABLE mt_data WITH TABLE KEY id = id_id.
    ls_data-id = id_id.

    lo_typedescr = cl_abap_typedescr=>describe_by_data( id_data ).
    IF lo_typedescr->type_kind = cl_abap_typedescr=>typekind_oref.
      TRY.
          ls_data-obj_ref = id_data.
        CATCH cx_root.
*       Error?
          RAISE error.
      ENDTRY.
    ELSE.
      TRY.
          ls_data-type = lo_typedescr->get_relative_name( ).
          lo_datadescr ?= lo_typedescr.
          CREATE DATA ls_data-data_ref TYPE HANDLE lo_datadescr.
          ASSIGN ls_data-data_ref->* TO <ld_any>.
          <ld_any> = id_data.
        CATCH cx_root.
*       Error?
          RAISE error.
      ENDTRY.

    ENDIF.

    INSERT ls_data INTO TABLE mt_data.

  ENDMETHOD.


  METHOD get.
    FIELD-SYMBOLS: <ld_data> TYPE any,
                   <ls_data> LIKE LINE OF mt_data.

    READ TABLE mt_data WITH KEY id = id_id ASSIGNING <ls_data>.
    IF sy-subrc <> 0.
      RAISE not_found.
    ELSE.
      TRY.
          IF <ls_data>-obj_ref IS NOT INITIAL.
*       objref
            IF ed_data IS REQUESTED.
              ed_data ?= <ls_data>-obj_ref.
            ENDIF.
            IF er_data IS REQUESTED.
              GET REFERENCE OF <ls_data>-obj_ref INTO er_data.
            ENDIF.
          ELSE.
*       data
            ASSIGN <ls_data>-data_ref->* TO <ld_data>.
            IF er_data IS REQUESTED.
              er_data = <ls_data>-data_ref.
            ENDIF.
            IF ed_data IS REQUESTED.
              ed_data = <ld_data>.
            ENDIF.
          ENDIF.
        CATCH cx_root.                                   "#EC CATCH_ALL
*     Error?
          RAISE error.
      ENDTRY.
      IF ed_type IS REQUESTED.
        ed_type = <ls_data>-type.
      ENDIF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
