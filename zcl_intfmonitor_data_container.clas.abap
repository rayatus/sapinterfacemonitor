"! <p class="shorttext synchronized" lang="en">SAP Interface Monitor - Data container</p>
CLASS zcl_intfmonitor_data_container DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

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
        id_id   TYPE string
        id_data TYPE any
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
        id_id   TYPE string
      EXPORTING
        ed_data TYPE any
        ed_type TYPE typename
        er_data TYPE REF TO data
      EXCEPTIONS
        not_found
        error .
  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_intfmonitor_data_container IMPLEMENTATION.



  METHOD add.

    DATA: ls_data LIKE LINE OF mt_data.

    READ TABLE mt_data WITH KEY id = id_id ASSIGNING FIELD-SYMBOL(<ls_data>).
    IF sy-subrc IS NOT INITIAL.
      ls_data-id = id_id.
      INSERT ls_data INTO TABLE mt_data.
      READ TABLE mt_data WITH KEY id = id_id ASSIGNING <ls_data>.
    ENDIF.


    DATA(lo_typedescr) = cl_abap_typedescr=>describe_by_data( p_data = id_data ).
    IF lo_typedescr IS NOT BOUND.
      "Unable to identify data
      RAISE error.
    ENDIF.

    IF lo_typedescr->type_kind = cl_abap_typedescr=>typekind_oref.

      TRY.
          <ls_data>-obj_ref = id_data.
        CATCH cx_root.                                   "#EC CATCH_ALL
          "Is not an object?! so it couldn't be typekind_oref!!!
          RAISE error.
      ENDTRY.

    ELSE.

      TRY.
          <ls_data>-type = lo_typedescr->get_relative_name( ).
          DATA(lo_datadescr) = CAST cl_abap_datadescr( lo_typedescr ).
          CREATE DATA <ls_data>-data_ref TYPE HANDLE lo_datadescr.
          ASSIGN <ls_data>-data_ref->* TO FIELD-SYMBOL(<ld_data>).
          <ld_data> = id_data.
        CATCH cx_root.                                   "#EC CATCH_ALL
          RAISE error.
      ENDTRY.
    ENDIF.

  ENDMETHOD.

  METHOD get.

    READ TABLE mt_data WITH KEY id = id_id ASSIGNING FIELD-SYMBOL(<ls_data>).
    IF sy-subrc IS NOT INITIAL.
      RAISE error.
    ELSE.

      IF <ls_data>-obj_ref IS NOT INITIAL.
        ed_data ?= <ls_data>-obj_ref.
      ELSEIF <ls_data>-data_ref IS NOT INITIAL.
        ASSIGN <ls_data>-data_ref->* TO FIELD-SYMBOL(<ld_data>).
        ed_data = <ld_data>.
        er_data = <ls_data>-data_ref.
      ENDIF.

      ed_type = <ls_data>-type.

    ENDIF.

  ENDMETHOD.

ENDCLASS.


