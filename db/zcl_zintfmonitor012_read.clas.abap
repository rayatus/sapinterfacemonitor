"! <p class="shorttext synchronized" lang="en">Interface Parameters</p>
CLASS zcl_zintfmonitor012_read DEFINITION
  PUBLIC
  CREATE PRIVATE INHERITING FROM zcl_zintfmonitor_base_read.

  PUBLIC SECTION.

    "! <p class="shorttext synchronized" lang="en">Class constructor</p>
    CLASS-METHODS class_constructor.
    "! <p class="shorttext synchronized" lang="en">Delete Details</p>
    "!
    "! @parameter is_details | <p class="shorttext synchronized" lang="en">List Details</p>
    CLASS-METHODS delete_details
      IMPORTING
        is_details TYPE zintfmonitor012
      RAISING
        zcx_intfmonitor .
    "! <p class="shorttext synchronized" lang="en">Delete Multiple</p>
    "!
    "! @parameter it_list | <p class="shorttext synchronized" lang="en">List Details</p>
    CLASS-METHODS delete_list
      IMPORTING
        it_list TYPE ztt_zintfmonitor012
      RAISING
        zcx_intfmonitor .
    "! <p class="shorttext synchronized" lang="en">Find details by keys</p>
    "!
    "! @parameter id_intfid | <p class="shorttext synchronized" lang="en">Interface</p>
    "! @parameter id_param  | <p class="shorttext synchronized" lang="en">Parameter</p>
    "! @parameter rs_result | <p class="shorttext synchronized" lang="en">Details</p>
    CLASS-METHODS get_details
      IMPORTING
        id_intfid        TYPE zintfmonitor012-intfid
        id_param         TYPE zintfmonitor012-param
      RETURNING
        VALUE(rs_result) TYPE zintfmonitor012
      RAISING
        zcx_intfmonitor .
    "! <p class="shorttext synchronized" lang="en">Find Multiple details by keys</p>
    "!
    "! @parameter id_intfid | <p class="shorttext synchronized" lang="en">Interface</p>
    "! @parameter id_param  | <p class="shorttext synchronized" lang="en">Parameter</p>
    "! @parameter et_list   | <p class="shorttext synchronized" lang="en">List Details</p>
    CLASS-METHODS get_list
      IMPORTING
        id_intfid TYPE zintfmonitor012-intfid OPTIONAL
        id_param  TYPE zintfmonitor012-param OPTIONAL
      EXPORTING
        et_list   TYPE ztt_zintfmonitor012
      RAISING
        zcx_intfmonitor .
    "! <p class="shorttext synchronized" lang="en">Initializes Buffer Data</p>
    CLASS-METHODS init_buffer .
    "! <p class="shorttext synchronized" lang="en">Save Details</p>
    "!
    "! @parameter is_details | <p class="shorttext synchronized" lang="en">List Details</p>
    CLASS-METHODS save_details
      IMPORTING
        is_details TYPE zintfmonitor012
      RAISING
        zcx_intfmonitor .
    "! <p class="shorttext synchronized" lang="en">Save Multiple</p>
    "!
    "! @parameter it_list | <p class="shorttext synchronized" lang="en">List Details</p>
    CLASS-METHODS save_list
      IMPORTING
        it_list TYPE ztt_zintfmonitor012
      RAISING
        zcx_intfmonitor .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF mtyp_ranges,
        intfid TYPE RANGE OF zintfmonitor012-intfid,
        param  TYPE RANGE OF zintfmonitor012-param,
      END   OF mtyp_ranges .

    "! <p class="shorttext synchronized" lang="en">Selection Ranges</p>
    CLASS-DATA gt_ranges TYPE mtyp_ranges .
    "! <p class="shorttext synchronized" lang="en">Data Buffer</p>
    CLASS-DATA gt_buffer TYPE ztt_zintfmonitor012 .
    "! <p class="shorttext synchronized" lang="en">Reads DB Table</p>
    CLASS-METHODS do_select.

ENDCLASS.



CLASS zcl_zintfmonitor012_read IMPLEMENTATION.


  METHOD delete_details.
    DATA lt_list      TYPE ztt_zintfmonitor012 .
    DATA ls_list      LIKE LINE OF lt_list.
    DATA lo_exception TYPE REF TO  zcx_intfmonitor.

    ASSERT is_details IS NOT INITIAL.
    TRY .
        MOVE-CORRESPONDING is_details TO ls_list.
        INSERT ls_list INTO TABLE lt_list[].
        delete_list( lt_list[] ).
      CATCH zcx_intfmonitor INTO lo_exception.
        RAISE EXCEPTION lo_exception.
    ENDTRY.
  ENDMETHOD.


  METHOD delete_list.

    ASSERT it_list[] IS NOT INITIAL.
    DELETE zintfmonitor012  FROM TABLE it_list.

    IF NOT sy-subrc IS INITIAL.
      RAISE EXCEPTION TYPE zcx_intfmonitor.
    ENDIF.
  ENDMETHOD.


  METHOD get_details.

    DATA lt_list      TYPE ztt_zintfmonitor012 .
    DATA ls_list      LIKE LINE OF lt_list.
    DATA lo_exception TYPE REF TO zcx_intfmonitor.

    READ TABLE gt_buffer INTO rs_result
      WITH KEY intfid = id_intfid
               param = id_param.

    IF NOT sy-subrc IS INITIAL.

      TRY.
          get_list( EXPORTING  id_intfid = id_intfid
                               id_param = id_param
                    IMPORTING  et_list = lt_list ).

          READ TABLE lt_list INDEX 1 INTO ls_list.
          MOVE-CORRESPONDING ls_list TO rs_result.

        CATCH zcx_intfmonitor INTO lo_exception.
          RAISE EXCEPTION lo_exception.
      ENDTRY.

    ENDIF.
  ENDMETHOD.


  METHOD get_list.

    CLEAR gt_ranges.

    IF id_intfid IS SUPPLIED.
      _add_range( EXPORTING id_low   = id_intfid
                  CHANGING  ct_range = gt_ranges-intfid ).
    ENDIF.

    IF id_param IS SUPPLIED.
      _add_range( EXPORTING id_low   = id_param
                  CHANGING  ct_range = gt_ranges-param ).
    ENDIF.

    LOOP AT gt_buffer INTO DATA(ls_buffer) WHERE intfid IN gt_ranges-intfid
                                             AND param IN gt_ranges-param.
      INSERT INITIAL LINE INTO TABLE et_list ASSIGNING FIELD-SYMBOL(<ls_list>).
      MOVE-CORRESPONDING ls_buffer TO <ls_list>.
    ENDLOOP.

    IF et_list IS INITIAL.
      RAISE EXCEPTION TYPE zcx_intfmonitor.
    ENDIF.
  ENDMETHOD.


  METHOD init_buffer.
    CLEAR gt_buffer[].
    do_select( ).
  ENDMETHOD.


  METHOD save_details.
    DATA lt_list TYPE ztt_zintfmonitor012 .
    DATA ls_list LIKE LINE OF lt_list.
    DATA lo_exception TYPE REF TO zcx_intfmonitor.

    ASSERT is_details IS NOT INITIAL.
    TRY .
        MOVE-CORRESPONDING is_details TO ls_list.
        INSERT ls_list INTO TABLE lt_list[].
        save_list( lt_list[] ).
      CATCH zcx_intfmonitor INTO lo_exception.
        RAISE EXCEPTION lo_exception.
    ENDTRY.
  ENDMETHOD.


  METHOD save_list.

    ASSERT it_list[] IS NOT INITIAL.
    MODIFY zintfmonitor012  FROM TABLE it_list.

    IF NOT sy-subrc IS INITIAL.
      RAISE EXCEPTION TYPE zcx_intfmonitor.
    ENDIF.
  ENDMETHOD.


  METHOD class_constructor.
    init_buffer( ).
  ENDMETHOD.

  METHOD do_select.
    SELECT * INTO TABLE gt_buffer FROM zintfmonitor012 ORDER BY PRIMARY KEY.
  ENDMETHOD.

ENDCLASS.
