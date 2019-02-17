"! <p class="shorttext synchronized" lang="en">Interface Parameters descriptions</p>
CLASS zcl_zintfmonitor013_read DEFINITION
  PUBLIC
  CREATE PRIVATE INHERITING FROM zcl_zintfmonitor_base_read.

  PUBLIC SECTION.

    "! <p class="shorttext synchronized" lang="en">Delete Details</p>
    "!
    "! @parameter is_details | <p class="shorttext synchronized" lang="en">List Details</p>
    CLASS-METHODS delete_details
      IMPORTING
        is_details TYPE zintfmonitor013
      RAISING
        zcx_intfmonitor .
    "! <p class="shorttext synchronized" lang="en">Delete Multiple</p>
    "!
    "! @parameter it_list | <p class="shorttext synchronized" lang="en">List Details</p>
    CLASS-METHODS delete_list
      IMPORTING
        !it_list TYPE ztt_zintfmonitor013
      RAISING
        zcx_intfmonitor .
    "! <p class="shorttext synchronized" lang="en">Find details by keys</p>
    "!
    "! @parameter id_intfid | <p class="shorttext synchronized" lang="en">Interface</p>
    "! @parameter id_spras  | <p class="shorttext synchronized" lang="en">Language</p>
    "! @parameter id_param  | <p class="shorttext synchronized" lang="en">Parameter</p>
    "! @parameter rs_result | <p class="shorttext synchronized" lang="en">Details</p>
    CLASS-METHODS get_details
      IMPORTING
        id_intfid        TYPE zintfmonitor013-intfid
        id_spras         TYPE zintfmonitor013-spras DEFAULT sy-langu
        id_param         TYPE zintfmonitor013-param
      RETURNING
        VALUE(rs_result) TYPE zintfmonitor013
      RAISING
        zcx_intfmonitor .
    "! <p class="shorttext synchronized" lang="en">Find Multiple details by keys</p>
    "!
    "! @parameter id_intfid | <p class="shorttext synchronized" lang="en">Interface</p>
    "! @parameter id_spras  | <p class="shorttext synchronized" lang="en">Language</p>
    "! @parameter id_param  | <p class="shorttext synchronized" lang="en">Parameter</p>
    "! @parameter et_list   | <p class="shorttext synchronized" lang="en">List Details</p>
    CLASS-METHODS get_list
      IMPORTING
        id_intfid TYPE zintfmonitor013-intfid OPTIONAL
        id_spras  TYPE zintfmonitor013-spras OPTIONAL
        id_param  TYPE zintfmonitor013-param OPTIONAL
      EXPORTING
        et_list   TYPE ztt_zintfmonitor013
      RAISING
        zcx_intfmonitor .
    "! <p class="shorttext synchronized" lang="en">Initializes Buffer Data</p>
    CLASS-METHODS init_buffer .
    "! <p class="shorttext synchronized" lang="en">Save Details</p>
    "!
    "! @parameter is_details | <p class="shorttext synchronized" lang="en">List Details</p>
    CLASS-METHODS save_details
      IMPORTING
        is_details TYPE zintfmonitor013
      RAISING
        zcx_intfmonitor .
    "! <p class="shorttext synchronized" lang="en">Save Multiple</p>
    "!
    "! @parameter it_list | <p class="shorttext synchronized" lang="en">List Details</p>
    CLASS-METHODS save_list
      IMPORTING
        it_list TYPE ztt_zintfmonitor013
      RAISING
        zcx_intfmonitor .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF mtyp_ranges,
        intfid TYPE RANGE OF zintfmonitor013-intfid,
        spras  TYPE RANGE OF zintfmonitor013-spras,
        param  TYPE RANGE OF zintfmonitor013-param,
      END   OF mtyp_ranges .

    "! <p class="shorttext synchronized" lang="en">Selection Ranges</p>
    CLASS-DATA gs_ranges TYPE mtyp_ranges .
    "! <p class="shorttext synchronized" lang="en">Data Buffer</p>
    CLASS-DATA gt_buffer TYPE ztt_zintfmonitor013 .

ENDCLASS.



CLASS zcl_zintfmonitor013_read IMPLEMENTATION.


  METHOD delete_details.
    DATA lt_list      TYPE ztt_zintfmonitor013 .
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
    DELETE zintfmonitor013  FROM TABLE it_list.

    IF NOT sy-subrc IS INITIAL.
      RAISE EXCEPTION TYPE zcx_intfmonitor.
    ENDIF.
  ENDMETHOD.


  METHOD get_details.

    DATA lt_list      TYPE ztt_zintfmonitor013 .
    DATA ls_list      LIKE LINE OF lt_list.
    DATA lo_exception TYPE REF TO zcx_intfmonitor.

    READ TABLE gt_buffer INTO rs_result
      WITH KEY intfid = id_intfid
               spras = id_spras
               param = id_param.

    IF NOT sy-subrc IS INITIAL.

      TRY.
          get_list( EXPORTING  id_intfid = id_intfid
                               id_spras = id_spras
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

    CLEAR gs_ranges.

    IF id_intfid IS SUPPLIED.
      _add_range( EXPORTING id_low   = id_intfid
                  CHANGING  ct_range = gs_ranges-intfid ).
    ENDIF.

    IF id_spras IS SUPPLIED.
      _add_range( EXPORTING id_low   = id_spras
                  CHANGING  ct_range = gs_ranges-spras ).
    ENDIF.

    IF id_param IS SUPPLIED.
      _add_range( EXPORTING id_low   = id_param
                  CHANGING  ct_range = gs_ranges-param ).
    ENDIF.

    SELECT * INTO TABLE et_list
      FROM zintfmonitor013
      WHERE intfid IN gs_ranges-intfid
        AND spras IN gs_ranges-spras
        AND param IN gs_ranges-param.

    INSERT LINES OF et_list INTO TABLE gt_buffer.
    SORT gt_buffer.
    DELETE ADJACENT DUPLICATES FROM gt_buffer.

    IF et_list IS INITIAL.
      RAISE EXCEPTION TYPE zcx_intfmonitor.
    ENDIF.
  ENDMETHOD.


  METHOD init_buffer.
    CLEAR gt_buffer[].
  ENDMETHOD.


  METHOD save_details.
    DATA lt_list TYPE ztt_zintfmonitor013 .
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
    MODIFY zintfmonitor013  FROM TABLE it_list.

    IF NOT sy-subrc IS INITIAL.
      RAISE EXCEPTION TYPE zcx_intfmonitor.
    ENDIF.
  ENDMETHOD.



ENDCLASS.
