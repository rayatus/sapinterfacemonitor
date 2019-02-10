"! <p class="shorttext synchronized" lang="en">Interface description</p>
CLASS zcl_zintfmonitor011_read DEFINITION
  PUBLIC
  CREATE PRIVATE INHERITING FROM zcl_zintfmonitor_base_read..

  PUBLIC SECTION.

    "! <p class="shorttext synchronized" lang="en">Delete Details</p>
    "!
    "! @parameter is_details | <p class="shorttext synchronized" lang="en">List Details</p>
    CLASS-METHODS delete_details
      IMPORTING
        is_details TYPE zintfmonitor011
      RAISING
        zcx_intfmonitor .
    "! <p class="shorttext synchronized" lang="en">Delete Multiple</p>
    "!
    "! @parameter it_list | <p class="shorttext synchronized" lang="en">List Details</p>
    CLASS-METHODS delete_list
      IMPORTING
        it_list TYPE ztt_zintfmonitor011
      RAISING
        zcx_intfmonitor .
    "! <p class="shorttext synchronized" lang="en">Find details by keys</p>
    "!
    "! @parameter id_intfid | <p class="shorttext synchronized" lang="en">Interface</p>
    "! @parameter id_spras  | <p class="shorttext synchronized" lang="en">Language</p>
    "! @parameter rs_result | <p class="shorttext synchronized" lang="en">Details</p>
    CLASS-METHODS get_details
      IMPORTING
        id_intfid        TYPE zintfmonitor011-intfid
        id_spras         TYPE zintfmonitor011-spras DEFAULT sy-langu
      RETURNING
        VALUE(rs_result) TYPE zintfmonitor011
      RAISING
        zcx_intfmonitor .
    "! <p class="shorttext synchronized" lang="en">Find Multiple details by keys</p>
    "!
    "! @parameter id_intfid | <p class="shorttext synchronized" lang="en">Interface</p>
    "! @parameter id_spras  | <p class="shorttext synchronized" lang="en">Language</p>
    "! @parameter et_list   | <p class="shorttext synchronized" lang="en">List Details</p>
    CLASS-METHODS get_list
      IMPORTING
        id_intfid TYPE zintfmonitor011-intfid OPTIONAL
        id_spras  TYPE zintfmonitor011-spras OPTIONAL
      EXPORTING
        et_list   TYPE ztt_zintfmonitor011
      RAISING
        zcx_intfmonitor .
    "! <p class="shorttext synchronized" lang="en">Initializes Buffer Data</p>
    CLASS-METHODS init_buffer .
    "! <p class="shorttext synchronized" lang="en">Save Details</p>
    "!
    "! @parameter is_details | <p class="shorttext synchronized" lang="en">List Details</p>
    CLASS-METHODS save_details
      IMPORTING
        is_details TYPE zintfmonitor011
      RAISING
        zcx_intfmonitor .
    "! <p class="shorttext synchronized" lang="en">Save Multiple</p>
    "!
    "! @parameter it_list | <p class="shorttext synchronized" lang="en">List Details</p>
    CLASS-METHODS save_list
      IMPORTING
        !it_list TYPE ztt_zintfmonitor011
      RAISING
        zcx_intfmonitor .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF mtyp_ranges,
        intfid TYPE RANGE OF zintfmonitor011-intfid,
        spras  TYPE RANGE OF zintfmonitor011-spras,
      END   OF mtyp_ranges .

    "! <p class="shorttext synchronized" lang="en">Selection Ranges</p>
    CLASS-DATA ms_ranges TYPE mtyp_ranges .
    "! <p class="shorttext synchronized" lang="en">Data Buffer</p>
    CLASS-DATA mt_buffer TYPE ztt_zintfmonitor011 .

ENDCLASS.



CLASS zcl_zintfmonitor011_read IMPLEMENTATION.


  METHOD delete_details.
    DATA lt_list      TYPE ztt_zintfmonitor011 .
    DATA ls_list      LIKE LINE OF lt_list.
    DATA lo_exception TYPE REF TO  zcx_intfmonitor.

    CHECK is_details IS NOT INITIAL.
    TRY .
        MOVE-CORRESPONDING is_details TO ls_list.
        INSERT ls_list INTO TABLE lt_list[].
        delete_list( lt_list[] ).
      CATCH zcx_intfmonitor INTO lo_exception.
        RAISE EXCEPTION lo_exception.
    ENDTRY.
  ENDMETHOD.


  METHOD delete_list.

    CHECK it_list[] IS NOT INITIAL.
    DELETE zintfmonitor011  FROM TABLE it_list.

    IF NOT sy-subrc IS INITIAL.
      RAISE EXCEPTION TYPE zcx_intfmonitor.
    ENDIF.
  ENDMETHOD.


  METHOD get_details.

    DATA lt_list      TYPE ztt_zintfmonitor011 .
    DATA ls_list      LIKE LINE OF lt_list.

    READ TABLE mt_buffer INTO rs_result
      WITH KEY intfid = id_intfid
  						 spras = id_spras.

    IF NOT sy-subrc IS INITIAL.

      TRY.
          get_list( EXPORTING  id_intfid = id_intfid
  														 id_spras = id_spras
                    IMPORTING  et_list = lt_list ).

          READ TABLE lt_list INDEX 1 INTO ls_list.
          MOVE-CORRESPONDING ls_list TO rs_result.

        CATCH zcx_intfmonitor INTO DATA(lo_exception).
          RAISE EXCEPTION lo_exception.
      ENDTRY.

    ENDIF.
  ENDMETHOD.


  METHOD get_list.

    CLEAR ms_ranges.

    IF id_intfid IS SUPPLIED.
      _add_range( EXPORTING id_low   = id_intfid
                  CHANGING  ct_range = ms_ranges-intfid ).
    ENDIF.

    IF id_spras IS SUPPLIED.
      _add_range( EXPORTING id_low   = id_spras
                  CHANGING  ct_range = ms_ranges-spras ).
    ENDIF.

    SELECT * INTO TABLE et_list
      FROM zintfmonitor011
      WHERE intfid IN ms_ranges-intfid
        AND spras IN ms_ranges-spras.

    INSERT LINES OF et_list INTO TABLE mt_buffer.
    SORT mt_buffer.
    DELETE ADJACENT DUPLICATES FROM mt_buffer.

    IF et_list IS INITIAL.
      RAISE EXCEPTION TYPE zcx_intfmonitor.
    ENDIF.
  ENDMETHOD.


  METHOD init_buffer.
    CLEAR mt_buffer[].
  ENDMETHOD.


  METHOD save_details.
    DATA lt_list TYPE ztt_zintfmonitor011 .
    DATA ls_list LIKE LINE OF lt_list.

    CHECK is_details IS NOT INITIAL.
    TRY .
        MOVE-CORRESPONDING is_details TO ls_list.
        INSERT ls_list INTO TABLE lt_list[].
        save_list( lt_list[] ).
      CATCH zcx_intfmonitor INTO DATA(lo_exception).
        RAISE EXCEPTION lo_exception.
    ENDTRY.
  ENDMETHOD.


  METHOD save_list.

    CHECK it_list[] IS NOT INITIAL.
    MODIFY zintfmonitor011  FROM TABLE it_list.

    IF NOT sy-subrc IS INITIAL.
      RAISE EXCEPTION TYPE zcx_intfmonitor.
    ENDIF.
  ENDMETHOD.


ENDCLASS.
