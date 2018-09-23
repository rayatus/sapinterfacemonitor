"! <p class="shorttext synchronized" lang="en">DB Framework for: ZINTFMONITOR020</p>
CLASS zcl_zintfmonitor020_read DEFINITION
  PUBLIC
  CREATE PRIVATE .

  PUBLIC SECTION.

    TYPES:

      BEGIN OF mtyp_all_fields,
        intfid      TYPE RANGE OF zeintfmonitor_detail-intfid,
        procdate    TYPE RANGE OF zeintfmonitor_detail-procdate,
        proctime    TYPE RANGE OF zeintfmonitor_detail-proctime,
        procby      TYPE RANGE OF zeintfmonitor_detail-procby,
        procendtype TYPE RANGE OF zeintfmonitor_detail-procendtype,
        lognumber   TYPE RANGE OF zeintfmonitor_detail-lognumber,
      END OF mtyp_all_fields.

    "! <p class="shorttext synchronized" lang="en">Delete Details</p>
    "!
    "! @parameter is_details | <p class="shorttext synchronized" lang="en">List Details</p>
    CLASS-METHODS delete_details
      IMPORTING
        !is_details TYPE zintfmonitor020
      RAISING
        zcx_intfmonitor .
    "! <p class="shorttext synchronized" lang="en">Delete Multiple</p>
    "!
    "! @parameter it_list | <p class="shorttext synchronized" lang="en">List Details</p>
    CLASS-METHODS delete_list
      IMPORTING
        !it_list TYPE ztt_zintfmonitor020
      RAISING
        zcx_intfmonitor .
    "! <p class="shorttext synchronized" lang="en">Find details by keys</p>
    "!
    "! @parameter id_guid   | <p class="shorttext synchronized" lang="en">Process Id</p>
    "! @parameter rs_result | <p class="shorttext synchronized" lang="en">Details</p>
    CLASS-METHODS get_details
      IMPORTING
        !id_guid         TYPE zintfmonitor020-guid
      RETURNING
        VALUE(rs_result) TYPE zintfmonitor020
      RAISING
        zcx_intfmonitor .
    "! <p class="shorttext synchronized" lang="en">Find Multiple details by keys</p>
    "!
    "! @parameter id_guid | <p class="shorttext synchronized" lang="en">Process Id</p>
    "! @parameter et_list | <p class="shorttext synchronized" lang="en">List Details</p>
    CLASS-METHODS get_list
      IMPORTING
        !id_guid TYPE zintfmonitor020-guid OPTIONAL
      EXPORTING
        !et_list TYPE ztt_zintfmonitor020
      RAISING
        zcx_intfmonitor .
    CLASS-METHODS get_list_by
      IMPORTING
        !is_filter_by TYPE zcl_zintfmonitor020_read=>mtyp_all_fields
      EXPORTING
        !et_list      TYPE ztt_zintfmonitor020.

    "! <p class="shorttext synchronized" lang="en">Initializes Buffer Data</p>
    CLASS-METHODS init_buffer .
    "! <p class="shorttext synchronized" lang="en">Save Details</p>
    "!
    "! @parameter is_details | <p class="shorttext synchronized" lang="en">List Details</p>
    CLASS-METHODS save_details
      IMPORTING
        !is_details TYPE zintfmonitor020
      RAISING
        zcx_intfmonitor .
    "! <p class="shorttext synchronized" lang="en">Save Multiple</p>
    "!
    "! @parameter it_list | <p class="shorttext synchronized" lang="en">List Details</p>
    CLASS-METHODS save_list
      IMPORTING
        !it_list TYPE ztt_zintfmonitor020
      RAISING
        zcx_intfmonitor .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF mtyp_ranges,
        guid TYPE RANGE OF zintfmonitor020-guid,
      END   OF mtyp_ranges .

    "! <p class="shorttext synchronized" lang="en">Selection Ranges</p>
    CLASS-DATA ms_ranges TYPE mtyp_ranges .
    "! <p class="shorttext synchronized" lang="en">Data Buffer</p>
    CLASS-DATA mt_buffer TYPE ztt_zintfmonitor020 .

    "! <p class="shorttext synchronized" lang="en">Add to Range</p>
    CLASS-METHODS _add_range
      IMPORTING
        !id_low   TYPE any
      CHANGING
        !ct_range TYPE ANY TABLE .
ENDCLASS.



CLASS zcl_zintfmonitor020_read IMPLEMENTATION.


  METHOD delete_details.
    DATA lt_list      TYPE ztt_zintfmonitor020 .
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
    DELETE zintfmonitor020  FROM TABLE it_list.

    IF NOT sy-subrc IS INITIAL.
      RAISE EXCEPTION TYPE zcx_intfmonitor.
    ENDIF.
  ENDMETHOD.


  METHOD get_details.

    DATA lt_list      TYPE ztt_zintfmonitor020 .
    DATA ls_list      LIKE LINE OF lt_list.
    DATA lo_exception TYPE REF TO zcx_intfmonitor.

    READ TABLE mt_buffer INTO rs_result
      WITH KEY guid = id_guid.

    IF NOT sy-subrc IS INITIAL.

      TRY.
          get_list( EXPORTING  id_guid = id_guid
                    IMPORTING  et_list = lt_list ).

          READ TABLE lt_list INDEX 1 INTO ls_list.
          MOVE-CORRESPONDING ls_list TO rs_result.

        CATCH zcx_intfmonitor INTO lo_exception.
          RAISE EXCEPTION lo_exception.
      ENDTRY.

    ENDIF.
  ENDMETHOD.


  METHOD get_list.

    CLEAR ms_ranges.

    IF id_guid IS SUPPLIED.
      _add_range( EXPORTING id_low   = id_guid
                  CHANGING  ct_range = ms_ranges-guid ).
    ENDIF.

    SELECT * INTO TABLE et_list
      FROM zintfmonitor020
      WHERE guid IN ms_ranges-guid.

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
    DATA lt_list TYPE ztt_zintfmonitor020 .
    DATA ls_list LIKE LINE OF lt_list.
    DATA lo_exception TYPE REF TO zcx_intfmonitor.

    CHECK is_details IS NOT INITIAL.
    TRY .
        MOVE-CORRESPONDING is_details TO ls_list.
        INSERT ls_list INTO TABLE lt_list[].
        save_list( lt_list[] ).
      CATCH zcx_intfmonitor INTO lo_exception.
        RAISE EXCEPTION lo_exception.
    ENDTRY.
  ENDMETHOD.


  METHOD save_list.

    CHECK it_list[] IS NOT INITIAL.
    MODIFY zintfmonitor020  FROM TABLE it_list.

    IF NOT sy-subrc IS INITIAL.
      RAISE EXCEPTION TYPE zcx_intfmonitor.
    ENDIF.
  ENDMETHOD.


  METHOD _add_range.

    FIELD-SYMBOLS: <ls_row>    TYPE any,
                   <ld_sign>   TYPE any,
                   <ld_option> TYPE any,
                   <ld_low>    TYPE any.

    INSERT INITIAL LINE INTO TABLE ct_range ASSIGNING <ls_row>.

    ASSIGN COMPONENT 'LOW' OF STRUCTURE <ls_row> TO <ld_low>.
    <ld_low> = id_low.

    ASSIGN COMPONENT 'SIGN' OF STRUCTURE <ls_row> TO <ld_sign>.
    <ld_sign> = 'I'.

    ASSIGN COMPONENT 'OPTION' OF STRUCTURE <ls_row> TO <ld_option>.
    <ld_option> = 'EQ'.
  ENDMETHOD.


  METHOD get_list_by.


    SELECT * INTO TABLE et_list
      FROM zintfmonitor020
      WHERE intfid      IN is_filter_by-intfid
        AND procby      IN is_filter_by-procby
        AND procdate    IN is_filter_by-procdate
        AND procendtype IN is_filter_by-procendtype
        AND proctime    IN is_filter_by-proctime
        AND lognumber   IN is_filter_by-lognumber.

    INSERT LINES OF et_list INTO TABLE mt_buffer.
    SORT mt_buffer.
    DELETE ADJACENT DUPLICATES FROM mt_buffer.

    IF et_list IS INITIAL.
      RAISE EXCEPTION TYPE zcx_intfmonitor.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
